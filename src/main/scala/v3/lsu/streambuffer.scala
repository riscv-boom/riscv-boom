//******************************************************************************
// See LICENSE.Berkeley for license details.
//------------------------------------------------------------------------------
// Stream Buffer Prefetcher for BOOM v3
//------------------------------------------------------------------------------

package boom.v3.lsu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.rocket._

import boom.v3.common._

/**
 * Configuration parameters for the Stream Buffer Prefetcher
 *
 * @param nEntries Number of stream buffer entries to track concurrent streams
 * @param nPrefetchAhead Number of cache lines to prefetch ahead per stream
 * @param detectStride Enable stride detection (false = unit stride only)
 * @param trainThreshold Number of sequential accesses before confident prefetching
 */
case class StreamBufferParams(
  nEntries: Int = 4,
  nPrefetchAhead: Int = 2,
  detectStride: Boolean = false,
  trainThreshold: Int = 2
)

/**
 * Stream Buffer Entry - tracks a single memory access stream
 */
class StreamBufferEntry(implicit p: Parameters) extends BoomBundle {
  val valid      = Bool()
  val baseAddr   = UInt(coreMaxAddrBits.W)  // Current stream address (block-aligned)
  val stride     = SInt(16.W)               // Stride in bytes (positive or negative)
  val confidence = UInt(3.W)                // Confidence counter (saturating)
  val lru        = UInt(8.W)                // LRU counter for replacement
}

/**
 * Stream Buffer Prefetcher
 *
 * Detects sequential/strided memory access patterns and prefetches ahead.
 * Each stream buffer entry tracks a separate memory stream.
 *
 * Algorithm:
 * 1. On cache miss, check if address matches any stream's predicted next address
 * 2. If match: increment confidence, advance stream, issue prefetches
 * 3. If no match: check if address continues any existing stream
 * 4. If still no match: allocate new entry using LRU replacement
 * 5. Prefetch requests are issued when confidence exceeds threshold
 */
class StreamBufferPrefetcher(params: StreamBufferParams)(implicit edge: TLEdgeOut, p: Parameters)
    extends DataPrefetcher {

  val nEntries = params.nEntries
  val nPrefetchAhead = params.nPrefetchAhead
  val detectStride = params.detectStride
  val trainThreshold = params.trainThreshold

  // Stream buffer entries
  val entries = RegInit(VecInit(Seq.fill(nEntries)(0.U.asTypeOf(new StreamBufferEntry))))

  // Global LRU counter
  val lruCounter = RegInit(0.U(8.W))

  // Prefetch request queue
  val prefetchQueue = Module(new Queue(new BoomDCacheReq, nPrefetchAhead * nEntries))

  // Block-align the miss address
  val missAddrAligned = (io.req_addr >> lgCacheBlockBytes.U) << lgCacheBlockBytes.U

  // Calculate predicted next addresses for each entry
  val predictedAddrs = Wire(Vec(nEntries, UInt(coreMaxAddrBits.W)))
  for (i <- 0 until nEntries) {
    predictedAddrs(i) := (entries(i).baseAddr.asSInt + entries(i).stride).asUInt
  }

  // Check for stream hits (address matches predicted next address)
  val streamHits = Wire(Vec(nEntries, Bool()))
  for (i <- 0 until nEntries) {
    val predictedAligned = (predictedAddrs(i) >> lgCacheBlockBytes.U) << lgCacheBlockBytes.U
    streamHits(i) := entries(i).valid && (missAddrAligned === predictedAligned)
  }
  val hasStreamHit = streamHits.asUInt.orR
  val streamHitIdx = PriorityEncoder(streamHits)

  // Check for potential new streams (address is adjacent to existing entry)
  val adjacentHits = Wire(Vec(nEntries, Bool()))
  val adjacentStrides = Wire(Vec(nEntries, SInt(16.W)))
  for (i <- 0 until nEntries) {
    val diff = (missAddrAligned.asSInt - entries(i).baseAddr.asSInt)
    val isAdjacent = entries(i).valid &&
                     (diff.abs <= (cacheBlockBytes * 4).S) &&
                     (diff =/= 0.S)
    adjacentHits(i) := isAdjacent
    adjacentStrides(i) := diff
  }
  val hasAdjacentHit = adjacentHits.asUInt.orR && !hasStreamHit
  val adjacentHitIdx = PriorityEncoder(adjacentHits)

  // Find LRU entry for replacement
  val lruValues = VecInit(entries.map(_.lru))
  val invalidEntries = VecInit(entries.map(!_.valid))
  val hasInvalid = invalidEntries.asUInt.orR
  val invalidIdx = PriorityEncoder(invalidEntries)

  // Find minimum LRU among valid entries using Scala fold at elaboration time
  val minLruIdx = (1 until nEntries).foldLeft(0.U(log2Ceil(nEntries).W)) { (bestIdx, i) =>
    Mux(lruValues(i) < lruValues(bestIdx), i.U, bestIdx)
  }
  val replaceIdx = Mux(hasInvalid, invalidIdx, minLruIdx)

  // State machine for prefetch generation
  val s_idle :: s_prefetch :: Nil = Enum(2)
  val state = RegInit(s_idle)

  val prefetchEntry = Reg(UInt(log2Ceil(nEntries).W))
  val prefetchCount = Reg(UInt(log2Ceil(nPrefetchAhead + 1).W))
  val prefetchAddr = Reg(UInt(coreMaxAddrBits.W))
  val prefetchCmd = Reg(UInt(M_SZ.W))

  // Check if prefetch address is cacheable
  val cacheableCheck = edge.manager.supportsAcquireBSafe(prefetchAddr, lgCacheBlockBytes.U)

  // Process cache miss - update stream buffers
  when (io.req_val) {
    lruCounter := lruCounter + 1.U

    when (hasStreamHit) {
      // Stream hit - advance the stream and increase confidence
      val idx = streamHitIdx
      val entry = entries(idx)

      entries(idx).baseAddr := missAddrAligned
      entries(idx).confidence := Mux(entry.confidence === 7.U, 7.U, entry.confidence + 1.U)
      entries(idx).lru := lruCounter

      // Start prefetching if confident enough
      when (entry.confidence >= trainThreshold.U && state === s_idle) {
        state := s_prefetch
        prefetchEntry := idx
        prefetchCount := nPrefetchAhead.U
        prefetchAddr := (missAddrAligned.asSInt + entry.stride).asUInt
        prefetchCmd := Mux(ClientStates.hasWritePermission(io.req_coh.state), M_PFW, M_PFR)
      }
    } .elsewhen (hasAdjacentHit && detectStride.B) {
      // Adjacent hit with stride detection - update stride
      val idx = adjacentHitIdx
      val newStride = adjacentStrides(idx)

      when (entries(idx).stride === newStride) {
        // Same stride - increase confidence
        entries(idx).baseAddr := missAddrAligned
        entries(idx).confidence := Mux(entries(idx).confidence === 7.U, 7.U, entries(idx).confidence + 1.U)
      } .otherwise {
        // Different stride - reset confidence, update stride
        entries(idx).baseAddr := missAddrAligned
        entries(idx).stride := newStride
        entries(idx).confidence := 1.U
      }
      entries(idx).lru := lruCounter
    } .otherwise {
      // No hit - allocate new entry
      val idx = replaceIdx
      entries(idx).valid := true.B
      entries(idx).baseAddr := missAddrAligned
      entries(idx).stride := cacheBlockBytes.S  // Default to unit stride (next cache line)
      entries(idx).confidence := 1.U
      entries(idx).lru := lruCounter
    }
  }

  // Prefetch generation state machine
  switch (state) {
    is (s_idle) {
      // Wait for stream hit to trigger prefetching
    }
    is (s_prefetch) {
      when (prefetchQueue.io.enq.fire) {
        prefetchCount := prefetchCount - 1.U
        prefetchAddr := (prefetchAddr.asSInt + entries(prefetchEntry).stride).asUInt

        when (prefetchCount === 1.U) {
          state := s_idle
        }
      }
    }
  }

  // Enqueue prefetch requests
  prefetchQueue.io.enq.valid := (state === s_prefetch) && cacheableCheck
  prefetchQueue.io.enq.bits := DontCare
  prefetchQueue.io.enq.bits.addr := prefetchAddr
  prefetchQueue.io.enq.bits.uop := NullMicroOp
  prefetchQueue.io.enq.bits.uop.mem_cmd := prefetchCmd
  prefetchQueue.io.enq.bits.data := DontCare
  prefetchQueue.io.enq.bits.is_hella := false.B

  // Output to MSHR
  io.prefetch.valid := prefetchQueue.io.deq.valid && io.mshr_avail
  io.prefetch.bits := prefetchQueue.io.deq.bits
  prefetchQueue.io.deq.ready := io.prefetch.ready && io.mshr_avail
}
