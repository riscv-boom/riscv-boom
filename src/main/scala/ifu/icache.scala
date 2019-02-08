//******************************************************************************
// Copyright (c) 2017 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------
// Author: Christopher Celio
//------------------------------------------------------------------------------

package boom.ifu

import chisel3._
import chisel3.util._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.experimental.dontTouch
import chisel3.experimental.chiselName

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.subsystem.RocketTilesKey
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._
import freechips.rocketchip.rocket.{HasL1ICacheParameters, ICacheParams, ICacheErrors, ICacheReq}

import boom.common._

class ICache(
    val icacheParams: ICacheParams,
    val hartId: Int,
    val enableBlackBox: Boolean = false)(implicit p: Parameters)
  extends LazyModule
{
  lazy val module: ICacheBaseModule = if (!enableBlackBox)
  {
    new ICacheModule(this)
  }
  else
  {
    new ICacheModuleBlackBox(this)
  }
  val masterNode = TLClientNode(Seq(TLClientPortParameters(Seq(TLClientParameters(
    sourceId = IdRange(0, 1 + icacheParams.prefetch.toInt), // 0=refill, 1=hint
    name = s"Core ${hartId} ICache")))))

  val size = icacheParams.nSets * icacheParams.nWays * icacheParams.blockBytes
  val device = new SimpleDevice("itim", Seq("sifive,itim0"))
  private val wordBytes = icacheParams.fetchBytes
  val slaveNode =
    TLManagerNode(icacheParams.itimAddr.toSeq.map { itimAddr => TLManagerPortParameters(
      Seq(TLManagerParameters(
        address         = Seq(AddressSet(itimAddr, size-1)),
        resources       = device.reg("mem"),
        regionType      = RegionType.UNCACHEABLE,
        executable      = true,
        supportsPutFull = TransferSizes(1, wordBytes),
        supportsPutPartial = TransferSizes(1, wordBytes),
        supportsGet     = TransferSizes(1, wordBytes),
        fifoId          = Some(0))), // requests handled in FIFO order
      beatBytes = wordBytes,
      minLatency = 1)})
}

class ICacheResp(val outer: ICache) extends Bundle
{
  val data = UInt((outer.icacheParams.fetchBytes*8).W)
  val replay = Bool()
  val ae = Bool()
}

class ICachePerfEvents extends Bundle
{
  val acquire = Bool()
}

class ICacheBundle(val outer: ICache) extends CoreBundle()(outer.p)
{
  val hartid = Input(UInt(hartIdLen.W))
  val req = Flipped(Decoupled(new ICacheReq))
  val s1_vaddr = Input(UInt(vaddrBits.W)) // vaddr delayed one cycle w.r.t. req
  val s1_paddr = Input(UInt(paddrBits.W)) // delayed one cycle w.r.t. req
  val s2_vaddr = Input(UInt(vaddrBits.W)) // delayed two cycles w.r.t. req
  val s1_kill = Input(Bool()) // delayed one cycle w.r.t. req
  val s2_kill = Input(Bool()) // delayed two cycles; prevents I$ miss emission
  val s2_prefetch = Input(Bool()) // should I$ prefetch next line on a miss?

  val resp = Valid(new ICacheResp(outer))
  val invalidate = Input(Bool())

  val errors = new ICacheErrors
  val perf = Output(new ICachePerfEvents())
}

// get a tile-specific property without breaking deduplication
object GetPropertyByHartId
{
  def apply[T <: Data](tiles: Seq[RocketTileParams], f: RocketTileParams => Option[T], hartId: UInt): T = {
    PriorityMux(tiles.collect { case t if f(t).isDefined => (t.hartId.U === hartId) -> f(t).get })
  }
}

abstract class ICacheBaseModule(outer: ICache) extends LazyModuleImp(outer)
  with HasL1ICacheBankedParameters
{
  override val cacheParams = outer.icacheParams // Use the local parameters
  val io = IO(new ICacheBundle(outer))
}

@chiselName
class ICacheModule(outer: ICache) extends ICacheBaseModule(outer)
{
  val (tl_out, edge_out) = outer.masterNode.out(0)
  // Option.unzip does not exist :-(
  val (tl_in, edge_in) = outer.slaveNode.in.headOption.unzip

  val tECC = cacheParams.tagCode
  val dECC = cacheParams.dataCode

  require(isPow2(nSets) && isPow2(nWays))

  // How many bits do we intend to fetch at most every cycle?
  val wordBits = outer.icacheParams.fetchBytes*8
  // How many banks does the ICache use?
  val nBanks = if (cacheParams.fetchBytes <= 8) 1 else 2
  // Each of these cases require some special-case handling.
  require (tl_out.d.bits.data.getWidth == wordBits || (2*tl_out.d.bits.data.getWidth == wordBits && nBanks == 2))
  // If TL refill is half the wordBits size and we have two banks, then the
  // refill writes to only one bank per cycle (instead of across two banks every
  // cycle).
  val refillsToOneBank = (2*tl_out.d.bits.data.getWidth == wordBits)

  val scratchpadOn = RegInit(false.B)
  val scratchpadMax = tl_in.map(tl => Reg(UInt(log2Ceil(nSets * (nWays - 1)).W)))
  def lineInScratchpad(line: UInt) = scratchpadMax.map(scratchpadOn && line <= _).getOrElse(false.B)
  val scratchpadBase = outer.icacheParams.itimAddr.map { dummy =>
    GetPropertyByHartId(p(RocketTilesKey), _.icache.flatMap(_.itimAddr.map(_.U)), io.hartid)
  }
  def addrMaybeInScratchpad(addr: UInt) = scratchpadBase.map(base => addr >= base &&
                                                             addr < base + outer.size.U).getOrElse(false.B)
  def addrInScratchpad(addr: UInt) = addrMaybeInScratchpad(addr) &&
                                     lineInScratchpad(addr(untagBits+log2Ceil(nWays)-1, blockOffBits))
  def scratchpadWay(addr: UInt) = addr.extract(untagBits+log2Ceil(nWays)-1, untagBits)
  def scratchpadWayValid(way: UInt) = way < nWays.U - 1.U
  def scratchpadLine(addr: UInt) = addr(untagBits+log2Ceil(nWays)-1, blockOffBits)
  val s0_slaveValid = tl_in.map(_.a.fire()).getOrElse(false.B)
  val s1_slaveValid = RegNext(s0_slaveValid, false.B)
  val s2_slaveValid = RegNext(s1_slaveValid, false.B)
  val s3_slaveValid = RegNext(false.B)

  val s1_valid = RegInit(false.B)
  val s1_tag_hit = Wire(Vec(nWays, Bool()))
  val s1_hit = s1_tag_hit.reduce(_||_) || Mux(s1_slaveValid, true.B, addrMaybeInScratchpad(io.s1_paddr))
  dontTouch(s1_hit)
  val s2_valid = RegNext(s1_valid && !io.s1_kill, false.B)
  val s2_hit = RegNext(s1_hit)

  val invalidated = Reg(Bool())
  val refill_valid = RegInit(false.B)
  val send_hint = RegInit(false.B)
  val refill_fire = tl_out.a.fire() && !send_hint
  val hint_outstanding = RegInit(false.B)
  val s2_miss = s2_valid && !s2_hit && !io.s2_kill && !RegNext(refill_valid)
  val refill_paddr = RegEnable(io.s1_paddr, s1_valid && !(refill_valid || s2_miss))
  val refill_vaddr = RegEnable(io.s1_vaddr, s1_valid && !(refill_valid || s2_miss))
  val refill_tag = refill_paddr(tagBits+untagBits-1,untagBits)
  val refill_idx = refill_vaddr(untagBits-1,blockOffBits)
  val refill_one_beat = tl_out.d.fire() && edge_out.hasData(tl_out.d.bits)

  io.req.ready := !(refill_one_beat || s0_slaveValid || s3_slaveValid)
  val s0_valid = io.req.fire()
  val s0_vaddr = io.req.bits.addr
  s1_valid := s0_valid

  val (_, _, d_done, refill_cnt) = edge_out.count(tl_out.d)
  val refill_done = refill_one_beat && d_done
  tl_out.d.ready := !s3_slaveValid
  require (edge_out.manager.minLatency > 0)

  val repl_way = if (isDM)
  {
    0.U
  }
  else
  {
    // pick a way that is not used by the scratchpad
    val v0 = LFSR16(refill_fire)(log2Ceil(nWays)-1,0)
    var v = v0
    for (i <- log2Ceil(nWays) - 1 to 0 by -1)
    {
      val mask = nWays - (BigInt(1) << (i + 1))
      v = v | (lineInScratchpad(Cat(v0 | mask.U, refill_idx)) << i)
    }
    assert(!lineInScratchpad(Cat(v, refill_idx)))
    v
  }

  val tag_array = SyncReadMem(nSets, Vec(nWays, UInt(tECC.width(1 + tagBits).W)))
  val tag_rdata = tag_array.read(s0_vaddr(untagBits-1,blockOffBits), !refill_done && s0_valid)
  val accruedRefillError = Reg(Bool())
  when (refill_done)
  {
    // For AccessAckData, denied => corrupt
    val enc_tag = tECC.encode(Cat(tl_out.d.bits.corrupt, refill_tag))
    tag_array.write(refill_idx, VecInit(Seq.fill(nWays)(enc_tag)), Seq.tabulate(nWays)(repl_way === _.U))

    ccover(tl_out.d.bits.corrupt, "D_CORRUPT", "I$ D-channel corrupt")
  }

  val vb_array = RegInit(0.U((nSets*nWays).W))
  when (refill_one_beat)
  {
    // clear bit when refill starts so hit-under-miss doesn't fetch bad data
    vb_array := vb_array.bitSet(Cat(repl_way, refill_idx), refill_done && !invalidated)
  }

  val invalidate = WireInit(io.invalidate)
  when (invalidate)
  {
    vb_array := 0.U
    invalidated := true.B
  }

  val s1_tag_disparity = Wire(Vec(nWays, Bool()))
  val s1_tl_error = Wire(Vec(nWays, Bool()))
  val s1_dout = Wire(Vec(nWays, UInt(dECC.width(wordBits).W)))
  val s1_bankId = Wire(Bool())

  val s0_slaveAddr = tl_in.map(_.a.bits.address).getOrElse(0.U)
  val s1s3_slaveAddr = Reg(UInt(log2Ceil(outer.size).W))
  val s1s3_slaveData = Reg(UInt(wordBits.W))

  for (i <- 0 until nWays)
  {
    val s1_idx = io.s1_vaddr(untagBits-1,blockOffBits)
    val s1_tag = io.s1_paddr(tagBits+untagBits-1,untagBits)
    val scratchpadHit = scratchpadWayValid(i.U) &&
      Mux(s1_slaveValid,
        lineInScratchpad(scratchpadLine(s1s3_slaveAddr)) && scratchpadWay(s1s3_slaveAddr) === i.U,
        addrInScratchpad(io.s1_paddr) && scratchpadWay(io.s1_paddr) === i.U)
    val s1_vb = vb_array(Cat(i.U, s1_idx)) && !s1_slaveValid
    val enc_tag = tECC.decode(tag_rdata(i))
    val (tl_error, tag) = Split(enc_tag.uncorrected, tagBits)
    val tagMatch = s1_vb && tag === s1_tag
    s1_tag_disparity(i) := s1_vb && enc_tag.error
    s1_tl_error(i) := tagMatch && tl_error.toBool
    s1_tag_hit(i) := tagMatch || scratchpadHit
  }
  assert(!(s1_valid || s1_slaveValid) ||
         PopCount(s1_tag_hit zip s1_tag_disparity map { case (h, d) => h && !d }) <= 1.U)

  assert (!(s1_slaveValid), "[icache] We do not support the icache slave.")


  // declare arrays outside conditional so they show up named in Verilog.
  val ramDepth =
    if (2*tl_out.d.bits.data.getWidth == wordBits) (nSets * refillCycles/2)
    else (nSets * refillCycles)
  val dataArraysB0 = Seq.fill(nWays) { SyncReadMem(ramDepth, UInt(dECC.width(wordBits/nBanks).W)) }
  val dataArraysB1 = Seq.fill(nWays) { SyncReadMem(ramDepth, UInt(dECC.width(wordBits/nBanks).W)) }

  if (cacheParams.fetchBytes <= 8)
  {
    // Use unbanked icache for narrow accesses.
    val dataArrays = Seq.fill(nWays) { SyncReadMem(nSets * refillCycles, UInt(dECC.width(wordBits).W)) }
    s1_bankId := 0.U
    for ((dataArray, i) <- dataArrays zipWithIndex)
    {
      def row(addr: UInt) = addr(untagBits-1, blockOffBits-log2Ceil(refillCycles))
      val s0_ren = s0_valid || s0_slaveValid

      val way = Mux(s3_slaveValid, scratchpadWay(s1s3_slaveAddr), repl_way)
      val wen = ((refill_one_beat && !invalidated) || s3_slaveValid) && way === i.U

      val mem_idx = Mux(refill_one_beat, (refill_idx << log2Ceil(refillCycles)) | refill_cnt,
                    Mux(s3_slaveValid, row(s1s3_slaveAddr),
                    Mux(s0_slaveValid, row(s0_slaveAddr),
                    row(s0_vaddr))))
      when (wen)
      {
        val data = Mux(s3_slaveValid, s1s3_slaveData, tl_out.d.bits.data)
        dataArray.write(mem_idx, dECC.encode(data))
      }
      s1_dout(i) := dataArray.read(mem_idx, !wen && s0_ren)
    }
  }
  else
  {
    // Use two banks, interleaved.
    require (nBanks == 2)

    // Bank0 row's id wraps around if Bank1 is the starting bank.
    def b0Row(addr: UInt) =
      if (refillsToOneBank)
      {
        addr(untagBits-1, blockOffBits-log2Ceil(refillCycles)+1) + bank(addr)
      }
      else
      {
        addr(untagBits-1, blockOffBits-log2Ceil(refillCycles)) + bank(addr)
      }
    // Bank1 row's id stays the same regardless of which Bank has the fetch address.
    def b1Row(addr: UInt) =
      if (refillsToOneBank)
      {
        addr(untagBits-1, blockOffBits-log2Ceil(refillCycles)+1)
      }
      else
      {
        addr(untagBits-1, blockOffBits-log2Ceil(refillCycles))
      }

    s1_bankId := RegNext(bank(s0_vaddr))

    for (i <- 0 until nWays)
    {
      val s0_ren = s0_valid || s0_slaveValid
      val way = Mux(s3_slaveValid, scratchpadWay(s1s3_slaveAddr), repl_way)
      val wen = ((refill_one_beat && !invalidated) || s3_slaveValid) && way === i.U

      var mem_idx0: UInt = null
      var mem_idx1: UInt = null

      if (refillsToOneBank)
      {
        // write a refill beat across only one beat.
        mem_idx0 =
          Mux(refill_one_beat, (refill_idx << (log2Ceil(refillCycles)-1)) | (refill_cnt >> 1.U),
          b0Row(s0_vaddr))
        mem_idx1 =
          Mux(refill_one_beat, (refill_idx << (log2Ceil(refillCycles)-1)) | (refill_cnt >> 1.U),
          b1Row(s0_vaddr))

        val data = Mux(s3_slaveValid, s1s3_slaveData, tl_out.d.bits.data)
        when (wen && refill_cnt(0) === 0.U)
        {
          dataArraysB0(i).write(mem_idx0, dECC.encode(data))
        }
        when (wen && refill_cnt(0) === 1.U)
        {
          dataArraysB1(i).write(mem_idx1, dECC.encode(data))
        }
      }
      else
      {
        // write a refill beat across both banks.
        mem_idx0 =
          Mux(refill_one_beat, (refill_idx << log2Ceil(refillCycles)) | refill_cnt,
          b0Row(s0_vaddr))
        mem_idx1 =
          Mux(refill_one_beat, (refill_idx << log2Ceil(refillCycles)) | refill_cnt,
          b1Row(s0_vaddr))

        when (wen)
        {
          val data = Mux(s3_slaveValid, s1s3_slaveData, tl_out.d.bits.data)
          dataArraysB0(i).write(mem_idx0, dECC.encode(data(wordBits/2-1, 0)))
          dataArraysB1(i).write(mem_idx1, dECC.encode(data(wordBits-1, wordBits/2)))
        }
      }
      s1_dout(i) := Cat(dataArraysB1(i).read(mem_idx1, !wen && s0_ren), dataArraysB0(i).read(mem_idx0, !wen && s0_ren))
    }
  }

  val s1_clk_en = s1_valid || s1_slaveValid
  val s2_tag_hit = RegEnable(s1_tag_hit, s1_clk_en)
  val s2_hit_way = OHToUInt(s2_tag_hit)
  val s2_scratchpad_word_addr = Cat(s2_hit_way,
                                    Mux(s2_slaveValid,
                                        s1s3_slaveAddr,
                                        io.s2_vaddr)(untagBits-1, log2Ceil(wordBits/8)), 0.U(log2Ceil(wordBits/8).W))
  val s2_dout = RegEnable(s1_dout, s1_clk_en)
  val s2_bankId = RegEnable(s1_bankId, s1_clk_en)
  val s2_way_mux = Mux1H(s2_tag_hit, s2_dout)

  val s2_tag_disparity = RegEnable(s1_tag_disparity, s1_clk_en).asUInt.orR
  val s2_tl_error = RegEnable(s1_tl_error.asUInt.orR, s1_clk_en)

  // Let's rely on dead code elimination to get rid of extra logic here.
  // If our I$ is unbanked.
  val s2_unbankedDataDecoded = dECC.decode(s2_way_mux)
  // If our I$ is banked we need to do some more shuffling.
  val sz = s2_way_mux.getWidth
  val s2_bank0DataDecoded = dECC.decode(s2_way_mux(sz/2-1,0))
  val s2_bank1DataDecoded = dECC.decode(s2_way_mux(sz-1, sz/2))
  // NOTE: if we run off the cache-line, the Bank0Data is garbage. The pipeline should not use those instructions.

  val s2_data =
    if (nBanks == 2)
    {
      Mux(s2_bankId,
        Cat(s2_bank0DataDecoded.uncorrected, s2_bank1DataDecoded.uncorrected),
        Cat(s2_bank1DataDecoded.uncorrected, s2_bank0DataDecoded.uncorrected))
    }
    else
    {
      s2_unbankedDataDecoded.uncorrected
    }
  val s2_deccError =
    if (nBanks == 2) (s2_bank0DataDecoded.error || s2_bank1DataDecoded.error)
    else s2_unbankedDataDecoded.error
  val s2_deccUncorrectable =
    if (nBanks == 2) (s2_bank0DataDecoded.uncorrectable || s2_bank1DataDecoded.uncorrectable)
    else s2_unbankedDataDecoded.uncorrectable
  val s2_deccCorrectable =
    if (nBanks == 2) (s2_bank0DataDecoded.correctable || s2_bank1DataDecoded.correctable)
    else s2_unbankedDataDecoded.correctable

  val s2_disparity = s2_tag_disparity || s2_deccError
  val s2_full_word_write = WireInit(false.B)

  val s1_scratchpad_hit = Mux(s1_slaveValid,
                              lineInScratchpad(scratchpadLine(s1s3_slaveAddr)),
                              addrInScratchpad(io.s1_paddr))
  val s2_scratchpad_hit = RegEnable(s1_scratchpad_hit, s1_clk_en)
  val s2_report_uncorrectable_error =
    s2_scratchpad_hit &&
    s2_deccUncorrectable &&
    (s2_valid || (s2_slaveValid && !s2_full_word_write))
  val s2_error_addr = scratchpadBase.map(base => Mux(s2_scratchpad_hit,
                                                     base + s2_scratchpad_word_addr,
                                                     0.U)).getOrElse(0.U)

  // output signals
  outer.icacheParams.latency match {
    case 1 =>
      require(tECC.isInstanceOf[IdentityCode])
      require(dECC.isInstanceOf[IdentityCode])
      require(outer.icacheParams.itimAddr.isEmpty)
      io.resp.bits.data := Mux1H(s1_tag_hit, s1_dout)
      io.resp.bits.ae := s1_tl_error.asUInt.orR
      io.resp.valid := s1_valid && s1_hit

    case 2 =>
      // when some sort of memory bit error have occurred
      when (s2_valid && s2_disparity) { invalidate := true.B }

      io.resp.bits.data := s2_data
      io.resp.bits.ae := s2_tl_error
      io.resp.bits.replay := s2_disparity
      io.resp.valid := s2_valid && s2_hit

      io.errors.correctable.foreach { c =>
        c.valid := (s2_valid || s2_slaveValid) && s2_disparity && !s2_report_uncorrectable_error
        c.bits := s2_error_addr
      }
      io.errors.uncorrectable.foreach { u =>
        u.valid := s2_report_uncorrectable_error
        u.bits := s2_error_addr
      }

      tl_in.map { tl =>
        val respValid = RegInit(false.B)
        tl.a.ready := !(tl_out.d.valid || s1_slaveValid || s2_slaveValid || s3_slaveValid || respValid)
        val s1_a = RegEnable(tl.a.bits, s0_slaveValid)
        s2_full_word_write := edge_in.get.hasData(s1_a) && s1_a.mask.andR
        when (s0_slaveValid) {
          val a = tl.a.bits
          s1s3_slaveAddr := tl.a.bits.address
          s1s3_slaveData := tl.a.bits.data
          when (edge_in.get.hasData(a)) {
            val enable = scratchpadWayValid(scratchpadWay(a.address))
            when (!lineInScratchpad(scratchpadLine(a.address))) {
              scratchpadMax.get := scratchpadLine(a.address)
              invalidate := true.B
            }
            scratchpadOn := enable

            val itim_allocated = !scratchpadOn && enable
            val itim_deallocated = scratchpadOn && !enable
            val itim_increase = scratchpadOn && enable && scratchpadLine(a.address) > scratchpadMax.get
            val refilling = refill_valid && refill_cnt > 0.U
            ccover(itim_allocated, "ITIM_ALLOCATE", "ITIM allocated")
            ccover(itim_allocated && refilling, "ITIM_ALLOCATE_WHILE_REFILL", "ITIM allocated while I$ refill")
            ccover(itim_deallocated, "ITIM_DEALLOCATE", "ITIM deallocated")
            ccover(itim_deallocated && refilling, "ITIM_DEALLOCATE_WHILE_REFILL", "ITIM deallocated while I$ refill")
            ccover(itim_increase, "ITIM_SIZE_INCREASE", "ITIM size increased")
            ccover(itim_increase && refilling, "ITIM_SIZE_INCREASE_WHILE_REFILL", "ITIM size increased while I$ refill")
          }
        }

        val s2_dataCorrected =
          if (nBanks == 2)
          {
            Mux(s2_bankId,
              Cat(s2_bank0DataDecoded.corrected, s2_bank1DataDecoded.corrected),
              Cat(s2_bank1DataDecoded.corrected, s2_bank0DataDecoded.corrected))
          }
          else
          {
            s2_unbankedDataDecoded.corrected
          }

        assert(!s2_valid || RegNext(RegNext(s0_vaddr)) === io.s2_vaddr)
        when (!(tl.a.valid || s1_slaveValid || s2_slaveValid || respValid)
              && s2_valid && s2_deccError  && !s2_tag_disparity)
        {
          // handle correctable errors on CPU accesses to the scratchpad.
          // if there is an in-flight slave-port access to the scratchpad,
          // report the a miss but don't correct the error (as there is
          // a structural hazard on s1s3_slaveData/s1s3_slaveAddress).
          s3_slaveValid := true.B
          // TODO this doesn't make sense if we go off the cache-line.
          s1s3_slaveData := s2_dataCorrected
          s1s3_slaveAddr := s2_scratchpad_word_addr | s1s3_slaveAddr(log2Ceil(wordBits/8)-1, 0)
        }

        respValid := s2_slaveValid || (respValid && !tl.d.ready)
        val respError = RegEnable(
          s2_scratchpad_hit &&
          s2_deccUncorrectable  &&
          !s2_full_word_write, s2_slaveValid)
        when (s2_slaveValid)
        {
          when (edge_in.get.hasData(s1_a) || s2_deccError) { s3_slaveValid := true.B }
          def byteEn(i: Int) = !(edge_in.get.hasData(s1_a) && s1_a.mask(i))
          s1s3_slaveData := (0 until wordBits/8).map(i => Mux(byteEn(i),
                                                              s2_dataCorrected,
                                                              s1s3_slaveData)(8*(i+1)-1, 8*i)).asUInt
        }

        tl.d.valid := respValid
        tl.d.bits := Mux(edge_in.get.hasData(s1_a),
          edge_in.get.AccessAck(s1_a),
          edge_in.get.AccessAck(s1_a, 0.U, denied = false.B, corrupt = respError))
        tl.d.bits.data := s1s3_slaveData

        // Tie off unused channels
        tl.b.valid := false.B
        tl.c.ready := true.B
        tl.e.ready := true.B

        ccover(s0_valid && s1_slaveValid, "CONCURRENT_ITIM_ACCESS_1", "ITIM accessed, then I$ accessed next cycle")
        ccover(s0_valid && s2_slaveValid, "CONCURRENT_ITIM_ACCESS_2", "ITIM accessed, then I$ accessed 2 cycles later")
        ccover(tl.d.valid && !tl.d.ready, "ITIM_D_STALL", "ITIM response blocked by D-channel")
        ccover(tl_out.d.valid && !tl_out.d.ready, "ITIM_BLOCK_D", "D-channel blocked by ITIM access")
      }
  }

  tl_out.a.valid := s2_miss && !refill_valid
  tl_out.a.bits := edge_out.Get(
                    fromSource = 0.U,
                    toAddress = (refill_paddr >> blockOffBits) << blockOffBits,
                    lgSize = lgCacheBlockBytes.U)._2
  if (cacheParams.prefetch)
  {
    val (crosses_page, next_block) = Split(refill_paddr(pgIdxBits-1, blockOffBits) +& 1.U, pgIdxBits-blockOffBits)
    when (tl_out.a.fire())
    {
      send_hint := !hint_outstanding && io.s2_prefetch && !crosses_page
      when (send_hint)
      {
        send_hint := false.B
        hint_outstanding := true.B
      }
    }
    when (refill_done)
    {
      send_hint := false.B
    }
    when (tl_out.d.fire() && !refill_one_beat)
    {
      hint_outstanding := false.B
    }

    when (send_hint)
    {
      tl_out.a.valid := true.B
      tl_out.a.bits := edge_out.Hint(
                        fromSource = 1.U,
                        toAddress = Cat(refill_paddr >> pgIdxBits, next_block) << blockOffBits,
                        lgSize = lgCacheBlockBytes.U,
                        param = TLHints.PREFETCH_READ)._2
    }

    ccover(send_hint && !tl_out.a.ready,
           "PREFETCH_A_STALL", "I$ prefetch blocked by A-channel")
    ccover(refill_valid && (tl_out.d.fire() && !refill_one_beat),
           "PREFETCH_D_BEFORE_MISS_D", "I$ prefetch resolves before miss")
    ccover(!refill_valid && (tl_out.d.fire() && !refill_one_beat),
           "PREFETCH_D_AFTER_MISS_D", "I$ prefetch resolves after miss")
    ccover(tl_out.a.fire() && hint_outstanding,
           "PREFETCH_D_AFTER_MISS_A", "I$ prefetch resolves after second miss")
  }
  tl_out.b.ready := true.B
  tl_out.c.valid := false.B
  tl_out.e.valid := false.B
  assert(!(tl_out.a.valid && addrMaybeInScratchpad(tl_out.a.bits.address)))

  when (!refill_valid) { invalidated := false.B }
  when (refill_fire) { refill_valid := true.B }
  when (refill_done) { refill_valid := false.B}

  io.perf.acquire := refill_fire

  ccover(!send_hint && (tl_out.a.valid && !tl_out.a.ready), "MISS_A_STALL", "I$ miss blocked by A-channel")
  ccover(invalidate && refill_valid, "FLUSH_DURING_MISS", "I$ flushed during miss")

  def ccover(cond: Bool, label: String, desc: String)(implicit sourceInfo: SourceInfo) =
    cover(cond, s"ICACHE_$label", "MemorySystem;;" + desc)

  val mem_active_valid = Seq(CoverBoolean(s2_valid, Seq("mem_active")))
  val data_error = Seq(
    // TODO add covers for both banks.
    CoverBoolean(!s2_deccUncorrectable, Seq("no_data_error")),
    CoverBoolean(s2_deccCorrectable, Seq("data_correctable_error")),
    CoverBoolean(s2_deccUncorrectable, Seq("data_uncorrectable_error")))
  val request_source = Seq(
    CoverBoolean(!s2_slaveValid, Seq("from_CPU")),
    CoverBoolean(s2_slaveValid, Seq("from_TL"))
  )
  val tag_error = Seq(
    CoverBoolean(!s2_tag_disparity, Seq("no_tag_error")),
    CoverBoolean(s2_tag_disparity, Seq("tag_error"))
  )
  val mem_mode = Seq(
    CoverBoolean(s2_scratchpad_hit, Seq("ITIM_mode")),
    CoverBoolean(!s2_scratchpad_hit, Seq("cache_mode"))
  )

  val error_cross_covers = new CrossProperty(
    Seq(mem_active_valid, data_error, tag_error, request_source, mem_mode),
    Seq(
      // tag error cannot occur in ITIM mode
      Seq("tag_error", "ITIM_mode"),
      // Can only respond to TL in ITIM mode
      Seq("from_TL", "cache_mode")
    ),
    "MemorySystem;;Memory Bit Flip Cross Covers")

  cover(error_cross_covers)

  val ramWidth = dECC.width(wordBits/nBanks)
  override def toString: String =
    "\n   ==L1-ICache==" +
    "\n   Fetch bytes   : " + cacheParams.fetchBytes +
    "\n   Block bytes   : " + (1 << blockOffBits) +
    "\n   Row bytes     : " + rowBytes +
    "\n   Word bits     : " + wordBits +
    "\n   Sets          : " + nSets +
    "\n   Ways          : " + nWays +
    "\n   Refill cycles : " + refillCycles +
    "\n   RAMs          : (" +  ramWidth + " x " + nSets*refillCycles + ") using " + nBanks + " banks"  +
    "\n   " + (if (nBanks == 2) "Dual-banked" else "Single-banked") +
    "\n   I-TLB entries : " + cacheParams.nTLBEntries + "\n"
}

// Provide a BlackBox version of the ICache.
// NOTE: we can't really BlackBox a LazyModule, so instead we use the
// LazyModuleImp as a thin shim around the actual BlackBox itself.
// However, we have to provide another level of indirection through the IOs to
// avoid an emitter error (which may be related to Option()s).
class ICacheModuleBlackBox(outer: ICache) extends ICacheBaseModule(outer)
{
  val icachebb = Module(new ICacheBlackBox(outer))
  io <> icachebb.io.signals
}

class ICacheBlackBox(outer: ICache) extends BlackBox
{
  val io = IO(new ICacheBundleShim(outer))
}

class ICacheBundleShim(val outer: ICache) extends CoreBundle()(outer.p)
{
  val signals = new ICacheBundle(outer)
}

