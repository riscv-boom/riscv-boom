package boom.v3.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{RegField, RegWriteFn}
import freechips.rocketchip.tilelink._
import midas.targetutils.SynthesizePrintf

// Number of 64-bit counter registers exposed via MMIO
// Layout (offsets in bytes):
//   0x000: control   (W: bit 0 = snapshot, bit 1 = release snapshot, bit 2 = dump (live - snapshot), R: 0)
//   0x008: cycles    (debug_tsc_reg)
//   0x010: instret   (debug_irt_reg)
//   0x018: tma_retiring
//   0x020: tma_bad_speculation
//   0x028: tma_frontend_bound
//   0x030: tma_backend_bound
//   0x038: tma_fetch_latency
//   0x040: tma_fetch_bandwidth
//   0x048: tma_branch_mispredict
//   0x050: tma_machine_clears
//   0x058: tma_memory_bound
//   0x060: tma_core_bound
//   0x068: retired_loads
//   0x070: retired_stores
//   0x078: retired_branches
//   0x080: retired_jals
//   0x088: retired_jalrs
//   0x090: retired_fp
//   0x098: retired_amo
//   0x0A0: retired_system
//   0x0A8: rob_full_cycles
//   0x0B0: ldq_full_cycles
//   0x0B8: stq_full_cycles
//   0x0C0: int_iq_full_cycles
//   0x0C8: mem_iq_full_cycles
//   0x0D0: branch_mask_full_cycles
//   0x0D8: rename_stall_cycles
//   0x0E0: flush_cycles
//   0x0E8: rollback_cycles
//   0x0F0: icache_miss
//   0x0F8: dcache_miss
//   0x100: dcache_release
//   0x108: itlb_miss
//   0x110: dtlb_miss
//   0x118: l2tlb_miss
//   0x120: br_mispredict
//   0x128: br_resolve
//   0x130: jalr_mispredict
//   0x138: br_mispredict_bpd
//   0x140: br_mispredict_btb
// --- New Core Counters (Pipeline/Execution, Branch Prediction, Fetch/Decode) ---
//   0x148: dispatch_slots_valid
//   0x150: issued_int_total
//   0x158: issued_mem_total
//   0x160: issued_mul_total
//   0x168: issued_div_total
//   0x170: flush_xcpt_events
//   0x178: flush_eret_events
//   0x180: flush_refetch_events
//   0x188: flush_next_events
//   0x190: dis_stall_cycles
//   0x198: br_cond_mispredict
//   0x1A0: br_indirect_mispredict
//   0x1A8: br_ret_mispredict
//   0x1B0: br_no_prediction
//   0x1B8: fetch_bubble_raw
//   0x1C0: fetch_slots_delivered
//   0x1C8: decode_backend_stall
//   0x1D0: int_iq_empty_cycles
//   0x1D8: mem_iq_empty_cycles
//   0x1E0: sfb_opt_events
// --- Memory Ordering Counters ---
//   0x1E8: stld_fwd_stall_cycles
//   0x1F0: stld_fwd_success
//   0x1F8: stld_fwd_wakeup_retries
//   0x200: stld_fwd_block_load_wakeup_cycles
//   0x208: mem_order_failures
//   0x210: load_ordering_failures
//   0x218: load_spec_mispredict
//   0x220: load_nack_retries
// --- Data Dependency Counters ---
//   0x228: dep_stall_cycles
//   0x230: operand_wait_slot_cycles
//   0x238: iq_dispatched_ready
//   0x240: iq_dispatched_not_ready
//   0x248: issued_with_poison
//   0x250: ldspec_squash_grants
//   0x258: spec_ld_wakeup_events
// --- L2 Cache Counters (from InclusiveCache) ---
//   0x260: l2_pf_hint_req_accepted
//   0x268: l2_pf_hint_req_blocked_cycles
//   0x270: l2_pf_hint_alloc_dir_miss
//   0x278: l2_pf_hint_alloc_dir_hit
//   0x280: l2_demand_alloc_dir_miss
//   0x288: l2_demand_alloc_dir_hit_on_prefetched
//   0x290: l2_demand_alloc_dir_hit_on_pf_brought
//   0x298: l2_demand_queued_behind_prefetch
//   0x2A0: l2_demand_alloc_dir_hit_regular
//   0x2A8: l2_secondary_misses
//   0x2B0: l2_evictions_dirty
//   0x2B8: l2_evictions_clean
//   0x2C0: l2_evictions_prefetched
//   0x2C8: l2_mshr_occupancy_sum
//   0x2D0: l2_mshr_full_cycles
//   0x2D8: l2_set_conflict_stall_cycles
//   0x2E0: l2_bank_conflict_cycles
// --- OOO Engine Counters ---
//   0x2E8: int_preg_stall_cycles
//   0x2F0: fp_preg_stall_cycles
//   0x2F8: retire_width_0_cycles
//   0x300: retire_width_1_cycles
//   0x308: retire_width_2_cycles
//   0x310: retire_width_3_cycles
//   0x318: retire_width_4_cycles
// --- Fetch/Decode Counters ---
//   0x320: icache_lookups
// --- L3 TMA Counters (Intel-inspired BOOM-native observability) ---
//   0x328: l1d_miss_pending
//   0x330: divider_active
//   0x338: no_issue
//   0x340: issued_c1
//   0x348: issued_c2
//   0x350: issued_c3
//   0x358: icache_stall
//   0x360: itlb_stall
//   0x368: branch_mispredict_recovery
// --- L2 Extra Counter (appended to avoid shifting existing indices) ---
//   0x370: l2_demand_miss_pending (cycles with any demand Acquire outstanding below L2)
// --- Another control ---
//   0x378: control2 (W: bit 0 = snapshot2, bit 1 = release snapshot2, R: 0)
//   0x380: read_select (W: 0 = read default, 1 = read snapshot, 2 = read snapshot2, 3 = read live, R: 0)

object BoomPerfCounterConsts {
  val CORE_NUM_COUNTERS = 60
  val L2_NUM_COUNTERS = 18 // Total L2 counters from InclusiveCache (must match InclusiveCacheParameters.L2_NUM_COUNTERS)
  val L2_INLINE_NUM_COUNTERS = 17 // L2 counters placed in the inline block (global indices 75-91)
  val MEM_ORDER_NUM_COUNTERS = 8
  val DATA_DEP_NUM_COUNTERS = 7
  val OOO_ENGINE_NUM_COUNTERS = 7
  val FETCH_DECODE_NUM_COUNTERS = 1
  val L3_TMA_NUM_COUNTERS = 9
  // Global layout: Core(60) + MemOrder(8) + DataDep(7) + L2Inline(17) + OOO(7) + FetchDecode(1) + L3TMA(9) + L2Extra(1) = 110
  val NUM_COUNTERS = CORE_NUM_COUNTERS + MEM_ORDER_NUM_COUNTERS + DATA_DEP_NUM_COUNTERS + L2_INLINE_NUM_COUNTERS + OOO_ENGINE_NUM_COUNTERS + FETCH_DECODE_NUM_COUNTERS + L3_TMA_NUM_COUNTERS + (L2_NUM_COUNTERS - L2_INLINE_NUM_COUNTERS) // 110
}

case class BoomPerfCounterParams(
  address: BigInt = 0x10030000L
)

class BoomPerfCounterIO extends Bundle {
  val counters = Input(Vec(BoomPerfCounterConsts.NUM_COUNTERS, UInt(64.W)))
}

class BoomPerfCounterDevice(params: BoomPerfCounterParams, beatBytes: Int)(implicit p: Parameters)
  extends LazyModule
{
  val device = new SimpleDevice("boom-perf-counters", Seq("ucb-bar,boom-perf-counters"))
  val node = TLRegisterNode(
    Seq(AddressSet(params.address, 4096 - 1)),
    device,
    "reg/control",
    beatBytes = beatBytes)

  override lazy val module: BoomPerfCounterDeviceImp = new BoomPerfCounterDeviceImp(this)
  class BoomPerfCounterDeviceImp(outer: BoomPerfCounterDevice) extends LazyModuleImp(outer) {
    val io = IO(new BoomPerfCounterIO)

    // Snapshot registers: when software writes bit 0 of control, latch all counters
    val snapshot = Reg(Vec(BoomPerfCounterConsts.NUM_COUNTERS, UInt(64.W)))
    val snapshotValid = RegInit(false.B)

    val snapshot2 = Reg(Vec(BoomPerfCounterConsts.NUM_COUNTERS, UInt(64.W)))
    val snapshot2Valid = RegInit(false.B)

    // 0x380 read_select: 0 = live, 1 = force snapshot, 2 = force snapshot2.
    // 64-bit Reg so it reads back cleanly; only low 2 bits affect the Mux.
    val readSelect = RegInit(0.U(64.W))

    // Determine which values to read based on readSelect
    // Default (readSelect=0): read live counters from io.counters or snapshot if snapshotValid to be backwards compatible
    // readSelect=1: read snapshot (ignoring live updates)
    // readSelect=2: read snapshot2 (independent snapshot for software-printf delta
    val readValues = Wire(Vec(BoomPerfCounterConsts.NUM_COUNTERS, UInt(64.W)))
    for (i <- 0 until BoomPerfCounterConsts.NUM_COUNTERS) {
      readValues(i) := MuxLookup(readSelect(1, 0), Mux(snapshotValid, snapshot(i), io.counters(i)))(Seq(
        1.U -> snapshot(i),
        2.U -> snapshot2(i), 
        3.U -> io.counters(i)
      ))
    }

    val controlWrite     = RegInit(0.U(64.W))
    val controlWriteValid = RegInit(false.B)
    when (controlWrite(0) && controlWriteValid) {
      // Snapshot: latch all counter values
      for (i <- 0 until BoomPerfCounterConsts.NUM_COUNTERS) {
        snapshot(i) := io.counters(i)
      }
      snapshotValid := true.B
    }
    when (controlWrite(1) && controlWriteValid) {
      // Release snapshot
      snapshotValid := false.B
    }

    // Control2 write handler (independent snapshot2 for software-printf delta)
    val controlWrite2     = RegInit(0.U(64.W))
    val controlWrite2Valid = RegInit(false.B)
    when (controlWrite2(0) && controlWrite2Valid) {
      for (i <- 0 until BoomPerfCounterConsts.NUM_COUNTERS) {
        snapshot2(i) := io.counters(i)
      }
      snapshot2Valid := true.B
    }
    when (controlWrite2(1) && controlWrite2Valid) {
      snapshot2Valid := false.B
    }
    when (controlWrite(2) && controlWriteValid) {
      // Dump all counters to simulation console
      // When a snapshot is active, print (live - snapshot) to isolate the
      // region between TMA_SNAPSHOT() and TMA_DUMP() calls in software.
      val names = Seq(
        "cycles", "instret",
        "retiring", "bad_speculation", "frontend_bound", "backend_bound",
        "fetch_latency", "fetch_bandwidth", "branch_mispredict", "machine_clears",
        "memory_bound", "core_bound",
        "retired_loads", "retired_stores", "retired_branches",
        "retired_jals", "retired_jalrs", "retired_fp", "retired_amo", "retired_system",
        "rob_full", "ldq_full", "stq_full", "int_iq_full", "mem_iq_full",
        "branch_mask_full", "rename_stall", "flush_cycles", "rollback_cycles",
        "icache_miss", "dcache_miss", "dcache_release",
        "itlb_miss", "dtlb_miss", "l2tlb_miss",
        "br_mispredict", "br_resolve", "jalr_mispredict", "br_mispred_bpd", "br_mispred_btb",
        // New core counters
        "dispatch_slots_valid",
        "issued_int_total", "issued_mem_total", "issued_mul_total", "issued_div_total",
        "flush_xcpt", "flush_eret", "flush_refetch", "flush_next",
        "dis_stall",
        "br_cond_mispredict", "br_indirect_mispredict", "br_ret_mispredict", "br_no_prediction",
        "fetch_bubble_raw", "fetch_slots_delivered", "decode_backend_stall",
        "int_iq_empty", "mem_iq_empty", "sfb_opt_events",
        // Memory ordering counters
        "stld_fwd_stall_cycles", "stld_fwd_success", "stld_fwd_wakeup_retries",
        "stld_fwd_block_load_wakeup_cycles", "mem_order_failures",
        "load_ordering_failures", "load_spec_mispredict", "load_nack_retries",
        // Data dependency counters
        "dep_stall_cycles", "operand_wait_slot_cycles",
        "iq_dispatched_ready", "iq_dispatched_not_ready",
        "issued_with_poison", "ldspec_squash_grants", "spec_ld_wakeup_events",
        // L2 cache counters
        "l2_pf_hint_req_accepted", "l2_pf_hint_req_blocked", "l2_pf_alloc_dir_miss", "l2_pf_alloc_dir_hit",
        "l2_demand_alloc_dir_miss", "l2_demand_hit_prefetched", "l2_demand_hit_pf_brought",
        "l2_demand_queued_behind_pf", "l2_demand_hit_regular",
        "l2_secondary_misses", "l2_evict_dirty", "l2_evict_clean", "l2_evict_prefetched",
        "l2_mshr_occ_sum", "l2_mshr_full", "l2_set_conflict_stall", "l2_bank_conflict",
        // OOO engine counters
        "int_preg_stall_cycles", "fp_preg_stall_cycles",
        "retire_width_0_cycles", "retire_width_1_cycles", "retire_width_2_cycles",
        "retire_width_3_cycles", "retire_width_4_cycles",
        // Fetch/decode counters
        "icache_lookups",
        // L3 TMA counters
        "l1d_miss_pending", "divider_active",
        "no_issue", "issued_c1", "issued_c2", "issued_c3",
        "icache_stall", "itlb_stall", "branch_mispredict_recovery",
        // L2 extra counter
        "l2_demand_miss_pending")
      SynthesizePrintf(printf("===== TMA PERFORMANCE COUNTERS =====\n"))
      for (i <- 0 until BoomPerfCounterConsts.NUM_COUNTERS) {
        val value = Mux(snapshotValid, io.counters(i) - snapshot(i), io.counters(i))
        SynthesizePrintf(printf(s"  %24s = %%d\n".format(names(i)), value))
      }
      SynthesizePrintf(printf("====================================\n"))
    }

    // Build register map: control at 0x000, then counters at 0x008..0x370,
    // then control2 at 0x378 and read_select at 0x380.
    val regmapEntries = Seq(
      0x000 -> Seq(RegField(64, controlWrite, RegWriteFn((valid, data) => {
        when (valid) {
          controlWrite     := data
          controlWriteValid := true.B
        } .otherwise {
          controlWriteValid := false.B
        }
        true.B
      }))),
      0x378 -> Seq(RegField(64, controlWrite2, RegWriteFn((valid, data) => {
        when (valid) {
          controlWrite2     := data
          controlWrite2Valid := true.B
        } .otherwise {
          controlWrite2Valid := false.B
        }
        true.B
      }))),
      0x380 -> Seq(RegField(64, readSelect, RegWriteFn((valid, data) => {
        when (valid) { readSelect := data }
        true.B
      })))
    ) ++ (0 until BoomPerfCounterConsts.NUM_COUNTERS).map { i =>
      (0x008 + i * 0x008) -> Seq(RegField.r(64, readValues(i)))
    }

    node.regmap(regmapEntries: _*)
  }
}
