#include <cstdio>
#include <cstdint>

#define MAX_TILES 8
#define NUM_COUNTERS 110

static uint64_t last_counters[MAX_TILES][NUM_COUNTERS];

static const char* counter_names[NUM_COUNTERS] = {
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
    // New core counters (40-59)
    "dispatch_slots_valid",
    "issued_int_total", "issued_mem_total", "issued_mul_total", "issued_div_total",
    "flush_xcpt", "flush_eret", "flush_refetch", "flush_next",
    "dis_stall",
    "br_cond_mispredict", "br_indirect_mispredict", "br_ret_mispredict", "br_no_prediction",
    "fetch_bubble_raw", "fetch_slots_delivered", "decode_backend_stall",
    "int_iq_empty", "mem_iq_empty", "sfb_opt_events",
    // Memory ordering counters (60-67)
    "stld_fwd_stall_cycles", "stld_fwd_success", "stld_fwd_wakeup_retries",
    "stld_fwd_block_load_wakeup_cycles", "mem_order_failures",
    "load_ordering_failures", "load_spec_mispredict", "load_nack_retries",
    // Data dependency counters (68-74)
    "dep_stall_cycles", "operand_wait_slot_cycles",
    "iq_dispatched_ready", "iq_dispatched_not_ready",
    "issued_with_poison", "ldspec_squash_grants", "spec_ld_wakeup_events",
    // L2 cache counters (75-91)
    "l2_pf_hint_req_accepted", "l2_pf_hint_req_blocked",
    "l2_pf_alloc_dir_miss", "l2_pf_alloc_dir_hit",
    "l2_demand_alloc_dir_miss", "l2_demand_hit_prefetched",
    "l2_demand_hit_pf_brought", "l2_demand_queued_behind_pf",
    "l2_demand_hit_regular",
    "l2_secondary_misses", "l2_evict_dirty", "l2_evict_clean",
    "l2_evict_prefetched",
    "l2_mshr_occ_sum", "l2_mshr_full",
    "l2_set_conflict_stall", "l2_bank_conflict",
    // OOO engine counters (92-98)
    "int_preg_stall_cycles", "fp_preg_stall_cycles",
    "retire_width_0_cycles", "retire_width_1_cycles", "retire_width_2_cycles",
    "retire_width_3_cycles", "retire_width_4_cycles",
    // Fetch/decode counters (99)
    "icache_lookups",
    // L3 TMA counters (100-108)
    "l1d_miss_pending", "divider_active",
    "no_issue", "issued_c1", "issued_c2", "issued_c3",
    "icache_stall", "itlb_stall", "branch_mispredict_recovery",
    // L2 extra counter (109)
    "l2_demand_miss_pending"
};

extern "C" void tma_counter_store(int tile_id, int idx, uint64_t value) {
    if (tile_id >= 0 && tile_id < MAX_TILES && idx >= 0 && idx < NUM_COUNTERS) {
        last_counters[tile_id][idx] = value;
    }
}

extern "C" void tma_counter_dump_final(int tile_id) {
    if (tile_id < 0 || tile_id >= MAX_TILES) return;
    fprintf(stderr, "===== TMA PERFORMANCE COUNTERS (tile %d) =====\n", tile_id);
    for (int i = 0; i < NUM_COUNTERS; i++) {
        fprintf(stderr, "  %-32s = %20lu\n", counter_names[i], last_counters[tile_id][i]);
    }
    fprintf(stderr, "==============================================\n");
}
