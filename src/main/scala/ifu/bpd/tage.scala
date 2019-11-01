package boom.ifu

import chisel3._
import chisel3.util._
import chisel3.experimental.dontTouch

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.util.{BoomCoreStringPrefix, MaskLower}

class TageMeta(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val provider      = Vec(bankWidth, Valid(UInt(log2Ceil(tageNTables).W)))
  val alt_differs   = Vec(bankWidth, Bool())
  val provider_u    = Vec(bankWidth, UInt(2.W))
  val provider_ctr  = Vec(bankWidth, UInt(3.W))
  val allocate      = Vec(bankWidth, Valid(UInt(log2Ceil(tageNTables).W)))
}


class TageEntry(val tagSz: Int) extends Bundle {
  val valid = Bool()
  val tag = UInt(tagSz.W)
  val ctr = UInt(3.W)
}


class TageResp extends Bundle {
  val ctr = UInt(3.W)
  val u   = UInt(2.W)
}


class TageTable(val nRows: Int, val tagSz: Int, val histLength: Int)
  (implicit p: Parameters) extends BoomModule()(p)
  with HasBoomFrontendParameters
{
  val io = IO( new Bundle {
    val f0_req = Input(Valid(new BranchPredictionBankRequest))

    val f3_resp = Output(Vec(bankWidth, Valid(new TageResp)))

    val update_mask  = Input(Vec(bankWidth, Bool()))
    val update_ctr   = Input(Vec(bankWidth, UInt(3.W)))
    val update_pc    = Input(UInt())
    val update_hist  = Input(UInt())

    val update_u_mask = Input(Vec(bankWidth, Bool()))
    val update_u = Input(Vec(bankWidth, UInt(2.W)))
  })

  def compute_tag_and_hash(unhashed_idx: UInt, hist: UInt) = {
    val folded_history = if (histLength > log2Ceil(nRows)) {
      require(histLength < 2*log2Ceil(nRows))
      val base = hist(histLength-1, 0)
      base(log2Ceil(nRows)-1,0) ^ (base >> log2Ceil(nRows))
    } else {
      hist(histLength-1, 0)
    }
    val idx = (unhashed_idx ^ folded_history)(log2Ceil(nRows)-1,0)
    val tag = (unhashed_idx >> log2Ceil(nRows))(tagSz-1,0)
    (idx, tag)
  }

  val doing_reset = RegInit(true.B)
  val reset_idx = RegInit(0.U(log2Ceil(nRows).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (nRows-1).U) { doing_reset := false.B }

  val (s0_hashed_idx, s0_tag) = compute_tag_and_hash(fetchIdx(io.f0_req.bits.pc), io.f0_req.bits.hist)

  val hi_us  = Seq.fill(bankWidth) { SyncReadMem(nRows, Bool()) }
  val lo_us  = Seq.fill(bankWidth) { SyncReadMem(nRows, Bool()) }
  val table  = Seq.fill(bankWidth) { SyncReadMem(nRows, new TageEntry(tagSz)) }

  val s1_hashed_idx = RegNext(s0_hashed_idx)
  val s1_tag        = RegNext(s0_tag)

  val s1_req_rtage = VecInit(table.map(_.read(s0_hashed_idx, io.f0_req.valid)))
  val s1_req_rhius = VecInit(hi_us.map(_.read(s0_hashed_idx, io.f0_req.valid)))
  val s1_req_rlous = VecInit(lo_us.map(_.read(s0_hashed_idx, io.f0_req.valid)))
  val s1_req_rhits = VecInit(s1_req_rtage.map(e => e.valid && e.tag === s1_tag && !doing_reset))

  val s2_req_rtage = RegNext(s1_req_rtage)
  val s2_req_rhius = RegNext(s1_req_rhius)
  val s2_req_rlous = RegNext(s1_req_rlous)
  val s2_req_rhits = RegNext(s1_req_rhits)
  val s2_tag       = RegNext(s1_tag)

  for (w <- 0 until bankWidth) {
    // This bit indicates the TAGE table matched here
    io.f3_resp(w).valid    := RegNext(s2_req_rhits(w))
    io.f3_resp(w).bits.u   := RegNext(Cat(s2_req_rhius(w), s2_req_rlous(w)))
    io.f3_resp(w).bits.ctr := RegNext(s2_req_rtage(w).ctr)
  }

  val clear_u_ctr = RegInit(0.U((log2Ceil(tageUBitPeriod) + log2Ceil(nRows) + 1).W))
  when (doing_reset) { clear_u_ctr := 1.U } .otherwise { clear_u_ctr := clear_u_ctr + 1.U }

  val doing_clear_u = clear_u_ctr(log2Ceil(tageUBitPeriod)-1,0) === 0.U
  val doing_clear_u_hi = doing_clear_u && clear_u_ctr(log2Ceil(tageUBitPeriod) + log2Ceil(nRows)) === 1.U
  val doing_clear_u_lo = doing_clear_u && clear_u_ctr(log2Ceil(tageUBitPeriod) + log2Ceil(nRows)) === 0.U
  val clear_u_idx = clear_u_ctr >> log2Ceil(tageUBitPeriod)

  val (update_idx, update_tag) = compute_tag_and_hash(fetchIdx(io.update_pc), io.update_hist)
  for (w <- 0 until bankWidth) {
    val update_entry = Wire(new TageEntry(tagSz))
    update_entry.ctr   := io.update_ctr(w)
    update_entry.valid := true.B
    update_entry.tag   := update_tag


    when (io.update_mask(w) || doing_reset) {
      val idx = Mux(doing_reset, reset_idx, update_idx)
      table(w).write(idx, Mux(doing_reset, (0.U).asTypeOf(new TageEntry(tagSz)), update_entry))
    }

    when (io.update_u_mask(w) || doing_reset || doing_clear_u_hi) {
      val idx = Mux(doing_reset, reset_idx,
                Mux(io.update_u_mask(w), update_idx, clear_u_idx))
      hi_us(w).write(idx, Mux(doing_reset || !io.update_u_mask(w), false.B, io.update_u(w)(1)))
    }

    when (io.update_u_mask(w) || doing_reset || doing_clear_u_lo) {
      val idx = Mux(doing_reset, reset_idx,
                Mux(io.update_u_mask(w), update_idx, clear_u_idx))
      lo_us(w).write(idx, Mux(doing_reset || !io.update_u_mask(w), false.B, io.update_u(w)(0)))
    }

  }



}

class TageBranchPredictorBank(implicit p: Parameters) extends BranchPredictorBank()(p)
{
  val base = Module(new BTBBranchPredictorBank(BoomBTBParams()))
  val micro = Module(new BTBBranchPredictorBank(
    BoomBTBParams(nSets = 32, tagSz = 4, offsetSz = 13, extendedNSets = 0, micro = true))
  )

  base.io.f0_req := io.f0_req
  base.io.update := io.update
  base.io.update.bits.meta := io.update.bits.meta(base.metaSz-1,0)
  micro.io.f0_req := io.f0_req
  micro.io.update := io.update
  micro.io.update.bits.meta := io.update.bits.meta(base.metaSz+micro.metaSz-1,base.metaSz)

  io.f1_resp := micro.io.f1_resp
  io.f2_resp := base.io.f2_resp
  io.f3_resp := base.io.f3_resp

  val f3_meta = Wire(new TageMeta)

  override val metaSz = base.metaSz + micro.metaSz + f3_meta.asUInt.getWidth
  require(metaSz <= bpdMaxMetaLength)

  def inc_u(u: UInt, alt_differs: Bool, mispredict: Bool): UInt = {
    Mux(!alt_differs, u,
    Mux(mispredict, Mux(u === 0.U, 0.U, u - 1.U),
                    Mux(u === 3.U, 3.U, u + 1.U)))
  }
  def inc_ctr(ctr: UInt, taken: Bool): UInt = {
    Mux(!taken, Mux(ctr === 0.U, 0.U, ctr - 1.U),
                Mux(ctr === 7.U, 7.U, ctr + 1.U))
  }


  val tables = (0 until tageNTables) map { i =>
    Module(new TageTable(tageNSets(i), tageTagSz(i), tageHistoryLength(i)))
  }



  tables.map(_.io.f0_req := io.f0_req)
  val f3_resps = VecInit(tables.map(_.io.f3_resp))

  val s1_update_meta = (s1_update.bits.meta >> (base.metaSz + micro.metaSz)).asTypeOf(new TageMeta)
  val s1_update_mispredict_mask = UIntToOH(s1_update.bits.cfi_idx.bits) &
    Fill(bankWidth, s1_update.bits.cfi_mispredicted)

  val s1_update_mask  = WireInit((0.U).asTypeOf(Vec(tageNTables, Vec(bankWidth, Bool()))))
  val s1_update_u_mask  = WireInit((0.U).asTypeOf(Vec(tageNTables, Vec(bankWidth, UInt(1.W)))))

  val s1_update_ctr = Wire(Vec(tageNTables, Vec(bankWidth, UInt(3.W))))
  val s1_update_u = Wire(Vec(tageNTables, Vec(bankWidth, UInt(2.W))))

  s1_update_ctr := DontCare
  s1_update_u := DontCare


  for (w <- 0 until bankWidth) {
    var altpred = base.io.f3_resp(w).taken
    val final_altpred = WireInit(base.io.f3_resp(w).taken)
    var provided = false.B
    var provider = 0.U
    io.f3_resp(w).taken := base.io.f3_resp(w).taken

    for (i <- 0 until tageNTables) {
      val hit = f3_resps(i)(w).valid

      when (hit) {
        io.f3_resp(w).taken := f3_resps(i)(w).bits.ctr(2)
        final_altpred       := altpred
      }

      provided = provided || hit
      provider = Mux(hit, i.U, provider)
      altpred  = Mux(hit, f3_resps(i)(w).bits.ctr(2), altpred)
    }
    f3_meta.provider(w).valid := provided
    f3_meta.provider(w).bits  := provider
    f3_meta.alt_differs(w)    := final_altpred =/= io.f3_resp(w).taken
    f3_meta.provider_u(w)     := f3_resps(provider)(w).bits.u
    f3_meta.provider_ctr(w)   := f3_resps(provider)(w).bits.ctr

    // Create a mask of tables which did not hit our query, and also contain useless entries
    // and also uses a longer history than the provider
    val allocatable_slots = (
      VecInit(f3_resps.map(r => !r(w).valid && r(w).bits.u === 0.U)).asUInt &
      ~(MaskLower(UIntToOH(provider)) & Fill(tageNTables, provided))
    )

    f3_meta.allocate(w).valid := allocatable_slots =/= 0.U
    f3_meta.allocate(w).bits  := PriorityEncoder(allocatable_slots)

    val update_was_taken = (s1_update.bits.cfi_idx.valid &&
                            (s1_update.bits.cfi_idx.bits === w.U) &&
                            s1_update.bits.cfi_taken)
    when (s1_update.bits.br_mask(w) && s1_update.valid) {
      when (s1_update_meta.provider(w).valid) {
        val provider = s1_update_meta.provider(w).bits

        s1_update_mask(provider)(w) := true.B
        s1_update_u_mask(provider)(w) := true.B

        val new_u = inc_u(s1_update_meta.provider_u(w),
                          s1_update_meta.alt_differs(w),
                          s1_update_mispredict_mask(w))
        s1_update_u(provider)(w) := new_u
        s1_update_ctr(provider)(w) := inc_ctr(s1_update_meta.provider_ctr(w),
                                              update_was_taken)

      }
    }
  }
  when (s1_update.valid && s1_update.bits.cfi_mispredicted && s1_update.bits.cfi_idx.valid) {
    val idx = s1_update.bits.cfi_idx.bits
    val allocate = s1_update_meta.allocate(idx)
    when (allocate.valid) {
      s1_update_mask(allocate.bits)(idx) := true.B
      s1_update_ctr (allocate.bits)(idx) := Mux(s1_update.bits.cfi_taken, 4.U, 3.U)

      s1_update_u_mask(allocate.bits)(idx) := true.B
      s1_update_u     (allocate.bits)(idx) := 0.U

    } .otherwise {
      val provider = s1_update_meta.provider(idx)
      val decr_mask = Mux(provider.valid, ~MaskLower(UIntToOH(provider.bits)), 0.U)

      for (i <- 0 until tageNTables) {
        when (decr_mask(i)) {
          s1_update_u_mask(i)(idx) := true.B
          s1_update_u     (i)(idx) := 0.U
        }
      }
    }

  }


  for (i <- 0 until tageNTables) {
    for (w <- 0 until bankWidth) {
      tables(i).io.update_mask(w) := RegNext(s1_update_mask(i)(w))
      tables(i).io.update_ctr(w)  := RegNext(s1_update_ctr(i)(w))

      tables(i).io.update_u_mask(w) := RegNext(s1_update_u_mask(i)(w))
      tables(i).io.update_u(w)      := RegNext(s1_update_u(i)(w))
    }
    tables(i).io.update_pc    := RegNext(s1_update.bits.pc)
    tables(i).io.update_hist  := RegNext(s1_update.bits.hist)
  }


  io.f3_meta := Cat(f3_meta.asUInt, micro.io.f3_meta(micro.metaSz-1,0), base.io.f3_meta(base.metaSz-1, 0))
}
