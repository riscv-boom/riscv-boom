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
  val u   = UInt(2.W)
  val ctr = UInt(3.W)
}


class TageUpdate extends Bundle {
  val ctr = UInt(3.W)
  val u   = UInt(2.W)
}


class TageTable(val nRows: Int, val tagSz: Int, val histLength: Int)
  (implicit p: Parameters) extends BoomModule()(p)
  with HasBoomFrontendParameters
{
  val io = IO( new Bundle {
    val f0_req = Input(Valid(new BranchPredictionBankRequest))

    val f3_resp = Output(Vec(bankWidth, Valid(new TageEntry(tagSz))))

    val update_mask  = Input(Vec(bankWidth, Bool()))
    val update_bits  = Input(Vec(bankWidth, new TageUpdate))
    val update_pc    = Input(UInt())
    val update_hist  = Input(UInt())
  })

  def compute_tag_and_hash(unhashed_idx: UInt, hist: UInt) = {
    val folded_history = if (histLength > log2Ceil(nRows)) {
      require(histLength < 2*log2Ceil(nRows))
      val base = hist(histLength-1, 0)
      base(log2Ceil(nRows)-1,0) ^ (base >> log2Ceil(nRows))
    } else {
      hist(histLength-1, 0)
    }
    val idx = unhashed_idx ^ folded_history
    val tag = (unhashed_idx >> log2Ceil(nRows))(tagSz-1,0)
    (idx, tag)
  }

  val doing_reset = RegInit(true.B)
  val reset_idx = RegInit(0.U(log2Ceil(nRows).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (nRows-1).U) { doing_reset := false.B }

  val (s0_hashed_idx, s0_tag) = compute_tag_and_hash(fetchIdx(io.f0_req.bits.pc), io.f0_req.bits.hist)

  val table  = Seq.fill(bankWidth) { SyncReadMem(nRows, new TageEntry(tagSz)) }

  val s1_hashed_idx = RegNext(s0_hashed_idx)
  val s1_tag        = RegNext(s0_tag)

  val s1_req_rtage = VecInit(table.map(_.read(s0_hashed_idx, io.f0_req.valid)))
  val s1_req_rhits = VecInit(s1_req_rtage.map(e => e.valid && e.tag === s1_tag && !doing_reset))

  val s2_req_rtage = RegNext(s1_req_rtage)
  val s2_req_rhits = RegNext(s1_req_rhits)
  val s2_tag       = RegNext(s1_tag)

  for (w <- 0 until bankWidth) {
    // This bit indicates the TAGE table matched here
    io.f3_resp(w).valid := RegNext(s2_req_rhits(w))
    io.f3_resp(w).bits  := RegNext(s2_req_rtage(w))
  }

  val (update_idx, update_tag) = compute_tag_and_hash(fetchIdx(io.update_pc), io.update_hist)
  for (w <- 0 until bankWidth) {
    val update_entry = Wire(new TageEntry(tagSz))
    update_entry.ctr   := io.update_bits(w).ctr
    update_entry.u     := io.update_bits(w).u
    update_entry.valid := true.B
    update_entry.tag   := update_tag
    when (io.update_mask(w) || doing_reset) {
      table(w).write(Mux(doing_reset, reset_idx, update_idx),
                     Mux(doing_reset, (0.U).asTypeOf(new TageEntry(tagSz)), update_entry))
    }
  }

  // val s0_update_idx, s0_update_tag = compute_hash(fetchIdx(io.update_pc, io.update_hist))
  // for (w <- 0 until bankWidth) {
  //   val update = Wire(new TageEntry)
  //   update.ctr := io.updates(w).bits.ctr
  //   update.u   := io.updates(w).bits.u
  //   update.tag := s0_update_tag
  //   when (io.updates(w).valid) {
  //     table(w).write(s0_update_idx, update)
  //   }
  // }


}

class TageBranchPredictorBank(implicit p: Parameters) extends BranchPredictorBank()(p)
{
  val base = Module(new DenseBTBBranchPredictorBank)
  base.io.f0_req := io.f0_req
  base.io.update := io.update
  base.io.update.bits.meta := io.update.bits.meta >> (new TageMeta).getWidth
  io.f1_resp := base.io.f1_resp
  io.f2_resp := base.io.f2_resp
  io.f3_resp := base.io.f3_resp

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

  val f3_meta = Wire(new TageMeta)

  tables.map(_.io.f0_req := io.f0_req)
  val f3_resps = VecInit(tables.map(_.io.f3_resp))

  val s1_update_meta = s1_update.bits.meta.asTypeOf(new TageMeta)
  val s1_update_mispredict_mask = UIntToOH(s1_update.bits.cfi_idx.bits) &
    Fill(bankWidth, s1_update.bits.cfi_mispredicted)

  val s1_update_mask  = WireInit((0.U).asTypeOf(Vec(tageNTables, Vec(bankWidth, Bool()))))
  val s1_update_wdata = WireInit((0.U).asTypeOf(Vec(tageNTables, Vec(bankWidth, new TageUpdate))))

  for (w <- 0 until bankWidth) {
    var altpred = base.io.f3_resp(w).taken
    var provided = false.B
    var provider = 0.U
    io.f3_resp(w).taken := base.io.f3_resp(w).taken

    for (i <- 0 until tageNTables) {
      val hit = f3_resps(i)(w).valid

      when (hit) {
        io.f3_resp(w).taken                 := f3_resps(i)(w).bits.ctr(2)
      }

      provided = provided || hit
      provider = Mux(hit, i.U, provider)
      altpred  = Mux(hit, f3_resps(i)(w).bits.ctr(2), altpred)
    }
    f3_meta.provider(w).valid := provided
    f3_meta.provider(w).bits  := provider
    f3_meta.alt_differs(w)    := altpred =/= io.f3_resp(w).taken
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
        s1_update_wdata(provider)(w).u := inc_u(s1_update_meta.provider_u(w),
                                                s1_update_meta.alt_differs(w),
                                                s1_update_mispredict_mask(w))
        s1_update_wdata(provider)(w).ctr := inc_ctr(s1_update_meta.provider_ctr(w),
                                                    update_was_taken)

      }
      when (s1_update_mispredict_mask(w)) {
        val allocate = s1_update_meta.allocate(w)
        when (allocate.valid) {
          s1_update_mask(allocate.bits)(w)      := true.B
          s1_update_wdata(allocate.bits)(w).u   := 1.U
          s1_update_wdata(allocate.bits)(w).ctr := Mux(s1_update.bits.cfi_taken, 4.U, 3.U)
        } .otherwise {
          // TODO: Decrement counters here
          
        }
      }
    }
  }

  for (i <- 0 until tageNTables) {
    for (w <- 0 until bankWidth) {
      tables(i).io.update_mask(w) := RegNext(s1_update_mask(i)(w))
      tables(i).io.update_bits(w) := RegNext(s1_update_wdata(i)(w))

    }
    tables(i).io.update_pc    := RegNext(s1_update.bits.pc)
    tables(i).io.update_hist  := RegNext(s1_update.bits.hist)
  }


  io.f3_meta := (base.io.f3_meta << (new TageMeta).getWidth) | f3_meta.asUInt
}
