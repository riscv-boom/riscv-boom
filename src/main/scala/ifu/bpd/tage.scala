package boom.ifu

import chisel3._
import chisel3.util._
import chisel3.experimental.dontTouch

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.common._
import boom.util.{BoomCoreStringPrefix}

// class TageCheckpoint(implicit p: Parameters) extends BoomBundle()(p)
// {
//   val alt_matches = Bool()
//   val ctrs = Vec(tageNTables, UInt(2.W))
//   val us   = Vec(tageNTables, Bool())
//   val hits = Vec(tageNTables, Bool())
// }

// class TageBankCheckpoint(implicit p: Parameters) extends BoomBundle()(p)
// {
//   val tage_checkpoints = Vec(bankWidth, new TageCheckpoint)
// }

// class TageEntry(val tagSz: Int) extends Bundle {
//   val tag = UInt(tagSz.W)
//   val ctr = UInt(3.W)
//   val u   = UInt(2.W)
// }

// class TageUpdate extends Bundle {
//   val ctr = UInt(3.W)
//   val u   = UInt(2.W)
// }

// class TageTable(val nRows: Int, val tagSz: Int, val histLength: Int)
//   (implicit p: Parameters) extends BoomModule()(p)
//   with HasBoomFrontendParameters
// {
//   val io = IO( new Bundle {
//     val f0_req = Input(Valid(new BranchPredictionBankRequest))

//     val f3_resp = Output(Vec(bankWidth, new BranchPrediction))

//     val update_pc   = Input(UInt(vaddrBitsExtended.W))
//     val update_hist = Input(UInt(globalHistoryLength.W))
//     val updates = Input(Vec(bankWidth, Valid(new TageUpdate)))
//   })

//   def compute_tag_and_hash(unhashed_idx: UInt, hist: UInt) = {
//     val folded_history = if (histLength > log2Ceil(nRows)) {
//       require(histLength < 2*log2Ceil(nRows))
//       hist(2*histLength-1, histLength) ^ hist(histLength-1, 0)
//     } else {
//       hist(histLength-1, 0)
//     }
//     val idx = unhashed_idx ^ folded_history
//     val tag = idx >> log2Ceil(nRows)
//     idx, tag
//   }

//   val (s0_hashed_idx, s0_tag) = compute_hash(fetchIdx(io.f0_req.bits.pc), io.f0_req.bits.hist)

//   val valids = RegInit(VecInit(Seq.fill(nRows) { false.B }))
//   val table  = Seq.fill(bankWidth) { SyncReadMem(nRows, new TageEntry(tagSz)) }

//   val s1_hashed_idx = RegNext(s0_hashed_idx)
//   val s1_tag        = RegNext(s0_tag)

//   val s1_req_rtage = table.map(_.read(s0_hashed_idx, io.f0_req.valid))
//   val s1_req_rhits = s1_req_rtage.map(e => e.tag === s1_tag && valids(s1_hashed_idx))

//   val s2_req_rtage = RegNext(s1_req_rtage)
//   val s2_req_rhits = RegNext(s1_req_rhits)

//   for (w <- 0 until bankWidth) {
//     io.f3_resp(w).is_br := RegNext(s2_req_rhits(w)) // This bit indicates the TAGE table matched here
//     io.f3_resp(w).taken := RegNext(s2_req_rhits(w) && s2_req_rtage(w).tag(ctr))
//     io.f3_resp(w).tage_meta.ctrs := RegNext(VecInit(Seq.fill(tageNTables) { s2_req_rtage(w).ctr }))
//     io.f3_resp(w).tage_meta.hits := RegNext(VecInit(Seq.fill(tageNTables) { s2_req_rhits(w) }))
//     io.f3_resp(w).tage_meta.us   := RegNext(VecInit(Seq.fill(tageNTables) { s2_req_rtage(w).u }))
//   }

//   val s0_update_idx, s0_update_tag = compute_hash(fetchIdx(io.update_pc, io.update_hist))
//   for (w <- 0 until bankWidth) {
//     val update = Wire(new TageEntry)
//     update.ctr := io.updates(w).bits.ctr
//     update.u   := io.updates(w).bits.u
//     update.tag := s0_update_tag
//     when (io.updates(w).valid) {
//       table(w).write(s0_update_idx, update)
//     }
//   }


// }

class TageBranchPredictorBank(implicit p: Parameters) extends BranchPredictorBank()(p)
{
  val base = Module(new DenseBTBBranchPredictorBank)
  base.io.f0_req := io.f0_req
  base.io.update := io.update
  io.f1_resp := base.io.f1_resp
  io.f2_resp := base.io.f2_resp
  io.f3_resp := base.io.f3_resp



  // val tables = (0 until tageNTables) map { i =>
  //   Module(new TageTableBank(tageNSets(i), tageTagSz(i), tageHistoryLength(i)))
  // }

  // tables.map(_.io.f0_req := io.f0_req)

  // for (w <- 0 until bankWidth) {
  //   var altpred = base.io.f3_resp(w).taken
  //   for (i <- 0 until tageNTables) {
  //     val pred_val = tables(i).io.f3_resp(w).is_br && base.io.f3_resp(w).is_br
  //     when (pred_val) {
  //       io.f3_resp(w).taken                 := tables(i).io.f3_resp(w).taken
  //     }
  //     io.f3_resp(w).tage_meta.ctrs(i)       := tables(i).io.f3_resp(w).tage_meta.ctrs(i)
  //     io.f3_resp(w).tage_meta.us(i)         := tables(i).io.f3_resp(w).tage_meta.us(i)
  //     io.f3_resp(w).tage_meta.hits(i)       := pred_val
  //     io.f3_resp(w).tage_meta.alt_matches   := !(tables(i).io.f3_resp(w).taken ^ altpred)

  //     altpred = Mux(pred_val, tables(i).io.f3_resp(w).taken, altpred)
  //   }
  // }

  // when (io.update.valid) {
  //   for (w <- 0 until bankWidth) {
  //     val update_useful = !io.update.bits.tage_meta(w).alt_matches
  //   }
  // }

}
