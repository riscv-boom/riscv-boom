//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV 2bc-gskew Branch Predictor
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2016 Sep 26
//
// ABOUT:
//    2bc-gskew is 4 bimodal tables:
//       - bimodal table (bimo)
//       - gshare  table (gsh0)
//       - gshare  table (gsh1)
//       - meta    table (meta)
//
//    Each table has its own PC/history index hash function.
//    META chooses between BIMO table and majority vote of (bimo,gsh0,gsh1) tables.

package boom

import Chisel._
import freechips.rocketchip.config.{Parameters, Field}

case class GSkewParameters(
   enabled: Boolean = true,
   history_length: Int = 21,
   bimo_num_entries: Int = 16*1024,
   gsh0_num_entries: Int = 64*1024,
   gsh1_num_entries: Int = 64*1024,
   meta_num_entries: Int = 64*1024,
   dualported: Boolean = false,
   // Enable meta predictor to choose between bimodal and gskew majority vote.
   enable_meta: Boolean = true
   )

class GSkewResp(fetch_width: Int, bi_idx_sz: Int, g0_idx_sz: Int, g1_idx_sz: Int, me_idx_sz: Int) extends Bundle
{
   // needed to update predictor at Commit
   val bimo_index = UInt(width = bi_idx_sz)
   val gsh0_index = UInt(width = g0_idx_sz)
   val gsh1_index = UInt(width = g1_idx_sz)
   val meta_index = UInt(width = me_idx_sz)

   val bimo = UInt(width = fetch_width)
   val gsh0 = UInt(width = fetch_width)
   val gsh1 = UInt(width = fetch_width)
   val meta = UInt(width = fetch_width)
   override def cloneType: this.type = new GSkewResp(
      fetch_width, bi_idx_sz, g0_idx_sz, g1_idx_sz, me_idx_sz).asInstanceOf[this.type]
}

object GSkewBrPredictor
{
   def GetRespInfoSize(p: Parameters, fetchWidth: Int): Int =
   {
      val params = p(BoomKey).gskew.get
      val dummy = new GSkewResp(
         fetchWidth,
         log2Up(params.bimo_num_entries),
         log2Up(params.gsh0_num_entries),
         log2Up(params.gsh1_num_entries),
         log2Up(params.meta_num_entries)
         )
      dummy.getWidth
   }
}

class GSkewBrPredictor(fetch_width: Int,
                        history_length: Int = 12,
                        dualported: Boolean = false,
                        enable_meta: Boolean = false
   )(implicit p: Parameters) extends BrPredictor(fetch_width, history_length)(p)
{
   val params = p(BoomKey).gskew.get
   val bimo_num_entries = params.bimo_num_entries
   val gsh0_num_entries = params.gsh0_num_entries
   val gsh1_num_entries = params.gsh1_num_entries
   val meta_num_entries = params.meta_num_entries
   val bimo_idx_sz      = log2Up(bimo_num_entries)
   val gsh0_idx_sz      = log2Up(gsh0_num_entries)
   val gsh1_idx_sz      = log2Up(gsh1_num_entries)
   val meta_idx_sz      = log2Up(meta_num_entries)

   val total_num_entries = bimo_num_entries + gsh0_num_entries + gsh1_num_entries + meta_num_entries

   println ("\tBuilding (" + (total_num_entries * fetch_width * 2/8/1024) + " kB) GSkew Predictor" +
      ", with " + history_length + " bits of history for (" +
      fetch_width + "-wide fetch) and:\n" +
      "\t\t" + bimo_num_entries + " BIM  entries\n" +
      "\t\t" + gsh0_num_entries + " G0   entries\n" +
      "\t\t" + gsh1_num_entries + " G1   entries\n" +
      (if (enable_meta) ("\t\t" + meta_num_entries + " META entries") else ("\t\tNo meta.")))

   //------------------------------------------------------------
   private val shamt = log2Up(fetch_width*coreInstBytes)

   private def Fold (input: UInt, compressed_length: Int) =
   {
      val clen = compressed_length
      val hlen = history_length
      if (hlen <= clen)
      {
         input
      }
      else
      {
         var res = UInt(0,clen)
         var remaining = input.toUInt
         for (i <- 0 to hlen-1 by clen)
         {
            val len = if (i + clen > hlen ) (hlen - i) else clen
            require(len > 0)
            res = res(clen-1,0) ^ remaining(len-1,0)
            remaining = remaining >> UInt(len)
         }
         res
      }
   }

   private def BimoIdxHash (addr: UInt, h: UInt, w: Int) =
   {
      //(addr >> UInt(shamt)) ^ Cat(hist(3,0), UInt(0,2))
      val a = addr >> UInt(shamt)
      val z      = UInt(0,32) // unused - a from two cycles ago.

      val i10_5  = Cat(h(3,0), a(8,7)) // word-line address
      val i13_11 = Cat(a(11), a(9)^a(5), a(10)^a(6))
      val i4_2   = Cat(a(4) , a(3)^z(6), a( 2)^z(5))
      val i1_0   = a(1,0) // bank selector; should actually be using y,z.
      val ret    = Cat(i13_11, i10_5, i4_2, i1_0)
      require (ret.getWidth >= w) // make sure hash idx can cover entire table
      ret
   }

   private def Gsh0IdxHash (addr: UInt, h: UInt, w: Int) =
   {
      val a = addr >> UInt(shamt)
      val z      = UInt(0,32) // unused - a from two cycles ago.
//      (addr >> UInt(shamt)) ^ Cat(hist, UInt(0,1))
      val i15_11 = Cat(h(7)^h(11), h(8)^h(12), h(4)^h(5), a(9)^h(9), h(10)^h(6))
      val i10_5  = Cat(h(3,0), a(8,7)) // word-line address
      val i4_2   = Cat(a(4)^a(9)^a(13)^a(12)^h(5)^h(11)^h(8)^z(5),
                       a(3)^a(11)^h(9)^h(10)^h(12)^z(6)^z(5),
                       a(2)^a(14)^a(10)^h(6)^h(4)^h(7)^a(6))
      val i1_0   = a(1,0) // bank selector; should actually be using y,z.
      val ret    = Cat(i15_11, i10_5, i4_2, i1_0)
      require (ret.getWidth >= w) // make sure hash idx can cover entire table
      ret
   }

   private def Gsh1IdxHash (addr: UInt, h: UInt, w: Int) =
   {
      val a = addr >> UInt(shamt)
      val z      = UInt(0,32) // unused - a from two cycles ago.
//      (addr >> UInt(shamt)) ^ Fold(hist, gsh1_idx_sz)
      val i15_11 = Cat(h(19)^h(12), h(18)^h(11), h(17)^h(10), h(16)^h(4), h(15)^h(20))
      val i10_5  = Cat(h(3,0), a(8,7)) // word-line address
      val i4_2   = Cat(a(4)^a(11)^a(14)^a(6)^h(4)^h(6)^h(9)^h(14)^h(15)^h(16)^z(6),
                       a(3)^a(10)^a(13)^h(5)^h(11)^h(13)^h(18)^h(19)^h(20)^z(5),
                       a(2)^a(5)^a(9)^h(4)^h(8)^h(7)^h(10)^h(12)^h(13)^h(14)^h(17))
      val i1_0   = a(1,0) // bank selector; should actually be using y,z.
      val ret    = Cat(i15_11, i10_5, i4_2, i1_0)
      require (ret.getWidth >= w) // make sure hash idx can cover entire table
      ret
   }

   private def MetaIdxHash (addr: UInt, h: UInt, w: Int) =
   {
      //val hlen = Seq(history_length, 15).max
      //(addr >> UInt(shamt)) ^ Fold(hist(hlen-1,0), meta_idx_sz)
      val a = addr >> UInt(shamt)
      val z      = UInt(0,32) // unused - a from two cycles ago.
      val i15_11 = Cat(h(7)^h(11), h(8)^h(12), h(5)^h(13), h(4)^h(9), a(9)^h(6))
      val i10_5  = Cat(h(3,0), a(8,7)) // word-line address
      val i4_2   = Cat(a(4)^a(10)^a(5)^h(7)^h(10)^h(14)^h(13)^z(5),
                       a(3)^a(12)^a(14)^a(6)^h(4)^h(6)^h(8)^h(14),
                       a(2)^a(9)^a(11)^a(13)^h(5)^h(9)^h(11)^h(12)^z(6))
      val i1_0   = a(1,0) // bank selector; should actually be using y,z.
      val ret    = Cat(i15_11, i10_5, i4_2, i1_0)
      require (ret.getWidth >= w) // make sure hash idx can cover entire table
      ret
   }


   //------------------------------------------------------------
   // Predictor state.

   val bimo_table = Module(new TwobcCounterTable(fetch_width, bimo_num_entries, dualported))
   val gsh0_table = Module(new TwobcCounterTable(fetch_width, gsh0_num_entries, dualported))
   val gsh1_table = Module(new TwobcCounterTable(fetch_width, gsh1_num_entries, dualported))
   val meta_table = Module(new TwobcCounterTable(fetch_width, meta_num_entries, dualported))


   //------------------------------------------------------------
   // Get prediction.

   val bimo_out      = Wire(UInt())
   val gsh0_out      = Wire(UInt())
   val gsh1_out      = Wire(UInt())
   val meta_out      = Wire(UInt())

   val stall = !io.resp.ready
   bimo_table.io.stall := stall
   gsh0_table.io.stall := stall
   gsh1_table.io.stall := stall
   meta_table.io.stall := stall

   val s1_pc = io.req_pc
   val bimo_idx = BimoIdxHash(s1_pc, this.ghistory, bimo_idx_sz)
   val gsh0_idx = Gsh0IdxHash(s1_pc, this.ghistory, gsh0_idx_sz)
   val gsh1_idx = Gsh1IdxHash(s1_pc, this.ghistory, gsh1_idx_sz)
   val meta_idx = MetaIdxHash(s1_pc, this.ghistory, meta_idx_sz)
   bimo_table.io.s1_r_idx := bimo_idx
   gsh0_table.io.s1_r_idx := gsh0_idx
   gsh1_table.io.s1_r_idx := gsh1_idx
   meta_table.io.s1_r_idx := meta_idx
   bimo_out  := bimo_table.io.s2_r_out
   gsh0_out  := gsh0_table.io.s2_r_out
   gsh1_out  := gsh1_table.io.s2_r_out
   meta_out  := meta_table.io.s2_r_out

   val resp_info = Wire(new GSkewResp(fetch_width, bimo_idx_sz, gsh0_idx_sz, gsh1_idx_sz, meta_idx_sz))

   val takens = Wire(Vec(fetch_width, Bool()))

   for (i <- 0 until fetch_width)
   {
      val bim = bimo_out(i)
      val g0  = gsh0_out(i)
      val g1  = gsh1_out(i)
      val meta= meta_out(i)

      val vote = PopCount(bim :: g0 :: g1 :: Nil)
      if (enable_meta)
      {
         takens(i) := Mux(meta, vote > UInt(1), bim)
      }
      else
      {
         takens(i) := vote > UInt(1)
      }
   }

   io.resp.bits.takens := takens.asUInt

   resp_info.bimo_index := RegNext(bimo_idx)
   resp_info.gsh0_index := RegNext(gsh0_idx)
   resp_info.gsh1_index := RegNext(gsh1_idx)
   resp_info.meta_index := RegNext(meta_idx)
   resp_info.bimo       := bimo_out
   resp_info.gsh0       := gsh0_out
   resp_info.gsh1       := gsh1_out
   resp_info.meta       := meta_out
   io.resp.bits.info := resp_info.asUInt
   // Always overrule the BTB, which will almost certainly have less history.
   io.resp.valid := Bool(true)

   require (coreInstBytes == 4)

   //------------------------------------------------------------

   // decide for each bit whether we should:
   // - update it
   // - and in which direction.
   val bimo_update_valids = Wire(Vec(fetch_width, Bool()))
   val gsh0_update_valids = Wire(Vec(fetch_width, Bool()))
   val gsh1_update_valids = Wire(Vec(fetch_width, Bool()))
   val meta_update_valids = Wire(Vec(fetch_width, Bool()))
   bimo_update_valids.map(_ := Bool(false))
   gsh0_update_valids.map(_ := Bool(false))
   gsh1_update_valids.map(_ := Bool(false))
   meta_update_valids.map(_ := Bool(false))

   val bimo_update_mispredicted = Wire(Vec(fetch_width, Bool()))
   val gsh0_update_mispredicted = Wire(Vec(fetch_width, Bool()))
   val gsh1_update_mispredicted = Wire(Vec(fetch_width, Bool()))
   val meta_update_mispredicted = Wire(Vec(fetch_width, Bool()))
   bimo_update_mispredicted.map(_ := Bool(false))
   gsh0_update_mispredicted.map(_ := Bool(false))
   gsh1_update_mispredicted.map(_ := Bool(false))
   meta_update_mispredicted.map(_ := Bool(false))

   val meta_update_dir = Wire(Vec(fetch_width, Bool()))
   meta_update_dir.map(_ := Bool(false))

   val com_info = new GSkewResp(
      fetch_width, bimo_idx_sz, gsh0_idx_sz, gsh1_idx_sz, meta_idx_sz).fromBits(commit.bits.info.info)

   // Was the predictor correct?
   val correct = !commit.bits.ctrl.mispredicted.reduce(_|_)
   // Did the BIM and the Triumvirate agree?
   val com_vote = Vec(for (i <- 0 until fetch_width) yield
   {
      val cnt = PopCount(com_info.bimo(i) :: com_info.gsh0(i) :: com_info.gsh1(i) :: Nil)
       cnt > UInt(1)
   }).asUInt
   val both_agree  = ~(com_vote ^ com_info.bimo)
   val bim_mispredicted  = commit.bits.ctrl.taken.asUInt ^ com_info.bimo
   val g0_mispredicted   = commit.bits.ctrl.taken.asUInt ^ com_info.gsh0
   val g1_mispredicted   = commit.bits.ctrl.taken.asUInt ^ com_info.gsh1
   val meta_mispredicted = Wire(init = Bool(false))

   when (commit.valid && commit.bits.ctrl.executed.reduce(_|_))
   {
      for (i <- 0 until fetch_width)
      {
         if (enable_meta)
         {
            when (correct && both_agree(i))
            {
               // do nothing
            }
            .elsewhen (correct && !both_agree(i))
            {
               // strengthen meta
               meta_update_valids(i) := Bool(true)
               meta_update_dir(i) := com_info.meta(i)

               // strengthen only those that gave correct predictions.
               when (!bim_mispredicted(i)) { bimo_update_valids(i) := Bool(true) }
               when (!g0_mispredicted(i))  { gsh0_update_valids(i) := Bool(true) }
               when (!g1_mispredicted(i))  { gsh1_update_valids(i) := Bool(true) }
            }
            .elsewhen (!correct && !both_agree(i))
            {
               // 1. update meta to choose differently
               meta_update_valids(i) := Bool(true)
               meta_update_dir(i) := !com_info.meta(i)
               meta_mispredicted := Bool(true)

               // 2. check new prediction...
               //    - strengthen participating if correct
               //    - update all if incorrect.

               // Note: ideally, we'd know the new meta, but that requires reading the hystersis bit.
               // So instead we'll just invert the meta prediction (aka, assume the meta was weak).
               //bool new_meta = (meta[me_idx] >> 1) & 0x1
               //bool new_pred = new_meta ? last_bim : ((last_bim + last_g0 + last_g1) > 1)
               val new_meta = !com_info.meta(i)
               val new_pred = Mux(new_meta, com_vote, com_info.bimo(i))

               when (new_pred === commit.bits.ctrl.taken(i))
               {
                  when (new_meta)
                  {
                     bimo_update_valids(i) := Bool(true)
                     gsh0_update_valids(i) := Bool(true)
                     gsh1_update_valids(i) := Bool(true)
                  }
                  .otherwise
                  {
                     bimo_update_valids(i) := Bool(true)
                  }

               }
            }
            .otherwise
            {
               // !correct && both agree

               // update all of the things
               bimo_update_valids(i) := Bool(true)
               gsh0_update_valids(i) := Bool(true)
               gsh1_update_valids(i) := Bool(true)
            }
         }
         else
         {
            // no meta, just pure gskew
            when (correct)
            {
               // strengthen only those that gave correct predictions.
               when (!bim_mispredicted(i)) { bimo_update_valids(i) := Bool(true) }
               when (!g0_mispredicted(i))  { gsh0_update_valids(i) := Bool(true) }
               when (!g1_mispredicted(i))  { gsh1_update_valids(i) := Bool(true) }
            }
            .otherwise
            {
               // update all of the things
               bimo_update_valids(i) := Bool(true)
               gsh0_update_valids(i) := Bool(true)
               gsh1_update_valids(i) := Bool(true)
            }
         }
      }
   }

   //------------------------------------------------------------
   // prediction response information
   val commit_info = new GSkewResp(
      fetch_width, bimo_idx_sz, gsh0_idx_sz, gsh1_idx_sz, meta_idx_sz).fromBits(commit.bits.info.info)

   bimo_table.io.update.valid          := bimo_update_valids.reduce(_|_)
   gsh0_table.io.update.valid          := gsh0_update_valids.reduce(_|_)
   gsh1_table.io.update.valid          := gsh1_update_valids.reduce(_|_)

   if (enable_meta)
   {
      meta_table.io.update.valid          := meta_update_valids.reduce(_|_)
   }
   else
   {
      meta_table.io.update.valid          := Bool(false)
   }

   bimo_table.io.update.bits.was_mispredicted := bim_mispredicted.orR
   gsh0_table.io.update.bits.was_mispredicted := g0_mispredicted.orR
   gsh1_table.io.update.bits.was_mispredicted := g1_mispredicted.orR
   meta_table.io.update.bits.was_mispredicted := meta_mispredicted

   bimo_table.io.update.bits.index     := commit_info.bimo_index
   gsh0_table.io.update.bits.index     := commit_info.gsh0_index
   gsh1_table.io.update.bits.index     := commit_info.gsh1_index
   meta_table.io.update.bits.index     := commit_info.meta_index

   bimo_table.io.update.bits.executed  := commit.bits.ctrl.executed zip bimo_update_valids map {case(e,v) => e & v}
   gsh0_table.io.update.bits.executed  := commit.bits.ctrl.executed zip gsh0_update_valids map {case(e,v) => e & v}
   gsh1_table.io.update.bits.executed  := commit.bits.ctrl.executed zip gsh1_update_valids map {case(e,v) => e & v}
   meta_table.io.update.bits.executed  := commit.bits.ctrl.executed zip meta_update_valids map {case(e,v) => e & v}

   bimo_table.io.update.bits.takens    := commit.bits.ctrl.taken
   gsh0_table.io.update.bits.takens    := commit.bits.ctrl.taken
   gsh1_table.io.update.bits.takens    := commit.bits.ctrl.taken
   meta_table.io.update.bits.takens    := meta_update_dir

   bimo_table.io.update.bits.do_initialize := Bool(false)
   gsh0_table.io.update.bits.do_initialize := Bool(false)
   gsh1_table.io.update.bits.do_initialize := Bool(false)
   meta_table.io.update.bits.do_initialize := Bool(false)

   //------------------------------------------------------------
}

