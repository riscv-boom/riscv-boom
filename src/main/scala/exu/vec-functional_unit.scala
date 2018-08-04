//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Functional Units
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Hankun Zhao, Christopher Celio
// 2013 Mar 10
//
// If regfile bypassing is disabled, then the functional unit must do its own
// bypassing in here on the WB stage (i.e., bypassing the io.resp.data)

package boom.exu
import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket.ALU._
import freechips.rocketchip.util._
import freechips.rocketchip.tile
import boom.bpu.{BpredType, BranchPredInfo, BoomBTBUpdate}
import boom.common._
import boom.ifu._
import boom.util._

class Adder(w: Int) extends Module {
   val io = IO(new Bundle {
      val in0 = UInt(INPUT, w)
      val in1 = UInt(INPUT, w)
      val sub = Bool(INPUT)
      val out = UInt(OUTPUT, w)
   })
   val in1_inv = Mux(io.sub, ~io.in1, io.in1)
   val bits = io.in0 + in1_inv + io.sub
   io.out := bits
}

class Comparator(w: Int) extends Module {
   val io = IO(new Bundle {
      val in0 = UInt(INPUT, w)
      val in1 = UInt(INPUT, w)
      val slt = Bool(INPUT)
      val sltu = Bool(INPUT)
      val set = Bool(OUTPUT)
   })
   //val neg0, neg1 = (in0(w-1), in1(w-1))
   val neg0 = io.in0(w - 1)
   val neg1 = io.in1(w - 1)
   val ltu = io.in0 < io.in1
   val lt = ((neg0 === neg1) & ltu) | (neg0 & !neg1)
   val eq = io.in0 === io.in1
   io.set := (io.slt & lt) | (io.sltu & ltu)
}


class VALUUnit(num_stages: Int, data_width: Int) (implicit p: Parameters)
      extends PipelinedFunctionalUnit(num_stages = num_stages, // TODO_Vec: Maybe change this when contention for write port is allowed?
         num_bypass_stages = 0,
         earliest_bypass_stage = 0,
         data_width = data_width,
         is_branch_unit = false)(p) with Packing
{
   val uop = io.req.bits.uop
   val imm_xprlen = ImmGen(uop.imm_packed, uop.ctrl.imm_sel)
   require(num_stages > 0, "This should be > 0 for now");
   when (io.req.valid) {
      // printf("VALU received valid input\n");
      // printf("%d %x %x %x\n", uop.uopc, uop.dst_rtype, uop.rd_verep, uop.rd_vshape)
      assert ((io.req.bits.uop.dst_rtype === RT_VEC 
         && io.req.bits.uop.rd_vshape === VSHAPE_VECTOR
         && (io.req.bits.uop.rd_verep === VEREP_FP || io.req.bits.uop.rd_verep === VEREP_INT || io.req.bits.uop.rd_verep === VEREP_UINT))
         || (io.req.bits.uop.dst_rtype === RT_FIX && io.req.bits.uop.uopc === uopVEXTRACT),
         "Destination must be fp or int vector reg\n");
      when (uop.uopc === uopVFSJ || uop.uopc === uopVFSJN || uop.uopc === uopVFSJX) {
         assert (io.req.bits.uop.rd_verep === VEREP_FP,
            "Sign extension ops only valid on fp regs\n")
      }
      when (io.req.bits.uop.uopc =/= uopVEXTRACT && io.req.bits.uop.uopc =/= uopVINSERT) {
         when (io.req.bits.uop.lrs1_rtype =/= RT_X)
         {
            assert(io.req.bits.uop.rs1_vew === io.req.bits.uop.rd_vew,
               "Element width of rs1 does not match")
            assert(io.req.bits.uop.rs1_verep === io.req.bits.uop.rd_verep,
               "Element rep of rs1 does not match")
         }
         when (io.req.bits.uop.lrs2_rtype =/= RT_X)
         {
            assert(io.req.bits.uop.rs2_vew === io.req.bits.uop.rd_vew,
               "Element width of rs2 does not match")
            assert(io.req.bits.uop.rs2_verep === io.req.bits.uop.rd_verep,
               "Element rep of rs2 does not match")
         }
         when (io.req.bits.uop.lrs3_rtype =/= RT_X)
         {
            assert(io.req.bits.uop.rs3_vew === io.req.bits.uop.rd_vew,
               "Element width of rs3 does not match")
            assert(io.req.bits.uop.rs3_verep === io.req.bits.uop.rd_verep,
               "Element rep of rs3 does not match")
         }
      }
   }


   val results =
      List((SZ_D, VEW_64, unpack_d _, repack_d _, 1),
           (SZ_W, VEW_32, unpack_w _, repack_w _, 2),
           (SZ_H, VEW_16, unpack_h _, repack_h _, 3)) map {
         case (sz, ew, unpack, repack, sidx_w) => {
            val n = vecStripLen / sz
            val alu_val = io.req.valid && (io.req.bits.uop.rd_vew === ew || io.req.bits.uop.rs1_vew === ew)
            val eidx_lower = io.req.bits.rs2_data(sidx_w-1,0)
            val strip_vins = (io.req.bits.uop.eidx >> sidx_w) === (io.req.bits.rs2_data >> sidx_w)
            val results = for (i <- (0 until n)) yield {
               val op1 = unpack(io.req.bits.rs1_data, i).asUInt
               val op2 = unpack(io.req.bits.rs2_data, i).asUInt
               val op3 = unpack(io.req.bits.rs3_data, i).asUInt
               val sintop2 = Wire(SInt(width=sz))
               sintop2 := ImmGen(uop.imm_packed, IS_V)
               val adder = Module(new Adder(sz))
               adder.io.in0 := op1
               adder.io.in1 := Mux(uop.uopc === uopVADDI, sintop2.asUInt, op2)
               adder.io.sub := uop.uopc === uopVSUB
               val adder_out = adder.io.out

               val shamt = op1(5, 0).asUInt
               val sra_out = (op2.asSInt >> shamt).asUInt
               val srl_out = op2.asUInt >> shamt
               val sll_out = op2 << shamt
               val comp = Module(new Comparator(sz))
               comp.io.in0 := op1
               comp.io.in1 := op2
               comp.io.slt := uop.uopc === uopVSLT
               comp.io.sltu := uop.uopc === uopVSLTU
               val set_out = comp.io.set

               val vins = Mux(strip_vins && UInt(i) === io.req.bits.rs2_data(sidx_w-1, 0), io.req.bits.rs1_data(sz-1,0), op3)

               val result = Wire(UInt(width=sz))
               result := Mux1H(Array(
                  // TODO_Vec: Wow this seems pretty not optimal
                  (uop.uopc === uopVADD
                || uop.uopc === uopVADDI
                || uop.uopc === uopVSUB)  -> adder_out,
                  (uop.uopc === uopVSLL)  -> sll_out,
                  (uop.uopc === uopVSRL)  -> srl_out,
                  (uop.uopc === uopVSRA)  -> sra_out,
                  (uop.uopc === uopVSLT
                || uop.uopc === uopVSLTU) -> set_out,
                  (uop.uopc === uopVAND)  -> (op1 & op2),
                  (uop.uopc === uopVXOR)  -> (op1 ^ op2),
                  (uop.uopc === uopVOR)   -> (op1 | op2),
                  (uop.uopc === uopVFSJ)  -> Cat(op2(sz-1), op1(sz-2, 0)),
                  (uop.uopc === uopVFSJN) -> Cat(~op2(sz-1), op1(sz-2, 0)),
                  (uop.uopc === uopVFSJX) -> Cat(op2(sz-1) ^ op1(sz-1), op1(sz-2, 0)),
                  (uop.uopc === uopVINSERT)-> vins,
                  (uop.uopc === uopVEXTRACT)-> op1))

               val out = Pipe((alu_val && (uop.uopc =/= uopVEXTRACT || UInt(i) === eidx_lower)),
                  result, num_stages) // TODO_vec: this shouldn't be zero right??
               val out_val = out.valid
               val out_data = out.bits
               (out_val, out_data)
            }
            val result_val = results.map(_._1).reduce(_||_)
            // assert ( result_val === results.map(_._1).reduce(_&&_),
            //    "VALU slice not all responding valid at the same time!")
            val result_out = Wire(UInt(width=vecStripLen))
            when (results.map(_._1).reduce(_&&_)) {
               result_out := repack(results.map(_._2))
            } .otherwise {
               result_out := Mux1H(results.map(_._1), results.map(_._2))
            }
            (result_val, result_out)
         }
      }

   val alumatch = results.map(_._1)
   io.resp.valid := alumatch.reduce(_||_)
   io.resp.bits.data := Mux1H(alumatch, results.map(_._2))

}

class VFPUUnit(data_width: Int)(implicit p: Parameters) extends PipelinedFunctionalUnit(
   num_stages = p(tile.TileKey).core.fpu.get.dfmaLatency,
   num_bypass_stages = 0,
   earliest_bypass_stage = 0,
   data_width = data_width)(p)
{
   val vfpu = Module(new VFPU())
   vfpu.io.req <> io.req

   io.resp.bits.data               := vfpu.io.resp.bits.data
   io.resp.bits.fflags.valid       := vfpu.io.resp.bits.fflags.valid
   io.resp.bits.fflags.bits.uop    := io.resp.bits.uop
   io.resp.bits.fflags.bits.flags  := vfpu.io.resp.bits.fflags.bits.flags // kill me now x2
}
