package boom.vmu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._
import chisel3.internal.sourceinfo.SourceInfo

import boom.common._

class BoomVecMemUnit(hartid: Int)(implicit p: Parameters) extends LazyModule
// with HasBoomCoreParameters breaks TL here
{
   val numVMUEntries = p(TileKey).core.asInstanceOf[BoomCoreParams].numVMUEntries

   lazy val module = new BoomVecMemUnitModule(this, hartid)
   //val module: BoomVecMemUnitModule
   val node = TLClientNode(Seq(TLClientPortParameters(Seq(TLClientParameters(
      sourceId = IdRange(0, numVMUEntries << 1), // What is this?
      name = s"Core ${hartid} Vector Memory Port")))))
}

class BoomVecMemIO(implicit p: Parameters) extends BoomBundle()(p)
{
   val memreq_val   = Input(Bool())
   val memreq_addr  = Input(UInt(width=corePAddrBits.W))
   val memreq_wdata = Input(UInt(width=vecStripLen.W))
   val memreq_mask  = Input(UInt(width=(vecStripLen/8).W))
   val memreq_uop   = Input(new MicroOp())
   val memreq_tag   = Input(UInt(width=vmuEntrySz.W))
   val memreq_ready = Output(Bool())

   val memresp_val  = Output(Bool())
   val memresp_data = Output(UInt(width=vecStripLen.W))
   val memresp_tag  = Output(UInt(width=vmuEntrySz.W))
   val memresp_store= Output(Bool())
}

class BoomVecMemUnitModule(outer: BoomVecMemUnit, hartid: Int) extends LazyModuleImp(outer)
   with HasBoomCoreParameters
{
   val io = IO(new BoomVecMemIO())
   val node = outer.node

   val (dmem, edge) = outer.node.out.head

   io.memreq_ready := true.B
   dmem.a.valid    := io.memreq_val
   assert(!(io.memreq_uop.is_store && io.memreq_uop.is_load && io.memreq_val))

   val req_size = log2Up(vecStripLen/8).U
   val req_addr_beat_aligned = (io.memreq_addr >> req_size) << req_size

   dmem.a.bits := Mux(
      io.memreq_uop.is_load,
      edge.Get(io.memreq_tag, req_addr_beat_aligned, req_size)._2,
      edge.Put(io.memreq_tag, req_addr_beat_aligned, req_size, io.memreq_wdata, io.memreq_mask)._2)

   val resp_en = edge.hasData(dmem.d.bits) || io.memresp_store
   dmem.d.ready := !resp_en

   io.memresp_val    := dmem.d.valid && resp_en
   io.memresp_tag    := dmem.d.bits.source(vmuEntrySz,0)
   io.memresp_data   := dmem.d.bits.data
   io.memresp_store  := dmem.d.bits.opcode === TLMessages.AccessAck

   dmem.b.ready := true.B
   dmem.c.valid := false.B
   dmem.e.valid := false.B
}

/** Mix-ins for a separate vector memory port to L2 */
trait HasBoomVecMemUnit extends HasTileParameters { this: BaseTile =>
   val module: HasBoomVecMemUnitModule
   val vec_mem = LazyModule(new BoomVecMemUnit(hartId))
   tlMasterXbar.node := vec_mem.node

   // No PTW ports necessary, since the LSU walks the PT for us
}

trait HasBoomVecMemUnitModule {
   val outer: HasBoomVecMemUnit
   // What is this ???
}


