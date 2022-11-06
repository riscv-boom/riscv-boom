//******************************************************************************
// Copyright (c) 2015 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Rename Map Table
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._
import boom.common._
import boom.util._
import freechips.rocketchip.config.Parameters

// 获得当前请求的逻辑寄存器号
class MapReq(val lregSz: Int) extends Bundle
{
  val lrs1 = UInt(lregSz.W)
  val lrs2 = UInt(lregSz.W)
  val lrs3 = UInt(lregSz.W)
  val ldst = UInt(lregSz.W)
}
// 物理寄存器号的回复
// stale pdst：写回寄存器
class MapResp(val pregSz: Int) extends Bundle
{
  val prs1 = UInt(pregSz.W)
  val prs2 = UInt(pregSz.W)
  val prs3 = UInt(pregSz.W)
  val stale_pdst = UInt(pregSz.W)
}

// 重命名请求
class RemapReq(val lregSz: Int, val pregSz: Int) extends Bundle
{
  val ldst = UInt(lregSz.W)
  val pdst = UInt(pregSz.W)
  val valid = Bool()
}

// float : is it a float register?
class RenameMapTable(
  val plWidth: Int,
  val numLregs: Int,
  val numPregs: Int,
  val bypass: Boolean,
  val float: Boolean)
  (implicit p: Parameters) extends BoomModule
{
  val pregSz = log2Ceil(numPregs)

  val io = IO(new BoomBundle()(p) {
    // Logical sources -> physical sources.
    val map_reqs    = Input(Vec(plWidth, new MapReq(lregSz)))
    val map_resps   = Output(Vec(plWidth, new MapResp(pregSz)))

    // Remapping an ldst to a newly allocated pdst?
    val remap_reqs  = Input(Vec(plWidth, new RemapReq(lregSz, pregSz)))

    // Dispatching branches: need to take snapshots of table state.
    val ren_br_tags = Input(Vec(plWidth, Valid(UInt(brTagSz.W))))

    // Signals for restoring state following misspeculation.
    val brupdate      = Input(new BrUpdateInfo)
    val rollback    = Input(Bool())
  })

  // The map table register array and its branch snapshots.
  // numLregs: 逻辑寄存器的个数
  // pregSz: 物理寄存器索引需要的二进制位数
  // 每一个逻辑寄存器与一个物理寄存器索引对应
  val map_table = RegInit(VecInit(Seq.fill(numLregs){0.U(pregSz.W)}))
  val br_snapshots = Reg(Vec(maxBrCount, Vec(numLregs, UInt(pregSz.W))))

  // The intermediate states of the map table following modification by each pipeline slot.
  val remap_table = Wire(Vec(plWidth+1, Vec(numLregs, UInt(pregSz.W))))

  // Uops requesting changes to the map table.
  // 当前阶段被重命名的目的寄存器映射得到的物理寄存器号
  val remap_pdsts = io.remap_reqs map (_.pdst)
  val remap_ldsts_oh = io.remap_reqs map (req => UIntToOH(req.ldst) & Fill(numLregs, req.valid.asUInt))

  // Figure out the new mappings seen by each pipeline slot.
  for (i <- 0 until numLregs) {
    if (i == 0 && !float) {
      for (j <- 0 until plWidth+1) {
        remap_table(j)(i) := 0.U
      }
    } else {
      // 第i个逻辑寄存器在每个slot中的指令重命名后对应新的状态
      // 得到一个长度为plWidth+1的物理寄存器序列（第i个逻辑寄存器的映射变化）
      // (ldst, new_pdst) ： ldst(i), remap_pdsts
      val remapped_row = (remap_ldsts_oh.map(ldst => ldst(i)) zip remap_pdsts)
        .scanLeft(map_table(i)) {case (pdst, (ldst, new_pdst)) => Mux(ldst, new_pdst, pdst)}

      for (j <- 0 until plWidth+1) {
        remap_table(j)(i) := remapped_row(j)
      }
    }
  }

  // Create snapshots of new mappings.
  // 如果第i条指令的br_tag有效 则记录第i条指令重命名后的重命名表状态
  for (i <- 0 until plWidth) {
    when (io.ren_br_tags(i).valid) {
      br_snapshots(io.ren_br_tags(i).bits) := remap_table(i+1)
    }
  }
  // 分支预测错误 使用snapshot
  when (io.brupdate.b2.mispredict) {
    // Restore the map table to a branch snapshot.
    map_table := br_snapshots(io.brupdate.b2.uop.br_tag)
  } .otherwise {
    // Update mappings.
    map_table := remap_table(plWidth)
  }
  // 同一个周期前面指令可能已经为ldst分配了新的物理寄存器，但此时map_table并没有更新
  /*
  对于上述mapping代码而言，首先是一个for循环，从0到plWidth这些指令需要进行映射。
  接下来对于第i条指令，它的映射结果prs1先赋一个初始值map_table(io.map_reqs(i).lrs1)，
  再从它之前的指令中依次判断是否bypass、该指令是否修改了映射关系、该指令的ldst是否和本指令的lrs1相同，
  如果上述三个条件均满足（即该指令发生了RAW，它的操作数需要映射到最近一条写这个ISA寄存器的指令所映射的物理寄存器），
  则将结果改为该指令的pdst，否则还使用当前值。
  */
  // p: map_table(io.map_reqs(i).lrs1)

  // Read out mappings.
  for (i <- 0 until plWidth) {
    io.map_resps(i).prs1       := (0 until i).foldLeft(map_table(io.map_reqs(i).lrs1)) ((p,k) =>
      Mux(bypass.B && io.remap_reqs(k).valid && io.remap_reqs(k).ldst === io.map_reqs(i).lrs1, io.remap_reqs(k).pdst, p))
    io.map_resps(i).prs2       := (0 until i).foldLeft(map_table(io.map_reqs(i).lrs2)) ((p,k) =>
      Mux(bypass.B && io.remap_reqs(k).valid && io.remap_reqs(k).ldst === io.map_reqs(i).lrs2, io.remap_reqs(k).pdst, p))
    io.map_resps(i).prs3       := (0 until i).foldLeft(map_table(io.map_reqs(i).lrs3)) ((p,k) =>
      Mux(bypass.B && io.remap_reqs(k).valid && io.remap_reqs(k).ldst === io.map_reqs(i).lrs3, io.remap_reqs(k).pdst, p))
    io.map_resps(i).stale_pdst := (0 until i).foldLeft(map_table(io.map_reqs(i).ldst)) ((p,k) =>
      Mux(bypass.B && io.remap_reqs(k).valid && io.remap_reqs(k).ldst === io.map_reqs(i).ldst, io.remap_reqs(k).pdst, p))

    if (!float) io.map_resps(i).prs3 := DontCare
  }

  // Don't flag the creation of duplicate 'p0' mappings during rollback.
  // These cases may occur soon after reset, as all maptable entries are initialized to 'p0'.
  io.remap_reqs map (req => (req.pdst, req.valid)) foreach {case (p,r) =>
    assert (!r || !map_table.contains(p) || p === 0.U && io.rollback, "[maptable] Trying to write a duplicate mapping.")}
}
