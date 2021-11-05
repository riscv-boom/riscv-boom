package boom.exu

// import scala.collection.mutable.{ArrayBuffer}

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.tile.{XLen}
import freechips.rocketchip.tile
import freechips.rocketchip.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.CoreModule

import boom.common._
import boom.util._

object BITMANIP {
    val SZ_BITMANIP_FN = 5
    def FN_X    = BitPat("b????")
    def FN_ANDN = 0.U(SZ_BITMANIP_FN.W) // had to add .W or else it complains
    def FN_ORN  = 1.U(SZ_BITMANIP_FN.W)
    def FN_XNOR = 2.U(SZ_BITMANIP_FN.W)

    def FN_CLZ   = 3.U(SZ_BITMANIP_FN.W)
    def FN_CTZ   = 4.U(SZ_BITMANIP_FN.W)

    def FN_PCNT  = 5.U(SZ_BITMANIP_FN.W)

    def FN_MAX   = 6.U(SZ_BITMANIP_FN.W)
    def FN_MAXU  = 7.U(SZ_BITMANIP_FN.W)
    def FN_MIN   = 8.U(SZ_BITMANIP_FN.W)
    def FN_MINU  = 9.U(SZ_BITMANIP_FN.W)

    def FN_SEXTB = 10.U(SZ_BITMANIP_FN.W)
    def FN_SEXTH = 11.U(SZ_BITMANIP_FN.W)
    def FN_ZEXTH = 12.U(SZ_BITMANIP_FN.W)

    def FN_ROL   = 13.U(SZ_BITMANIP_FN.W)
    def FN_ROR   = 14.U(SZ_BITMANIP_FN.W)

    def FN_ORCB  = 15.U(SZ_BITMANIP_FN.W)

    def FN_REV8  = 16.U(SZ_BITMANIP_FN.W)
    // pack
}

import BITMANIP._

class BITMANIP(implicit p: Parameters) extends CoreModule()(p) {
  val io = IO(new Bundle { //wrap IO 
    // val dw = Bits(INPUT, SZ_DW) 
    val dw = Input(UInt(SZ_DW.W))
    // val fn = Bits(INPUT, SZ_BITMANIP_FN)
    val fn = Input(UInt(SZ_BITMANIP_FN.W))
    //val in2 = UInt(INPUT, xLen)
    val in2 = Input(UInt(xLen.W))
    //val in1 = UInt(INPUT, xLen)
    val in1 = Input(UInt(xLen.W))
    //val out = UInt(OUTPUT, xLen)
    val out = Output(UInt(xLen.W))
    //val adder_out = UInt(OUTPUT, xLen)
    //val cmp_out = Bool(OUTPUT)
  })
  io.out := io.in1 & io.in2
  //do all the calculations~~~
}