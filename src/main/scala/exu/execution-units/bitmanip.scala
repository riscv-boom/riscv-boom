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
    def FN_PACK  = 17.U(SZ_BITMANIP_FN.W)
}

import BITMANIP._

class BITMANIP(implicit p: Parameters) extends CoreModule()(p) {
  val io = IO(new Bundle { //wrap IO 
    val dw = Input(UInt(SZ_DW.W))
    val fn = Input(UInt(SZ_BITMANIP_FN.W))
    val in2 = Input(UInt(xLen.W))
    val in1 = Input(UInt(xLen.W))
    val out = Output(UInt(xLen.W))
  })
    val andn = io.in1 & ~io.in2
    val orn = io.in1 | ~io.in2
    val xnor = ~(io.in1 ^ io.in2)

    //clz, clzw
    val leading_reverse = Reverse(io.in1)
    val clz_msb =  PriorityEncoder(leading_reverse(63,32))
    val clz_lsb =  PriorityEncoder(leading_reverse(31,0))

    val clzw =  Mux(clz_msb === 31.U && io.in1(0) === 0.U, 32.U, clz_msb)
    val clz_lsb_res = Mux(clz_lsb === 31.U && io.in1(32) === 0.U, 32.U, clz_lsb)
    val clz = Mux(clz_lsb_res === 32.U, clzw +& clz_lsb_res, clz_lsb_res)

    //ctz, ctzw
    val ctz_msb =  PriorityEncoder(io.in1(63,32))
    val ctz_lsb =  PriorityEncoder(io.in1(31,0))
    val ctzw =  Mux(ctz_lsb === 31.U && io.in1(31) === 0.U, 32.U, ctz_lsb)
    val ctz_msb_res = Mux(ctz_msb === 31.U && io.in1(63) === 0.U, 32.U, ctz_msb)
    val ctz = Mux(ctzw === 32.U, ctzw +& ctz_msb_res, ctzw)

    val pcnt = PopCount(io.in1) //do math
    val pcntw = PopCount(io.in1(31,0))

    //try single comparator sign and unsigned
    val max = Mux(io.in1.asSInt < io.in2.asSInt, io.in2, io.in1)
    val maxu = Mux(io.in1 < io.in2, io.in2, io.in1)
    val min = Mux(io.in1.asSInt < io.in2.asSInt, io.in1, io.in2)
    val minu = Mux(io.in1 < io.in2, io.in1, io.in2)

    val sextb = Cat(Fill(56, io.in1(7)),io.in1(7,0))
    val sexth = Cat(Fill(48, io.in1(15)),io.in1(15,0))
    val zexth = Cat(Fill(48, "b0".U),io.in1(15,0))

    val mask = (1.U << 32) - 1.U
    val in1_mask = (io.in1 & mask)
    // rol
    val shamt_rol = io.in2(5,0)
    val left_rol = (io.in1 << shamt_rol)
    val power_rol = (1.U << shamt_rol) - 1.U
    val right_rol = ((io.in1 >> (64.U - shamt_rol) & power_rol)) 
    val rol = left_rol | right_rol

    //rolw
    val shamt_rolw = io.in2(4,0)
    val left_rolw = (in1_mask << shamt_rolw)
    val power_rolw = (1.U << shamt_rolw) - 1.U
    val right_rolw = ((in1_mask >> (32.U - shamt_rolw) & power_rolw)) 
    val rolw = (left_rolw | right_rolw) & mask

    // ror, rori
    val shamt_ror = io.in2(5,0)
    val ror = (io.in1 >> shamt_ror) | (io.in1 << (64.U - shamt_ror))

    // rorw, roriw
    val shamt_rorw = io.in2(4,0)
    val rorw = ((in1_mask >> shamt_rorw) | (in1_mask << (32.U - shamt_rorw))) & mask

    val orcb0 = Mux(io.in1(7,0) === "b_0000_0000".U, "b_0000_0000".U, "b_1111_1111".U)
    val orcb1 = Mux(io.in1(15,8) === "b_0000_0000".U, "b_0000_0000".U, "b_1111_1111".U)
    val orcb2 = Mux(io.in1(23,16) === "b_0000_0000".U, "b_0000_0000".U, "b_1111_1111".U)
    val orcb3 = Mux(io.in1(31,24) === "b_0000_0000".U, "b_0000_0000".U, "b_1111_1111".U)
    val orcb4 = Mux(io.in1(39,32) === "b_0000_0000".U, "b_0000_0000".U, "b_1111_1111".U)
    val orcb5 = Mux(io.in1(47,40) === "b_0000_0000".U, "b_0000_0000".U, "b_1111_1111".U)
    val orcb6 = Mux(io.in1(55,48) === "b_0000_0000".U, "b_0000_0000".U, "b_1111_1111".U)
    val orcb7 = Mux(io.in1(63,56) === "b_0000_0000".U, "b_0000_0000".U, "b_1111_1111".U)
    val orb = Cat(orcb7, orcb6, orcb5, orcb4, orcb3, orcb2, orcb1, orcb0)

    val rev0 = io.in1(7,0)
    val rev1 = io.in1(15,8)
    val rev2 = io.in1(23,16)
    val rev3 = io.in1(31,24)
    val rev4 = io.in1(39,32)
    val rev5 = io.in1(47,40)
    val rev6 = io.in1(55,48)
    val rev7 = io.in1(63,56)
    val rev8 = Cat(rev0, rev1, rev2, rev3, rev4, rev5, rev6, rev7)

    val pack = Cat(io.in2(31,0), io.in1(31,0))

    io.out := MuxCase("h_dead_beef_dead_beef".U, 
                Array((io.fn === 0.U)                   -> andn,
                      (io.fn === 1.U)                   -> orn, 
                      (io.fn === 2.U)                   -> xnor,
                      (io.fn === 3.U && io.dw === 1.U)  -> clz,
                      (io.fn === 3.U && io.dw === 0.U)  -> clzw,
                      (io.fn === 4.U && io.dw === 1.U)  -> ctz,
                      (io.fn === 4.U && io.dw === 0.U)  -> ctzw,
                      (io.fn === 5.U && io.dw === 1.U)  -> pcnt,
                      (io.fn === 5.U && io.dw === 0.U)  -> pcntw,
                      (io.fn === 6.U && io.dw === 1.U)  -> max,
                      (io.fn === 7.U && io.dw === 1.U)  -> maxu,
                      (io.fn === 8.U && io.dw === 1.U)  -> min,
                      (io.fn === 9.U && io.dw === 1.U)  -> minu,
                      (io.fn === 10.U && io.dw === 1.U)  -> sextb,
                      (io.fn === 11.U && io.dw === 1.U)  -> sexth,
                      (io.fn === 12.U && io.dw === 1.U)  -> zexth,
                      (io.fn === 13.U && io.dw === 1.U)  -> rol,
                      (io.fn === 13.U && io.dw === 0.U)  -> rolw,
                      (io.fn === 14.U && io.dw === 1.U)  -> ror, //ror and rori same
                      (io.fn === 14.U && io.dw === 0.U)  -> rorw, //rorw and roriw same
                      (io.fn === 15.U && io.dw === 1.U)  -> orb,
                      (io.fn === 16.U && io.dw === 1.U)  -> rev8,
                      (io.fn === 17.U && io.dw === 1.U)  -> pack)
                )
}
