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
    def FN_X    = BitPat("b?????")
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
  val io = IO(new Bundle {
    val dw = Input(UInt(SZ_DW.W))
    val fn = Input(UInt(SZ_BITMANIP_FN.W))
    val in2 = Input(UInt(xLen.W))
    val in1 = Input(UInt(xLen.W))
    val out = Output(UInt(xLen.W))
  })
    val andn = io.in1 & ~io.in2
    val orn = io.in1 | ~io.in2
    val xnor = ~(io.in1 ^ io.in2)

    val count_operand = Mux(io.fn === FN_CLZ, Reverse(io.in1), io.in1)
    val count_operand_msb = PriorityEncoder(count_operand(63,32))
    val count_operand_lsb = PriorityEncoder(count_operand(31,0))
    //clz, clzw
    val clz_msb =  count_operand_msb
    val clz_lsb =  count_operand_lsb
    val clzw =  Mux(clz_msb === 31.U && io.in1(0) === 0.U, 32.U, clz_msb)
    val clz_lsb_res = Mux(clz_lsb === 31.U && io.in1(32) === 0.U, 32.U, clz_lsb)
    val clz = Mux(clz_lsb_res === 32.U, clzw +& clz_lsb_res, clz_lsb_res)

    //ctz, ctzw
    val ctz_msb =  count_operand_msb
    val ctz_lsb =  count_operand_lsb
    val ctzw =  Mux(ctz_lsb === 31.U && io.in1(31) === 0.U, 32.U, ctz_lsb)
    val ctz_msb_res = Mux(ctz_msb === 31.U && io.in1(63) === 0.U, 32.U, ctz_msb)
    val ctz = Mux(ctzw === 32.U, ctzw +& ctz_msb_res, ctzw)

    // pcnt, pcntw
    val pcnt = PopCount(io.in1)
    val pcntw = PopCount(io.in1(31,0))

    // max, maxu, min, minu
    val max = Mux(io.in1.asSInt < io.in2.asSInt, io.in2, io.in1)
    val maxu = Mux(io.in1 < io.in2, io.in2, io.in1)
    val min = Mux(io.in1.asSInt < io.in2.asSInt, io.in1, io.in2)
    val minu = Mux(io.in1 < io.in2, io.in1, io.in2)

    // sextb, sexth, zexth
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

    //orcb
    val orcb0 = Fill(8, io.in1(7,0).orR)
    val orcb1 = Fill(8, io.in1(15,8).orR)
    val orcb2 = Fill(8, io.in1(23,16).orR)
    val orcb3 = Fill(8, io.in1(31,24).orR)
    val orcb4 = Fill(8, io.in1(39,32).orR)
    val orcb5 = Fill(8, io.in1(47,40).orR)
    val orcb6 = Fill(8, io.in1(55,48).orR)
    val orcb7 = Fill(8, io.in1(63,56).orR)
    val orb = Cat(orcb7, orcb6, orcb5, orcb4, orcb3, orcb2, orcb1, orcb0)
    
    //rev
    val rev0 = io.in1(7,0)
    val rev1 = io.in1(15,8)
    val rev2 = io.in1(23,16)
    val rev3 = io.in1(31,24)
    val rev4 = io.in1(39,32)
    val rev5 = io.in1(47,40)
    val rev6 = io.in1(55,48)
    val rev7 = io.in1(63,56)
    val rev8 = Cat(rev0, rev1, rev2, rev3, rev4, rev5, rev6, rev7)

    //pack
    val pack = Cat(io.in2(31,0), io.in1(31,0))

    io.out := MuxCase("h_dead_beef_dead_beef".U,
                Array((io.fn === FN_ANDN)                              -> andn,
                      (io.fn === FN_ORN)                               -> orn,
                      (io.fn === FN_XNOR)                              -> xnor,
                      (io.fn === FN_CLZ   && io.dw === DW_64)  -> clz,
                      (io.fn === FN_CLZ   && io.dw === DW_32)  -> clzw,
                      (io.fn === FN_CTZ   && io.dw === DW_64)  -> ctz,
                      (io.fn === FN_CTZ   && io.dw === DW_32)  -> ctzw,
                      (io.fn === FN_PCNT  && io.dw === DW_64)  -> pcnt,
                      (io.fn === FN_PCNT  && io.dw === DW_32)  -> pcntw,
                      (io.fn === FN_MAX   && io.dw === DW_64)  -> max,
                      (io.fn === FN_MAXU  && io.dw === DW_64)  -> maxu,
                      (io.fn === FN_MIN   && io.dw === DW_64)  -> min,
                      (io.fn === FN_MINU  && io.dw === DW_64)  -> minu,
                      (io.fn === FN_SEXTB && io.dw === DW_64)  -> sextb,
                      (io.fn === FN_SEXTH && io.dw === DW_64)  -> sexth,
                      (io.fn === FN_ZEXTH && io.dw === DW_64)  -> zexth,
                      (io.fn === FN_ROL   && io.dw === DW_64)  -> rol,
                      (io.fn === FN_ROL   && io.dw === DW_32)  -> rolw,
                      (io.fn === FN_ROR   && io.dw === DW_64)  -> ror, //ror and rori same
                      (io.fn === FN_ROR   && io.dw === DW_32)  -> rorw, //rorw and roriw same
                      (io.fn === FN_ORCB  && io.dw === DW_64)  -> orb,
                      (io.fn === FN_REV8  && io.dw === DW_64)  -> rev8,
                      (io.fn === FN_PACK  && io.dw === DW_64)  -> pack)
                )
}
