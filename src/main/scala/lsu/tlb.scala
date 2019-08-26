package boom.lsu

import chisel3._
import chisel3.util._
import chisel3.experimental.dontTouch

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem.{CacheBlockBytes}
import freechips.rocketchip.diplomacy.{RegionType}
import freechips.rocketchip.util._

import boom.common._
import boom.exu.{BrResolutionInfo, Exception, FuncUnitResp, CommitSignals}
import boom.util.{BoolToChar, AgePriorityEncoder, IsKilledByBranch, GetNewBrMask, WrapInc, IsOlder, UpdateBrMask}

class NBDTLB(instruction: Boolean, lgMaxSize: Int, cfg: TLBConfig)(implicit edge: TLEdgeOut, p: Parameters) extends BoomModule()(p) {
  require(!instruction)
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new TLBReq(lgMaxSize)))
    val miss_rdy = Output(Bool())
    val resp = Output(new TLBResp)
    val sfence = Input(Valid(new SFenceReq))
    val ptw = new TLBPTWIO
    val kill = Input(Bool())
  })

  class EntryData extends Bundle {
    val ppn = UInt(ppnBits.W)
    val u = Bool()
    val g = Bool()
    val ae = Bool()
    val sw = Bool()
    val sx = Bool()
    val sr = Bool()
    val pw = Bool()
    val px = Bool()
    val pr = Bool()
    val pal = Bool() // AMO logical
    val paa = Bool() // AMO arithmetic
    val eff = Bool() // get/put effects
    val c = Bool()
    val fragmented_superpage = Bool()
  }

  class Entry(val nSectors: Int, val superpage: Boolean, val superpageOnly: Boolean) extends Bundle {
    require(nSectors == 1 || !superpage)
    require(isPow2(nSectors))
    require(!superpageOnly || superpage)

    val level = UInt(log2Ceil(pgLevels).W)
    val tag = UInt(vpnBits.W)
    val data = Vec(nSectors, UInt(new EntryData().getWidth.W))
    val valid = Vec(nSectors, Bool())
    def entry_data = data.map(_.asTypeOf(new EntryData))

    private def sectorIdx(vpn: UInt) = vpn.extract(log2Ceil(nSectors)-1, 0)
    def getData(vpn: UInt) = OptimizationBarrier(data(sectorIdx(vpn)).asTypeOf(new EntryData))
    def sectorHit(vpn: UInt) = valid.orR && sectorTagMatch(vpn)
    def sectorTagMatch(vpn: UInt) = ((tag ^ vpn) >> log2Ceil(nSectors)) === 0.U
    def hit(vpn: UInt) = {
      if (superpage && usingVM) {
        var tagMatch = valid.head
        for (j <- 0 until pgLevels) {
          val base = vpnBits - (j + 1) * pgLevelBits
          val ignore = level < j.U || (superpageOnly && j == pgLevels - 1).B
          tagMatch = tagMatch && (ignore || tag(base + pgLevelBits - 1, base) === vpn(base + pgLevelBits - 1, base))
        }
        tagMatch
      } else {
        val idx = sectorIdx(vpn)
        valid(idx) && sectorTagMatch(vpn)
      }
    }
    def ppn(vpn: UInt) = {
      val data = getData(vpn)
      if (superpage && usingVM) {
        var res = data.ppn >> pgLevelBits*(pgLevels - 1)
        for (j <- 1 until pgLevels) {
          val ignore = (level < j.U) || (superpageOnly && j == pgLevels - 1).B
          res = Cat(res, (Mux(ignore, vpn, 0.U) | data.ppn)(vpnBits - j*pgLevelBits - 1, vpnBits - (j + 1)*pgLevelBits))
        }
        res
      } else {
        data.ppn
      }
    }

    def insert(tag: UInt, level: UInt, entry: EntryData) {
      this.tag := tag
      this.level := level.extract(log2Ceil(pgLevels - superpageOnly.toInt)-1, 0)

      val idx = sectorIdx(tag)
      valid(idx) := true.B
      data(idx) := entry.asUInt
    }

    def invalidate() { valid.foreach(_ := false.B) }
    def invalidateVPN(vpn: UInt) {
      if (superpage) {
         when (hit(vpn)) { invalidate() }
      } else {
        when (sectorTagMatch(vpn)) { valid(sectorIdx(vpn)) := false.B }

        // For fragmented superpage mappings, we assume the worst (largest)
        // case, and zap entries whose most-significant VPNs match
         when (((tag ^ vpn) >> (pgLevelBits * (pgLevels - 1))) === 0.U) {
           for ((v, e) <- valid zip entry_data)
             when (e.fragmented_superpage) { v := false.B }
         }
      }
    }
    def invalidateNonGlobal() {
       for ((v, e) <- valid zip entry_data)
         when (!e.g) { v := false.B }
    }
  }

  val pageGranularityPMPs = pmpGranularity >= (1 << pgIdxBits)
  val sectored_entries = Reg(Vec(cfg.nEntries / cfg.nSectors, new Entry(cfg.nSectors, false, false)))
  val superpage_entries = Reg(Vec(cfg.nSuperpageEntries, new Entry(1, true, true)))
  val special_entry = (!pageGranularityPMPs).option(Reg(new Entry(1, true, false)))
  def ordinary_entries = sectored_entries ++ superpage_entries
  def all_entries = ordinary_entries ++ special_entry

  val s_ready :: s_request :: s_wait :: s_wait_invalidate :: Nil = Enum(4)
  val state = RegInit(s_ready)
  val r_refill_tag = Reg(UInt(vpnBits.W))
  val r_superpage_repl_addr = Reg(UInt(log2Ceil(superpage_entries.size).W))
  val r_sectored_repl_addr = Reg(UInt(log2Ceil(sectored_entries.size).W))
  val r_sectored_hit_addr = Reg(UInt(log2Ceil(sectored_entries.size).W))
  val r_sectored_hit = Reg(Bool())

  val priv = if (instruction) io.ptw.status.prv else io.ptw.status.dprv
  val priv_s = priv(0)
  val priv_uses_vm = priv <= PRV.S.U
  val vm_enabled = usingVM.B && io.ptw.ptbr.mode(io.ptw.ptbr.mode.getWidth-1) && priv_uses_vm && !io.req.bits.passthrough

  // share a single physical memory attribute checker (unshare if critical path)
  val vpn = io.req.bits.vaddr(vaddrBits-1, pgIdxBits)
  val refill_ppn = io.ptw.resp.bits.pte.ppn(ppnBits-1, 0)
  val do_refill = usingVM.B && io.ptw.resp.valid
  val invalidate_refill = state.isOneOf(s_request /* don't care */, s_wait_invalidate)
  val mpu_ppn = Mux(do_refill, refill_ppn,
                Mux(vm_enabled && special_entry.nonEmpty.B, special_entry.map(_.ppn(vpn)).getOrElse(0.U), io.req.bits.vaddr >> pgIdxBits))
  val mpu_physaddr = Cat(mpu_ppn, io.req.bits.vaddr(pgIdxBits-1, 0))
  val pmp = Module(new PMPChecker(lgMaxSize))
  pmp.io.addr := mpu_physaddr
  pmp.io.size := io.req.bits.size
  pmp.io.pmp := (io.ptw.pmp: Seq[PMP])
  pmp.io.prv := Mux(usingVM.B && (do_refill || io.req.bits.passthrough /* PTW */), PRV.S.U, priv)
  val legal_address = edge.manager.findSafe(mpu_physaddr).reduce(_||_)
  def fastCheck(member: TLManagerParameters => Boolean) =
    legal_address && edge.manager.fastProperty(mpu_physaddr, member, (b:Boolean) => b.B)
  val cacheable = fastCheck(_.supportsAcquireT) && (instruction || !usingDataScratchpad).B
  val homogeneous = TLBPageLookup(edge.manager.managers, xLen, p(CacheBlockBytes), BigInt(1) << pgIdxBits)(mpu_physaddr).homogeneous
  val prot_r = fastCheck(_.supportsGet) && pmp.io.r
  val prot_w = fastCheck(_.supportsPutFull) && pmp.io.w
  val prot_al = fastCheck(_.supportsLogical)
  val prot_aa = fastCheck(_.supportsArithmetic)
  val prot_x = fastCheck(_.executable) && pmp.io.x
  val prot_eff = fastCheck(Seq(RegionType.PUT_EFFECTS, RegionType.GET_EFFECTS) contains _.regionType)

  val sector_hits = sectored_entries.map(_.sectorHit(vpn))
  val superpage_hits = superpage_entries.map(_.hit(vpn))
  val hitsVec = all_entries.map(vm_enabled && _.hit(vpn))
  val real_hits = hitsVec.asUInt
  val hits = Cat(!vm_enabled, real_hits)
  val ppn = Mux1H(hitsVec :+ !vm_enabled, all_entries.map(_.ppn(vpn)) :+ vpn(ppnBits-1, 0))

    // permission bit arrays
  when (do_refill && !invalidate_refill) {
    val pte = io.ptw.resp.bits.pte
    val newEntry = Wire(new EntryData)
    newEntry.ppn := pte.ppn
    newEntry.c := cacheable
    newEntry.u := pte.u
    newEntry.g := pte.g
    newEntry.ae := io.ptw.resp.bits.ae
    newEntry.sr := pte.sr()
    newEntry.sw := pte.sw()
    newEntry.sx := pte.sx()
    newEntry.pr := prot_r
    newEntry.pw := prot_w
    newEntry.px := prot_x
    newEntry.pal := prot_al
    newEntry.paa := prot_aa
    newEntry.eff := prot_eff
    newEntry.fragmented_superpage := io.ptw.resp.bits.fragmented_superpage

    when (special_entry.nonEmpty.B && !io.ptw.resp.bits.homogeneous) {
      special_entry.foreach(_.insert(r_refill_tag, io.ptw.resp.bits.level, newEntry))
    }.elsewhen (io.ptw.resp.bits.level < (pgLevels-1).U) {
      for ((e, i) <- superpage_entries.zipWithIndex) when (r_superpage_repl_addr === i.U) {
        e.insert(r_refill_tag, io.ptw.resp.bits.level, newEntry)
      }
    }.otherwise {
      val waddr = Mux(r_sectored_hit, r_sectored_hit_addr, r_sectored_repl_addr)
      for ((e, i) <- sectored_entries.zipWithIndex) when (waddr === i.U) {
        when (!r_sectored_hit) { e.invalidate() }
        e.insert(r_refill_tag, 0.U, newEntry)
      }
    }
  }

  val entries = all_entries.map(_.getData(vpn))
  val normal_entries = ordinary_entries.map(_.getData(vpn))
  val nPhysicalEntries = 1 + special_entry.size
  val ptw_ae_array = Cat(false.B, entries.map(_.ae).asUInt)
  val priv_rw_ok = Mux(!priv_s || io.ptw.status.sum, entries.map(_.u).asUInt, 0.U) | Mux(priv_s, ~entries.map(_.u).asUInt, 0.U)
  val priv_x_ok = Mux(priv_s, ~entries.map(_.u).asUInt, entries.map(_.u).asUInt)
  val r_array = Cat(true.B, priv_rw_ok & (entries.map(_.sr).asUInt | Mux(io.ptw.status.mxr, entries.map(_.sx).asUInt, 0.U)))
  val w_array = Cat(true.B, priv_rw_ok & entries.map(_.sw).asUInt)
  val x_array = Cat(true.B, priv_x_ok & entries.map(_.sx).asUInt)
  val pr_array = Cat(Fill(nPhysicalEntries, prot_r), normal_entries.map(_.pr).asUInt) & ~ptw_ae_array
  val pw_array = Cat(Fill(nPhysicalEntries, prot_w), normal_entries.map(_.pw).asUInt) & ~ptw_ae_array
  val px_array = Cat(Fill(nPhysicalEntries, prot_x), normal_entries.map(_.px).asUInt) & ~ptw_ae_array
  val eff_array = Cat(Fill(nPhysicalEntries, prot_eff), normal_entries.map(_.eff).asUInt)
  val c_array = Cat(Fill(nPhysicalEntries, cacheable), normal_entries.map(_.c).asUInt)
  val paa_array = Cat(Fill(nPhysicalEntries, prot_aa), normal_entries.map(_.paa).asUInt)
  val pal_array = Cat(Fill(nPhysicalEntries, prot_al), normal_entries.map(_.pal).asUInt)
  val paa_array_if_cached = paa_array | Mux(usingAtomicsInCache.B, c_array, 0.U)
  val pal_array_if_cached = pal_array | Mux(usingAtomicsInCache.B, c_array, 0.U)
  val prefetchable_array = Cat((cacheable && homogeneous) << (nPhysicalEntries-1), normal_entries.map(_.c).asUInt)

  val misaligned = (io.req.bits.vaddr & (UIntToOH(io.req.bits.size) - 1.U)).orR
  val bad_va = if (!usingVM || (minPgLevels == pgLevels && vaddrBits == vaddrBitsExtended)) false.B else vm_enabled && {
    val nPgLevelChoices = pgLevels - minPgLevels + 1
    val minVAddrBits = pgIdxBits + minPgLevels * pgLevelBits
    (for (i <- 0 until nPgLevelChoices) yield {
      val mask = ((BigInt(1) << vaddrBitsExtended) - (BigInt(1) << (minVAddrBits + i * pgLevelBits - 1))).U
      val maskedVAddr = io.req.bits.vaddr & mask
      io.ptw.ptbr.additionalPgLevels === i.U && !(maskedVAddr === 0.U || maskedVAddr === mask)
    }).orR
  }

  val cmd_lrsc = usingAtomics.B && io.req.bits.cmd.isOneOf(M_XLR, M_XSC)
  val cmd_amo_logical = usingAtomics.B && isAMOLogical(io.req.bits.cmd)
  val cmd_amo_arithmetic = usingAtomics.B && isAMOArithmetic(io.req.bits.cmd)
  val cmd_read = isRead(io.req.bits.cmd)
  val cmd_write = isWrite(io.req.bits.cmd)
  val cmd_write_perms = cmd_write ||
    coreParams.haveCFlush.B && io.req.bits.cmd === M_FLUSH_ALL // not a write, but needs write permissions

  val lrscAllowed = Mux((usingDataScratchpad || usingAtomicsOnlyForIO).B, 0.U, c_array)
  val ae_array =
    Mux(misaligned, eff_array, 0.U) |
    Mux(cmd_lrsc, ~lrscAllowed, 0.U)
  val ae_ld_array = Mux(cmd_read, ae_array | ~pr_array, 0.U)
  val ae_st_array =
    Mux(cmd_write_perms, ae_array | ~pw_array, 0.U) |
    Mux(cmd_amo_logical, ~pal_array_if_cached, 0.U) |
    Mux(cmd_amo_arithmetic, ~paa_array_if_cached, 0.U)
  val must_alloc_array =
    Mux(cmd_amo_logical, ~paa_array, 0.U) |
    Mux(cmd_amo_arithmetic, ~pal_array, 0.U) |
    Mux(cmd_lrsc, ~0.U(pal_array.getWidth.W), 0.U)
  val ma_ld_array = Mux(misaligned && cmd_read, ~eff_array, 0.U)
  val ma_st_array = Mux(misaligned && cmd_write, ~eff_array, 0.U)
  val pf_ld_array = Mux(cmd_read, ~(r_array | ptw_ae_array), 0.U)
  val pf_st_array = Mux(cmd_write_perms, ~(w_array | ptw_ae_array), 0.U)
  val pf_inst_array = ~(x_array | ptw_ae_array)

  val tlb_hit = real_hits.orR
  val tlb_miss = vm_enabled && !bad_va && !tlb_hit

  val sectored_plru = new PseudoLRU(sectored_entries.size)
  val superpage_plru = new PseudoLRU(superpage_entries.size)
  when (io.req.valid && vm_enabled) {
    when (sector_hits.orR) { sectored_plru.access(OHToUInt(sector_hits)) }
    when (superpage_hits.orR) { superpage_plru.access(OHToUInt(superpage_hits)) }
  }

  // Superpages create the possibility that two entries in the TLB may match.
  // This corresponds to a software bug, but we can't return complete garbage;
  // we must return either the old translation or the new translation.  This
  // isn't compatible with the Mux1H approach.  So, flush the TLB and report
  // a miss on duplicate entries.
  val multipleHits = PopCountAtLeast(real_hits, 2)

  io.req.ready := true.B
  io.miss_rdy := state === s_ready
  io.resp.pf.ld := (bad_va && cmd_read) || (pf_ld_array & hits).orR
  io.resp.pf.st := (bad_va && cmd_write_perms) || (pf_st_array & hits).orR
  io.resp.pf.inst := bad_va || (pf_inst_array & hits).orR
  io.resp.ae.ld := (ae_ld_array & hits).orR
  io.resp.ae.st := (ae_st_array & hits).orR
  io.resp.ae.inst := (~px_array & hits).orR
  io.resp.ma.ld := (ma_ld_array & hits).orR
  io.resp.ma.st := (ma_st_array & hits).orR
  io.resp.ma.inst := false.B // this is up to the pipeline to figure out
  io.resp.cacheable := (c_array & hits).orR
  io.resp.must_alloc := (must_alloc_array & hits).orR
  io.resp.prefetchable := (prefetchable_array & hits).orR && edge.manager.managers.forall(m => !m.supportsAcquireB || m.supportsHint).B
  io.resp.miss := do_refill || tlb_miss || multipleHits
  io.resp.paddr := Cat(ppn, io.req.bits.vaddr(pgIdxBits-1, 0))

  io.ptw.req.valid := state === s_request
  io.ptw.req.bits.valid := !io.kill
  io.ptw.req.bits.bits.addr := r_refill_tag

  if (usingVM) {
    val sfence = io.sfence.valid
    when (io.req.fire() && tlb_miss && state === s_ready) {
      state := s_request
      r_refill_tag := vpn

      r_superpage_repl_addr := replacementEntry(superpage_entries, superpage_plru.replace)
      r_sectored_repl_addr := replacementEntry(sectored_entries, sectored_plru.replace)
      r_sectored_hit_addr := OHToUInt(sector_hits)
      r_sectored_hit := sector_hits.orR
    }
    when (state === s_request) {
      when (sfence) { state := s_ready }
      when (io.ptw.req.ready) { state := Mux(sfence, s_wait_invalidate, s_wait) }
      when (io.kill) { state := s_ready }
    }
    when (state === s_wait && sfence) {
      state := s_wait_invalidate
    }
    when (io.ptw.resp.valid) {
      state := s_ready
    }

    when (sfence) {
      assert(!io.sfence.bits.rs1 || (io.sfence.bits.addr >> pgIdxBits) === vpn)
      for (e <- all_entries) {
        when (io.sfence.bits.rs1) { e.invalidateVPN(vpn) }
        .elsewhen (io.sfence.bits.rs2) { e.invalidateNonGlobal() }
        .otherwise { e.invalidate() }
      }
    }
    when (multipleHits || reset.asBool) {
      all_entries.foreach(_.invalidate())
    }
  }

  def replacementEntry(set: Seq[Entry], alt: UInt) = {
    val valids = set.map(_.valid.orR).asUInt
    Mux(valids.andR, alt, PriorityEncoder(~valids))
  }


}
