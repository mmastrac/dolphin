// Copyright 2008 Dolphin Emulator Project
// Licensed under GPLv2+
// Refer to the license.txt file included.

#include "Common/BitSet.h"
#include "Common/CommonTypes.h"
#include "Common/x64Emitter.h"
#include "Core/CoreTiming.h"
#include "Core/HW/ProcessorInterface.h"
#include "Core/PowerPC/Jit64/Jit.h"
#include "Core/PowerPC/Jit64/JitRegCache.h"
#include "Core/PowerPC/JitCommon/Jit_Util.h"
#include "Core/PowerPC/PowerPC.h"

using namespace Gen;

void Jit64::GetCRFieldBit(int field, int bit, X64Reg out, bool negate)
{
  switch (bit)
  {
  case CR_SO_BIT:  // check bit 61 set
    BT(64, PPCSTATE(cr_val[field]), Imm8(61));
    SETcc(negate ? CC_NC : CC_C, R(out));
    break;

  case CR_EQ_BIT:  // check bits 31-0 == 0
    CMP(32, PPCSTATE(cr_val[field]), Imm8(0));
    SETcc(negate ? CC_NZ : CC_Z, R(out));
    break;

  case CR_GT_BIT:  // check val > 0
    CMP(64, PPCSTATE(cr_val[field]), Imm8(0));
    SETcc(negate ? CC_NG : CC_G, R(out));
    break;

  case CR_LT_BIT:  // check bit 62 set
    BT(64, PPCSTATE(cr_val[field]), Imm8(62));
    SETcc(negate ? CC_NC : CC_C, R(out));
    break;

  default:
    _assert_msg_(DYNA_REC, false, "Invalid CR bit");
  }
}

void Jit64::SetCRFieldBit(int field, int bit, X64Reg in)
{
  MOV(64, R(RSCRATCH2), PPCSTATE(cr_val[field]));
  MOVZX(32, 8, in, R(in));

  // Gross but necessary; if the input is totally zero and we set SO or LT,
  // or even just add the (1<<32), GT will suddenly end up set without us
  // intending to. This can break actual games, so fix it up.
  if (bit != CR_GT_BIT)
  {
    TEST(64, R(RSCRATCH2), R(RSCRATCH2));
    FixupBranch dont_clear_gt = J_CC(CC_NZ);
    BTS(64, R(RSCRATCH2), Imm8(63));
    SetJumpTarget(dont_clear_gt);
  }

  switch (bit)
  {
  case CR_SO_BIT:  // set bit 61 to input
    BTR(64, R(RSCRATCH2), Imm8(61));
    SHL(64, R(in), Imm8(61));
    OR(64, R(RSCRATCH2), R(in));
    break;

  case CR_EQ_BIT:  // clear low 32 bits, set bit 0 to !input
    SHR(64, R(RSCRATCH2), Imm8(32));
    SHL(64, R(RSCRATCH2), Imm8(32));
    XOR(32, R(in), Imm8(1));
    OR(64, R(RSCRATCH2), R(in));
    break;

  case CR_GT_BIT:  // set bit 63 to !input
    BTR(64, R(RSCRATCH2), Imm8(63));
    NOT(32, R(in));
    SHL(64, R(in), Imm8(63));
    OR(64, R(RSCRATCH2), R(in));
    break;

  case CR_LT_BIT:  // set bit 62 to input
    BTR(64, R(RSCRATCH2), Imm8(62));
    SHL(64, R(in), Imm8(62));
    OR(64, R(RSCRATCH2), R(in));
    break;
  }

  BTS(64, R(RSCRATCH2), Imm8(32));
  MOV(64, PPCSTATE(cr_val[field]), R(RSCRATCH2));
}

void Jit64::ClearCRFieldBit(int field, int bit)
{
  switch (bit)
  {
  case CR_SO_BIT:
    BTR(64, PPCSTATE(cr_val[field]), Imm8(61));
    break;

  case CR_EQ_BIT:
    OR(64, PPCSTATE(cr_val[field]), Imm8(1));
    break;

  case CR_GT_BIT:
    BTS(64, PPCSTATE(cr_val[field]), Imm8(63));
    break;

  case CR_LT_BIT:
    BTR(64, PPCSTATE(cr_val[field]), Imm8(62));
    break;
  }
  // We don't need to set bit 32; the cases where that's needed only come up when setting bits, not
  // clearing.
}

void Jit64::SetCRFieldBit(int field, int bit)
{
  MOV(64, R(RSCRATCH), PPCSTATE(cr_val[field]));
  if (bit != CR_GT_BIT)
  {
    TEST(64, R(RSCRATCH), R(RSCRATCH));
    FixupBranch dont_clear_gt = J_CC(CC_NZ);
    BTS(64, R(RSCRATCH), Imm8(63));
    SetJumpTarget(dont_clear_gt);
  }

  switch (bit)
  {
  case CR_SO_BIT:
    BTS(64, PPCSTATE(cr_val[field]), Imm8(61));
    break;

  case CR_EQ_BIT:
    SHR(64, R(RSCRATCH), Imm8(32));
    SHL(64, R(RSCRATCH), Imm8(32));
    break;

  case CR_GT_BIT:
    BTR(64, PPCSTATE(cr_val[field]), Imm8(63));
    break;

  case CR_LT_BIT:
    BTS(64, PPCSTATE(cr_val[field]), Imm8(62));
    break;
  }

  BTS(64, R(RSCRATCH), Imm8(32));
  MOV(64, PPCSTATE(cr_val[field]), R(RSCRATCH));
}

FixupBranch Jit64::JumpIfCRFieldBit(int field, int bit, bool jump_if_set)
{
  switch (bit)
  {
  case CR_SO_BIT:  // check bit 61 set
    BT(64, PPCSTATE(cr_val[field]), Imm8(61));
    return J_CC(jump_if_set ? CC_C : CC_NC, true);

  case CR_EQ_BIT:  // check bits 31-0 == 0
    CMP(32, PPCSTATE(cr_val[field]), Imm8(0));
    return J_CC(jump_if_set ? CC_Z : CC_NZ, true);

  case CR_GT_BIT:  // check val > 0
    CMP(64, PPCSTATE(cr_val[field]), Imm8(0));
    return J_CC(jump_if_set ? CC_G : CC_LE, true);

  case CR_LT_BIT:  // check bit 62 set
    BT(64, PPCSTATE(cr_val[field]), Imm8(62));
    return J_CC(jump_if_set ? CC_C : CC_NC, true);

  default:
    _assert_msg_(DYNA_REC, false, "Invalid CR bit");
  }

  // Should never happen.
  return FixupBranch();
}

static void DoICacheReset()
{
  PowerPC::ppcState.iCache.Reset();
}

void Jit64::mtspr(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITSystemRegistersOff);
  u32 iIndex = (inst.SPRU << 5) | (inst.SPRL & 0x1F);
  int d = inst.RD;

  switch (iIndex)
  {
  case SPR_DMAU:

  case SPR_SPRG0:
  case SPR_SPRG1:
  case SPR_SPRG2:
  case SPR_SPRG3:

  case SPR_SRR0:
  case SPR_SRR1:

  case SPR_LR:
  case SPR_CTR:

  case SPR_GQR0:
  case SPR_GQR0 + 1:
  case SPR_GQR0 + 2:
  case SPR_GQR0 + 3:
  case SPR_GQR0 + 4:
  case SPR_GQR0 + 5:
  case SPR_GQR0 + 6:
  case SPR_GQR0 + 7:
    // These are safe to do the easy way, see the bottom of this function.
    break;

  case SPR_XER:
  {
    auto rd = regs.gpr.Lock(d);
    auto xd = rd.Bind(BindMode::Read);
    auto scratch = regs.gpr.Borrow();
    MOV(32, scratch, xd);
    AND(32, scratch, Imm32(0xff7f));
    MOV(16, PPCSTATE(xer_stringctrl), scratch);

    MOV(32, scratch, xd);
    SHR(32, scratch, Imm8(XER_CA_SHIFT));
    AND(8, scratch, Imm8(1));
    MOV(8, PPCSTATE(xer_ca), scratch);

    MOV(32, scratch, xd);
    SHR(32, scratch, Imm8(XER_OV_SHIFT));
    MOV(8, PPCSTATE(xer_so_ov), scratch);
  }
    return;

  case SPR_HID0:
  {
    auto rd = regs.gpr.Lock(d);
    auto xd = rd.Bind(BindMode::Read);
    BTR(32, xd, Imm8(31 - 20));  // ICFI
    MOV(32, PPCSTATE(spr[iIndex]), xd);
    FixupBranch dont_reset_icache = J_CC(CC_NC);
    BitSet32 inuse = CallerSavedRegistersInUse();
    ABI_PushRegistersAndAdjustStack(inuse, 0);
    ABI_CallFunction((void*)DoICacheReset);
    ABI_PopRegistersAndAdjustStack(inuse, 0);
    SetJumpTarget(dont_reset_icache);
    break;
  }

  default:
    FALLBACK_IF(true);
  }

  auto rd = regs.gpr.Lock(d);
  rd.LoadIfNotImmediate();
  MOV(32, PPCSTATE(spr[iIndex]), rd);
}

void Jit64::mfspr(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITSystemRegistersOff);
  u32 iIndex = (inst.SPRU << 5) | (inst.SPRL & 0x1F);
  int d = inst.RD;
  switch (iIndex)
  {
  case SPR_TL:
  case SPR_TU:
  {
    // TODO: we really only need to call GetFakeTimeBase once per JIT block; this matters because
    // typical use of this instruction is to call it three times, e.g. mftbu/mftbl/mftbu/cmpw/bne
    // to deal with possible timer wraparound. This makes the second two (out of three) completely
    // redundant for the JIT.
    // no register choice

    gpr.FlushLockX(RDX, RAX);

    // An inline implementation of CoreTiming::GetFakeTimeBase, since in timer-heavy games the
    // cost of calling out to C for this is actually significant.
    // Scale downcount by the CPU overclocking factor.
    CVTSI2SS(XMM0, PPCSTATE(downcount));
    MULSS(XMM0, M(&CoreTiming::g_lastOCFactor_inverted));
    CVTSS2SI(RDX, R(XMM0));  // RDX is downcount scaled by the overclocking factor
    MOV(32, R(RAX), M(&CoreTiming::g_slicelength));
    SUB(64, R(RAX), R(RDX));  // cycles since the last CoreTiming::Advance() event is (slicelength -
                              // Scaled_downcount)
    ADD(64, R(RAX), M(&CoreTiming::g_globalTimer));
    SUB(64, R(RAX), M(&CoreTiming::g_fakeTBStartTicks));
    // It might seem convenient to correct the timer for the block position here for even more
    // accurate
    // timing, but as of currently, this can break games. If we end up reading a time *after* the
    // time
    // at which an interrupt was supposed to occur, e.g. because we're 100 cycles into a block with
    // only
    // 50 downcount remaining, some games don't function correctly, such as Karaoke Party
    // Revolution,
    // which won't get past the loading screen.
    // if (js.downcountAmount)
    //	ADD(64, R(RAX), Imm32(js.downcountAmount));

    // a / 12 = (a * 0xAAAAAAAAAAAAAAAB) >> 67
    MOV(64, R(RDX), Imm64(0xAAAAAAAAAAAAAAABULL));
    MUL(64, R(RDX));
    MOV(64, R(RAX), M(&CoreTiming::g_fakeTBStartValue));
    SHR(64, R(RDX), Imm8(3));
    ADD(64, R(RAX), R(RDX));
    MOV(64, PPCSTATE(spr[SPR_TL]), R(RAX));

    if (MergeAllowedNextInstructions(1))
    {
      const UGeckoInstruction& next = js.op[1].inst;
      // Two calls of TU/TL next to each other are extremely common in typical usage, so merge them
      // if we can.
      u32 nextIndex = (next.SPRU << 5) | (next.SPRL & 0x1F);
      // Be careful; the actual opcode is for mftb (371), not mfspr (339)
      int n = next.RD;
      if (next.OPCD == 31 && next.SUBOP10 == 371 && (nextIndex == SPR_TU || nextIndex == SPR_TL) &&
          n != d)
      {
        js.downcountAmount++;
        js.skipInstructions = 1;
        gpr.Lock(d, n);
        gpr.BindToRegister(d, false);
        gpr.BindToRegister(n, false);
        if (iIndex == SPR_TL)
          MOV(32, gpr.R(d), R(RAX));
        if (nextIndex == SPR_TL)
          MOV(32, gpr.R(n), R(RAX));
        SHR(64, R(RAX), Imm8(32));
        if (iIndex == SPR_TU)
          MOV(32, gpr.R(d), R(RAX));
        if (nextIndex == SPR_TU)
          MOV(32, gpr.R(n), R(RAX));
        break;
      }
    }
    gpr.Lock(d);
    gpr.BindToRegister(d, false);
    if (iIndex == SPR_TU)
      SHR(64, R(RAX), Imm8(32));
    MOV(32, gpr.R(d), R(RAX));
    break;
  }
  case SPR_XER:
    gpr.Lock(d);
    gpr.BindToRegister(d, false);
    MOVZX(32, 16, gpr.RX(d), PPCSTATE(xer_stringctrl));
    MOVZX(32, 8, RSCRATCH, PPCSTATE(xer_ca));
    SHL(32, R(RSCRATCH), Imm8(XER_CA_SHIFT));
    OR(32, gpr.R(d), R(RSCRATCH));

    MOVZX(32, 8, RSCRATCH, PPCSTATE(xer_so_ov));
    SHL(32, R(RSCRATCH), Imm8(XER_OV_SHIFT));
    OR(32, gpr.R(d), R(RSCRATCH));
    break;
  case SPR_WPAR:
  case SPR_DEC:
  case SPR_PMC1:
  case SPR_PMC2:
  case SPR_PMC3:
  case SPR_PMC4:
    FALLBACK_IF(true);
  default:
    gpr.Lock(d);
    gpr.BindToRegister(d, false);
    MOV(32, gpr.R(d), PPCSTATE(spr[iIndex]));
    break;
  }
  gpr.UnlockAllX();
  gpr.UnlockAll();
}

void Jit64::mtmsr(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITSystemRegistersOff);
  if (!gpr.R(inst.RS).IsImm())
  {
    gpr.Lock(inst.RS);
    gpr.BindToRegister(inst.RS, true, false);
  }
  MOV(32, PPCSTATE(msr), gpr.R(inst.RS));
  gpr.UnlockAll();
  gpr.Flush();
  fpr.Flush();

  // If some exceptions are pending and EE are now enabled, force checking
  // external exceptions when going out of mtmsr in order to execute delayed
  // interrupts as soon as possible.
  TEST(32, PPCSTATE(msr), Imm32(0x8000));
  FixupBranch eeDisabled = J_CC(CC_Z);

  TEST(32, PPCSTATE(Exceptions),
       Imm32(EXCEPTION_EXTERNAL_INT | EXCEPTION_PERFORMANCE_MONITOR | EXCEPTION_DECREMENTER));
  FixupBranch noExceptionsPending = J_CC(CC_Z);

  // Check if a CP interrupt is waiting and keep the GPU emulation in sync (issue 4336)
  TEST(32, M(&ProcessorInterface::m_InterruptCause), Imm32(ProcessorInterface::INT_CAUSE_CP));
  FixupBranch cpInt = J_CC(CC_NZ);

  MOV(32, PPCSTATE(pc), Imm32(js.compilerPC + 4));
  WriteExternalExceptionExit();

  SetJumpTarget(cpInt);
  SetJumpTarget(noExceptionsPending);
  SetJumpTarget(eeDisabled);

  MOV(32, R(RSCRATCH), Imm32(js.compilerPC + 4));
  WriteExitDestInRSCRATCH();
}

void Jit64::mfmsr(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITSystemRegistersOff);
  // Privileged?
  int d = inst.RD;
  auto rd = regs.gpr.Lock(d);
  auto xd = rd.Bind(BindMode::Write);
  MOV(32, xd, PPCSTATE(msr));
}

void Jit64::mftb(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITSystemRegistersOff);
  mfspr(inst);
}

void Jit64::mfcr(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITSystemRegistersOff);
  int d = inst.RD;
  auto rd = regs.gpr.Lock(d);
  // mfcr needs these three registers
  // TODO: this is not ideal
  auto scratch_extra = regs.gpr.Borrow(RSCRATCH_EXTRA);
  auto scratch = regs.gpr.Borrow(RSCRATCH);
  auto scratch2 = regs.gpr.Borrow(RSCRATCH2);
  CALL(asm_routines.mfcr);
  auto xd = rd.Bind(BindMode::Write);
  MOV(32, xd, scratch);
}

void Jit64::mtcrf(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITSystemRegistersOff);

  // USES_CR
  u32 crm = inst.CRM;
  if (crm != 0)
  {
    if (gpr.R(inst.RS).IsImm())
    {
      for (int i = 0; i < 8; i++)
      {
        if ((crm & (0x80 >> i)) != 0)
        {
          u8 newcr = (gpr.R(inst.RS).Imm32() >> (28 - (i * 4))) & 0xF;
          u64 newcrval = PPCCRToInternal(newcr);
          if ((s64)newcrval == (s32)newcrval)
          {
            MOV(64, PPCSTATE(cr_val[i]), Imm32((s32)newcrval));
          }
          else
          {
            MOV(64, R(RSCRATCH), Imm64(newcrval));
            MOV(64, PPCSTATE(cr_val[i]), R(RSCRATCH));
          }
        }
      }
    }
    else
    {
      gpr.Lock(inst.RS);
      gpr.BindToRegister(inst.RS, true, false);
      for (int i = 0; i < 8; i++)
      {
        if ((crm & (0x80 >> i)) != 0)
        {
          MOV(32, R(RSCRATCH), gpr.R(inst.RS));
          if (i != 7)
            SHR(32, R(RSCRATCH), Imm8(28 - (i * 4)));
          if (i != 0)
            AND(32, R(RSCRATCH), Imm8(0xF));
          MOV(64, R(RSCRATCH), MScaled(RSCRATCH, SCALE_8, (u32)(u64)m_crTable));
          MOV(64, PPCSTATE(cr_val[i]), R(RSCRATCH));
        }
      }
      gpr.UnlockAll();
    }
  }
}

void Jit64::mcrf(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITSystemRegistersOff);

  // USES_CR
  if (inst.CRFS != inst.CRFD)
  {
    MOV(64, R(RSCRATCH), PPCSTATE(cr_val[inst.CRFS]));
    MOV(64, PPCSTATE(cr_val[inst.CRFD]), R(RSCRATCH));
  }
}

void Jit64::mcrxr(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITSystemRegistersOff);

  // Copy XER[0-3] into CR[inst.CRFD]
  MOVZX(32, 8, RSCRATCH, PPCSTATE(xer_ca));
  MOVZX(32, 8, RSCRATCH2, PPCSTATE(xer_so_ov));
  // [0 SO OV CA]
  LEA(32, RSCRATCH, MComplex(RSCRATCH, RSCRATCH2, SCALE_2, 0));
  // [SO OV CA 0] << 3
  SHL(32, R(RSCRATCH), Imm8(4));

  MOV(64, R(RSCRATCH), MDisp(RSCRATCH, (u32)(u64)m_crTable));
  MOV(64, PPCSTATE(cr_val[inst.CRFD]), R(RSCRATCH));

  // Clear XER[0-3]
  MOV(8, PPCSTATE(xer_ca), Imm8(0));
  MOV(8, PPCSTATE(xer_so_ov), Imm8(0));
}

void Jit64::crXXX(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITSystemRegistersOff);
  _dbg_assert_msg_(DYNA_REC, inst.OPCD == 19, "Invalid crXXX");

  // Special case: crclr
  if (inst.CRBA == inst.CRBB && inst.CRBA == inst.CRBD && inst.SUBOP10 == 193)
  {
    ClearCRFieldBit(inst.CRBD >> 2, 3 - (inst.CRBD & 3));
    return;
  }

  // Special case: crset
  if (inst.CRBA == inst.CRBB && inst.CRBA == inst.CRBD && inst.SUBOP10 == 289)
  {
    SetCRFieldBit(inst.CRBD >> 2, 3 - (inst.CRBD & 3));
    return;
  }

  // TODO(delroth): Potential optimizations could be applied here. For
  // instance, if the two CR bits being loaded are the same, two loads are
  // not required.

  // creqv or crnand or crnor
  bool negateA = inst.SUBOP10 == 289 || inst.SUBOP10 == 225 || inst.SUBOP10 == 33;
  // crandc or crorc or crnand or crnor
  bool negateB =
      inst.SUBOP10 == 129 || inst.SUBOP10 == 417 || inst.SUBOP10 == 225 || inst.SUBOP10 == 33;

  GetCRFieldBit(inst.CRBA >> 2, 3 - (inst.CRBA & 3), RSCRATCH, negateA);
  GetCRFieldBit(inst.CRBB >> 2, 3 - (inst.CRBB & 3), RSCRATCH2, negateB);

  // Compute combined bit
  switch (inst.SUBOP10)
  {
  case 33:   // crnor: ~(A || B) == (~A && ~B)
  case 129:  // crandc: A && ~B
  case 257:  // crand:  A && B
    AND(8, R(RSCRATCH), R(RSCRATCH2));
    break;

  case 193:  // crxor: A ^ B
  case 289:  // creqv: ~(A ^ B) = ~A ^ B
    XOR(8, R(RSCRATCH), R(RSCRATCH2));
    break;

  case 225:  // crnand: ~(A && B) == (~A || ~B)
  case 417:  // crorc: A || ~B
  case 449:  // cror:  A || B
    OR(8, R(RSCRATCH), R(RSCRATCH2));
    break;
  }

  // Store result bit in CRBD
  SetCRFieldBit(inst.CRBD >> 2, 3 - (inst.CRBD & 3), RSCRATCH);
}

void Jit64::mcrfs(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITSystemRegistersOff);

  u8 shift = 4 * (7 - inst.CRFS);
  u32 mask = 0xF << shift;

  // Only clear exception bits (but not FEX/VX).
  mask &= 0x9FF87000;

  auto scratch = regs.gpr.Borrow();
  auto scratch2 = regs.gpr.Borrow();

  MOV(32, scratch, PPCSTATE(fpscr));
  if (cpu_info.bBMI1)
  {
    MOV(32, scratch2, Imm32((4 << 8) | shift));
    BEXTR(32, scratch2, scratch, scratch2);
  }
  else
  {
    MOV(32, scratch2, scratch);
    SHR(32, scratch2, Imm8(shift));
    AND(32, scratch2, Imm32(0xF));
  }
  AND(32, scratch, Imm32(mask));
  MOV(32, PPCSTATE(fpscr), scratch);
  LEA(64, scratch, M(&m_crTable));
  MOV(64, scratch, MComplex(scratch, scratch2, SCALE_8, 0));
  MOV(64, PPCSTATE(cr_val[inst.CRFD]), scratch);
}

void Jit64::mffsx(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITSystemRegistersOff);
  FALLBACK_IF(inst.Rc);

  int d = inst.FD;
  auto rd = regs.fpu.Lock(d);

  auto fpscr = regs.gpr.Borrow();
  auto scratch = regs.gpr.Borrow();

  MOV(32, R(fpscr), PPCSTATE(fpscr));

  // FPSCR.FEX = 0 (and VX for below)
  AND(32, R(fpscr), Imm32(~0x60000000));

  // FPSCR.VX = (FPSCR.Hex & FPSCR_VX_ANY) != 0;
  XOR(32, R(scratch), R(scratch));
  TEST(32, R(fpscr), Imm32(FPSCR_VX_ANY));
  SETcc(CC_NZ, R(scratch));
  SHL(32, R(scratch), Imm8(31 - 2));
  OR(32, R(fpscr), R(scratch));

  MOV(32, PPCSTATE(fpscr), R(fpscr));

  MOV(64, R(scratch), Imm64(0xFFF8000000000000));
  OR(64, R(fpscr), R(scratch));
  auto xmm = regs.fpu.Borrow();
  MOVQ_xmm(xmm, R(fpscr));
  auto xd = rd.Bind(BindMode::Write);
  MOVSD(xd, R(xmm));
}

// MXCSR = s_fpscr_to_mxcsr[FPSCR & 7]
static const u32 s_fpscr_to_mxcsr[] = {
    0x1F80, 0x7F80, 0x5F80, 0x3F80, 0x9F80, 0xFF80, 0xDF80, 0xBF80,
};

// Needs value of FPSCR in RSCRATCH.
void Jit64::UpdateMXCSR(X64Reg fpscr)
{
  auto scratch = regs.gpr.Borrow();
  LEA(64, scratch, M(&s_fpscr_to_mxcsr));
  AND(32, R(fpscr), Imm32(7));
  LDMXCSR(MComplex(scratch, fpscr, SCALE_4, 0));
}

void Jit64::mtfsb0x(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITSystemRegistersOff);
  FALLBACK_IF(inst.Rc);

  u32 mask = ~(0x80000000 >> inst.CRBD);
  if (inst.CRBD < 29)
  {
    AND(32, PPCSTATE(fpscr), Imm32(mask));
  }
  else
  {
    auto scratch = regs.gpr.Borrow();
    MOV(32, scratch, PPCSTATE(fpscr));
    AND(32, scratch, Imm32(mask));
    MOV(32, PPCSTATE(fpscr), scratch);
    UpdateMXCSR(scratch);
  }
}

void Jit64::mtfsb1x(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITSystemRegistersOff);
  FALLBACK_IF(inst.Rc);

  auto scratch = regs.gpr.Borrow();
  u32 mask = 0x80000000 >> inst.CRBD;
  MOV(32, scratch, PPCSTATE(fpscr));
  if (mask & FPSCR_ANY_X)
  {
    BTS(32, scratch, Imm8(31 - inst.CRBD));
    FixupBranch dont_set_fx = J_CC(CC_C);
    OR(32, scratch, Imm32(1u << 31));
    SetJumpTarget(dont_set_fx);
  }
  else
  {
    OR(32, scratch, Imm32(mask));
  }
  MOV(32, PPCSTATE(fpscr), scratch);
  if (inst.CRBD >= 29)
    UpdateMXCSR(scratch);
}

void Jit64::mtfsfix(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITSystemRegistersOff);
  FALLBACK_IF(inst.Rc);

  u8 imm = (inst.hex >> (31 - 19)) & 0xF;
  u32 or_mask = imm << (28 - 4 * inst.CRFD);
  u32 and_mask = ~(0xF0000000 >> (4 * inst.CRFD));

  auto scratch = regs.gpr.Borrow();
  MOV(32, scratch, PPCSTATE(fpscr));
  AND(32, scratch, Imm32(and_mask));
  OR(32, scratch, Imm32(or_mask));
  MOV(32, PPCSTATE(fpscr), scratch);

  // Field 7 contains NI and RN.
  if (inst.CRFD == 7)
    LDMXCSR(M(&s_fpscr_to_mxcsr[imm & 7]));
}

void Jit64::mtfsfx(UGeckoInstruction inst)
{
  INSTRUCTION_START
  JITDISABLE(bJITSystemRegistersOff);
  FALLBACK_IF(inst.Rc);

  u32 mask = 0;
  for (int i = 0; i < 8; i++)
  {
    if (inst.FM & (1 << i))
      mask |= 0xF << (4 * i);
  }

  int b = inst.FB;
  auto rb = regs.fpu.Lock(b);

  auto scratch = regs.gpr.Borrow();
  auto scratch2 = regs.gpr.Borrow();

  if (rb.IsRegBound())
  {
    auto xb = rb.Bind(BindMode::Reuse);
    MOVQ_xmm(R(scratch), xb);
  }
  else
  {
    MOV(32, scratch, rb);
  }

  MOV(32, scratch2, PPCSTATE(fpscr));
  AND(32, scratch, Imm32(mask));
  AND(32, scratch2, Imm32(~mask));
  OR(32, scratch, scratch2);
  MOV(32, PPCSTATE(fpscr), scratch);

  if (inst.FM & 1)
    UpdateMXCSR(scratch);
}
