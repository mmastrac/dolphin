// Copyright 2008 Dolphin Emulator Project
// Licensed under GPLv2+
// Refer to the license.txt file included.

#include "Common/Assert.h"
#include "Common/CommonTypes.h"
#include "Common/x64Emitter.h"
#include "Core/ConfigManager.h"
#include "Core/CoreTiming.h"
#include "Core/PowerPC/Gekko.h"
#include "Core/PowerPC/PowerPC.h"
#include "Core/PowerPC/PPCAnalyst.h"
#include "Core/PowerPC/Jit64/Jit.h"

// The branches are known good, or at least reasonably good.
// No need for a disable-mechanism.

// If defined, clears CR0 at blr and bl-s. If the assumption that
// flags never carry over between functions holds, then the task for
// an optimizer becomes much easier.

// #define ACID_TEST

// Zelda and many more games seem to pass the Acid Test.

using namespace Gen;

void Jit64::sc(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITBranchOff);

	regs.Flush();
	LOCK();
	OR(32, PPCSTATE(Exceptions), Imm32(EXCEPTION_SYSCALL));
	WriteExit(js.compilerPC + 4, ExitExceptionCheck::CHECK_ALL_EXCEPTIONS);
}

void Jit64::rfi(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITBranchOff);

	regs.Flush();
	// See Interpreter rfi for details
	const u32 mask = 0x87C0FFFF;
	const u32 clearMSR13 = 0xFFFBFFFF; // Mask used to clear the bit MSR[13]
	// MSR = ((MSR & ~mask) | (SRR1 & mask)) & clearMSR13;
	AND(32, PPCSTATE(msr), Imm32((~mask) & clearMSR13));
	auto scratch = regs.gpr.Borrow();
	MOV(32, scratch, PPCSTATE_SRR1);
	AND(32, scratch, Imm32(mask & clearMSR13));
	OR(32, PPCSTATE(msr), scratch);
	// NPC = SRR0
	WriteExit(PPCSTATE_SRR0, ExitExceptionCheck::CHECK_ALL_EXCEPTIONS);
}

void Jit64::bx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITBranchOff);

	// If this is not the last instruction of a block, we will skip the rest
	// of the process because PPCAnalyst::Flatten() merged the blocks.
	if (!js.isLastInstruction)
	{
		// We must always update LR, even if the blocks are merged by
		// PPCAnalyst::Flatten().
		if (inst.LK)
			MOV(32, PPCSTATE_LR, Imm32(js.compilerPC + 4));
		return;
	}

	regs.Flush();

	u32 destination;
	if (inst.AA)
		destination = SignExt26(inst.LI << 2);
	else
		destination = js.compilerPC + SignExt26(inst.LI << 2);
#ifdef ACID_TEST
	if (inst.LK)
		AND(32, PPCSTATE(cr), Imm32(~(0xFF000000)));
#endif
	if (destination == js.compilerPC && !inst.LK)
	{
		if (SConfig::GetInstance().bSkipIdle)
		{
			WriteExit(js.compilerPC, ExitExceptionCheck::IDLE_AND_CHECK_ALL_EXCEPTIONS);
			return;
		}
	}
	WriteExit(destination, inst.LK, js.compilerPC + 4);
}

// TODO - optimize to hell and beyond
// TODO - make nice easy to optimize special cases for the most common
// variants of this instruction.
void Jit64::bcx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITBranchOff);

	// USES_CR

	Jit64Reg::Registers branch = regs.Branch();

	FixupBranch pCTRDontBranch;
	if ((inst.BO & BO_DONT_DECREMENT_FLAG) == 0)  // Decrement and test CTR
	{
		SUB(32, PPCSTATE_CTR, Imm8(1));
		if (inst.BO & BO_BRANCH_IF_CTR_0)
			pCTRDontBranch = J_CC(CC_NZ, true);
		else
			pCTRDontBranch = J_CC(CC_Z, true);
	}

	FixupBranch pConditionDontBranch;
	if ((inst.BO & BO_DONT_CHECK_CONDITION) == 0)  // Test a CR bit
	{
		pConditionDontBranch = JumpIfCRFieldBit(inst.BI >> 2, 3 - (inst.BI & 3),
		                                        !(inst.BO_2 & BO_BRANCH_IF_TRUE));
	}

	u32 destination;
	if (inst.AA)
		destination = SignExt16(inst.BD << 2);
	else
		destination = js.compilerPC + SignExt16(inst.BD << 2);

	branch.Flush();
	WriteExit(destination, inst.LK, js.compilerPC + 4);

	if ((inst.BO & BO_DONT_CHECK_CONDITION) == 0)
		SetJumpTarget(pConditionDontBranch);
	if ((inst.BO & BO_DONT_DECREMENT_FLAG) == 0)
		SetJumpTarget(pCTRDontBranch);

	if (!analyzer.HasOption(PPCAnalyst::PPCAnalyzer::OPTION_CONDITIONAL_CONTINUE))
	{
		regs.Flush();
		WriteExit(js.compilerPC + 4);
	}
}

void Jit64::bcctrx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITBranchOff);

	// bcctrx doesn't decrement and/or test CTR
	_dbg_assert_msg_(POWERPC, inst.BO_2 & BO_DONT_DECREMENT_FLAG, "bcctrx with decrement and test CTR option is invalid!");

	if (inst.BO_2 & BO_DONT_CHECK_CONDITION)
	{
		// BO_2 == 1z1zz -> b always

		//NPC = CTR & 0xfffffffc;
		regs.Flush();
		WriteExit(PPCSTATE_CTR, inst.LK_3, js.compilerPC + 4);
	}
	else
	{
		// Rare condition seen in (just some versions of?) Nintendo's NES Emulator

		// BO_2 == 001zy -> b if false
		// BO_2 == 011zy -> b if true

		FixupBranch b = JumpIfCRFieldBit(inst.BI >> 2, 3 - (inst.BI & 3),
		                                 !(inst.BO_2 & BO_BRANCH_IF_TRUE));
		Jit64Reg::Registers branch = regs.Branch();
		branch.Flush();
		WriteExit(PPCSTATE_CTR, inst.LK_3, js.compilerPC + 4);
		// Would really like to continue the block here, but it ends. TODO.
		SetJumpTarget(b);

		if (!analyzer.HasOption(PPCAnalyst::PPCAnalyzer::OPTION_CONDITIONAL_CONTINUE))
		{
			regs.Flush();
			WriteExit(js.compilerPC + 4);
		}
	}
}

void Jit64::bclrx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITBranchOff);

	FixupBranch pCTRDontBranch;
	if ((inst.BO & BO_DONT_DECREMENT_FLAG) == 0)  // Decrement and test CTR
	{
		SUB(32, PPCSTATE_CTR, Imm8(1));
		if (inst.BO & BO_BRANCH_IF_CTR_0)
			pCTRDontBranch = J_CC(CC_NZ, true);
		else
			pCTRDontBranch = J_CC(CC_Z, true);
	}

	Jit64Reg::Registers branch = regs.Branch();
	FixupBranch pConditionDontBranch;
	if ((inst.BO & BO_DONT_CHECK_CONDITION) == 0)  // Test a CR bit
	{
		pConditionDontBranch = JumpIfCRFieldBit(inst.BI >> 2, 3 - (inst.BI & 3),
		                                        !(inst.BO_2 & BO_BRANCH_IF_TRUE));
	}

	// This below line can be used to prove that blr "eats flags" in practice.
	// This observation could let us do some useful optimizations.
#ifdef ACID_TEST
	AND(32, PPCSTATE(cr), Imm32(~(0xFF000000)));
#endif

	branch.Flush();
	WriteExit(PPCSTATE_LR, inst.LK, js.compilerPC + 4);

	if ((inst.BO & BO_DONT_CHECK_CONDITION) == 0)
		SetJumpTarget(pConditionDontBranch);
	if ((inst.BO & BO_DONT_DECREMENT_FLAG) == 0)
		SetJumpTarget(pCTRDontBranch);

	if (!analyzer.HasOption(PPCAnalyst::PPCAnalyzer::OPTION_CONDITIONAL_CONTINUE))
	{
		regs.Flush();
		WriteExit(js.compilerPC + 4);
	}
}
