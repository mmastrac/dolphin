// Copyright 2009 Dolphin Emulator Project
// Licensed under GPLv2+
// Refer to the license.txt file included.

#include "Core/PowerPC/Jit64/Jit.h"
#include "Core/PowerPC/Jit64/Jit64_Registers.h"

using namespace Gen;

namespace Jit64Reg 
{

void Registers::Init() 
{
	for (Type type : std::array<Type, 2> {{ GPR, FPU }})
	{
		for (auto& xreg : m_xregs[type])
		{
			xreg.lock = Free;
			xreg.dirty = false;
			xreg.ppcReg = INVALID_REG;
		}
		for (auto& reg : m_regs[type])
		{
			reg.location = GetDefaultLocation(type, &reg - &m_regs[type][0]);
			reg.away = false;
			reg.locked = false;
		}
	}
}

int Registers::SanityCheck() const
{
	return 0;
}

BitSet32 Registers::InUse(Type type) const
{
	// for (int i = 0; i < NUMXREGS; i++)
	// {
	// 	if (!gpr.IsFreeX(i))
	// 		result[i] = true;
	// 	if (!fpr.IsFreeX(i))
	// 		result[16 + i] = true;
	// }
	// return result & ABI_ALL_CALLER_SAVED;
	return (BitSet32)0;
}

int Registers::NumFreeRegisters()
{
	// int count = 0;
	// size_t aCount;
	// const X64Reg* aOrder = GetAllocationOrder(&aCount);
	// for (size_t i = 0; i < aCount; i++)
	// 	if (!xregs[aOrder[i]].locked && xregs[aOrder[i]].free)
	// 		count++;
	// return count;
	return 0;
}

void Registers::BindBatch(Type type, BitSet32 regs)
{
	for (int reg : regs)
	{
		// if (NumFreeRegisters() < 2)
		// 	break;
		// if (ops[i].gprInReg[reg] && !gpr.R(reg).IsImm())
		// 	gpr.BindToRegister(reg, true, false);
	}
}

OpArg Registers::GetDefaultLocation(Type type, reg_t reg) const
{
	return type == GPR ? PPCSTATE(gpr[reg]) : PPCSTATE(ps[reg][0]);
}

void Registers::LoadRegister(Type type, size_t preg, X64Reg newLoc)
{
	if (type == GPR)
		m_jit->MOV(32, R(newLoc), m_regs[type][preg].location);
	else
		m_jit->MOVAPD(newLoc, m_regs[type][preg].location);
}

void Registers::StoreRegister(Type type, size_t preg, const OpArg& newLoc)
{
	if (type == GPR)
		m_jit->MOV(32, newLoc, m_regs[type][preg].location);
	else
		m_jit->MOVAPD(newLoc, m_regs[type][preg].location.GetSimpleReg());
}

BitSet32 Registers::GetRegUtilization(Type type)
{
	_assert_msg_(DYNA_REC, type == GPR || type == FPU, "Invalid type, expect a crash soon");
	return type == GPR ? jit->js.op->gprInReg : jit->js.op->fprInXmm;
}

BitSet32 Registers::CountRegsIn(Type type, reg_t preg, u32 lookahead)
{
	_assert_msg_(DYNA_REC, type == GPR || type == FPU, "Invalid type, expect a crash soon");
	BitSet32 regsUsed;
	for (u32 i = 1; i < lookahead; i++)
	{
		BitSet32 regsIn = type == GPR ? jit->js.op[i].regsIn : jit->js.op[i].fregsIn;
		regsUsed |= regsIn;
		if (regsIn[preg])
			return regsUsed;
	}
	return regsUsed;
}

void Registers::StoreFromRegister(Type type, size_t preg, FlushMode mode)
{
	auto& ppc = m_regs[type][preg];

	if (ppc.away)
	{
		bool doStore;
		if (ppc.location.IsSimpleReg())
		{
			X64Reg xr = RX(type, preg);
			auto& xreg = m_xregs[type][xr];
			doStore = xreg.dirty;
			if (mode == FLUSH_ALL)
			{
				xreg.lock = Free;
				xreg.ppcReg = INVALID_REG;
				xreg.dirty = false;
			}
		}
		else
		{
			//must be immediate - do nothing
			doStore = true;
		}
		OpArg newLoc = GetDefaultLocation(type, preg);
		if (doStore)
			StoreRegister(type, preg, newLoc);
		if (mode == FLUSH_ALL)
		{
			ppc.location = newLoc;
			ppc.away = false;
		}
	}
}

void Registers::BindToRegister(Type type, reg_t preg, bool doLoad, bool makeDirty)
{
	auto& ppc = m_regs[type][preg];

	if (!ppc.away && ppc.location.IsImm())
		PanicAlert("Bad immediate");

	if (!ppc.away || (ppc.away && ppc.location.IsImm()))
	{
		X64Reg xr = GetFreeXReg(type);
		auto& xreg = m_xregs[type][xr];

		// Sanity check
		_assert_msg_(DYNA_REC, !xreg.dirty, "Xreg already dirty");
		_assert_msg_(DYNA_REC, xreg.lock != Borrowed, "GetFreeXReg returned borrowed register");
		for (PPCCachedReg& reg : m_regs[type])
			if (reg.location.IsSimpleReg(xr))
				Crash();

		xreg.lock = Bound;
		xreg.ppcReg = preg;
		xreg.dirty = makeDirty || ppc.location.IsImm();
		if (doLoad)
			LoadRegister(type, preg, xr);

		ppc.away = true;
		ppc.location = R(xr);
	}
	else
	{
		// reg location must be simplereg; memory locations
		// and immediates are taken care of above.
		m_xregs[type][RX(type, preg)].dirty |= makeDirty;
	}

	_assert_msg_(DYNA_REC, m_xregs[type][RX(type, preg)].lock == Borrowed, "This reg should have been flushed");
}

// Estimate roughly how bad it would be to de-allocate this register. Higher score
// means more bad.
float Registers::ScoreRegister(Type type, X64Reg xr)
{
	size_t preg = m_xregs[type][xr].ppcReg;
	float score = 0;

	// If it's not dirty, we don't need a store to write it back to the register file, so
	// bias a bit against dirty registers. Testing shows that a bias of 2 seems roughly
	// right: 3 causes too many extra clobbers, while 1 saves very few clobbers relative
	// to the number of extra stores it causes.
	if (m_xregs[type][xr].dirty)
		score += 2;

	// If the register isn't actually needed in a physical register for a later instruction,
	// writing it back to the register file isn't quite as bad.
	if (GetRegUtilization(type)[preg])
	{
		// Don't look too far ahead; we don't want to have quadratic compilation times for
		// enormous block sizes!
		// This actually improves register allocation a tiny bit; I'm not sure why.
		u32 lookahead = std::min(jit->js.instructionsLeft, 64);
		// Count how many other registers are going to be used before we need this one again.
		u32 regs_in_count = CountRegsIn(type, preg, lookahead).Count();
		// Totally ad-hoc heuristic to bias based on how many other registers we'll need
		// before this one gets used again.
		score += 1 + 2 * (5 - log2f(1 + (float)regs_in_count));
	}

	return score;
}

X64Reg Registers::GetFreeXReg(Type type)
{
	if (type == GPR)
		return GetFreeXReg(type, GPR_ALLOCATION_ORDER);
	return GetFreeXReg(type, FPU_ALLOCATION_ORDER);
}

template <std::size_t SIZE>
X64Reg Registers::GetFreeXReg(Type type, std::array<X64Reg, SIZE> order)
{
	auto& xregs = m_xregs[type];
	auto& regs = m_regs[type];

	// Are there any X64 registers of this type that are currently unlocked 
	// (ie: not borrowed) and free (ie: not bound)?
	for (X64Reg xr : order)
		if (xregs[xr].lock == Free)
			return xr;

	// Okay, not found; run the register allocator heuristic and figure out which register we should
	// clobber.
	float min_score = std::numeric_limits<float>::max();
	X64Reg best_xreg = INVALID_REG;
	size_t best_preg = 0;
	for (X64Reg xreg : order)
	{
		// Borrowed X64 regs aren't available
		if (xregs[xreg].lock == Borrowed)
			continue;

		// Must be bound
		size_t preg = xregs[xreg].ppcReg;
		if (regs[preg].locked)
			continue;

		float score = ScoreRegister(type, xreg);
		if (score < min_score)
		{
			min_score = score;
			best_xreg = xreg;
			best_preg = preg;
		}
	}

	if (best_xreg != INVALID_REG)
	{
		StoreFromRegister(type, best_preg, FLUSH_ALL);
		return best_xreg;
	}

	//Still no dice? Die!
	_assert_msg_(DYNA_REC, 0, "Regcache ran out of regs");
	return INVALID_REG;
}

Any Registers::Lock(Type type, size_t reg)
{
	_assert_msg_(DYNA_REC, type == GPR || type == FPU, "Invalid type, expect a crash soon");
	m_regs[type][reg].locked = true;
	return Any(this, type, reg);
}

Any Registers::Imm32(u32 value)
{
	return Any(this, Imm, value);
}

Native Registers::Borrow(Type type, X64Reg reg)
{
	printf("borrow %d\n", reg);
	if (type == GPR && reg == RSCRATCH_EXTRA)
	{
		// todo
		// m_gpr->FlushLockX(RSCRATCH_EXTRA);
	}

	return Native(this, reg);
}


Native::operator X64Reg() const
{
	return m_xreg;
}


Any::operator OpArg() const
{
	return m_reg->m_regs[m_type][m_val].location;
}

Native Any::Bind(BindMode mode)
{
	_assert_msg_(DYNA_REC, m_type == GPR || m_type == FPU, "Cannot bind a native or immediate register");

	switch (mode)
	{
	case Read:
		m_reg->BindToRegister(m_type, m_val, true, false);
		break;
	case ReadWrite:
		m_reg->BindToRegister(m_type, m_val, true, true);
		break;
	case Write:
		m_reg->BindToRegister(m_type, m_val, false, true);
		break;
	}

	return Native(m_reg, m_reg->m_regs[m_type][m_val].location.GetSimpleReg());
}

void Any::SetTransactionally(const OpArg& value, bool condition)
{
	// if (condition)
	// {
	// 	RegCache* which = m_type == GPR ? (RegCache*)m_reg->m_gpr : (RegCache*)m_reg->m_fpu;

	// 	if (value.IsImm())
	// 	{
	// 		// assert type
	// 		m_reg->m_gpr->SetImmediate32(m_val, value.Imm32());
	// 	}
	// 	else
	// 	{
	// 		which->KillImmediate(m_val, true, true);
	// 		m_reg->m_jit->MOV(32, OpArg(), value);
	// 	}
	// }
}

void Any::AddTransactionally(const OpArg& value, bool condition)
{
	if (condition)
		m_reg->m_jit->ADD(32, OpArg(), value);
}

void Any::Unlock()
{
	auto& ppc = m_reg->m_regs[m_type][m_val];
	_assert_msg_(DYNA_REC, ppc.locked, "Attempted to unlock a register that was never locked");
	ppc.locked = false;
}

};
