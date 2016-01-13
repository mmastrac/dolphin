// Copyright 2009 Dolphin Emulator Project
// Licensed under GPLv2+
// Refer to the license.txt file included.

#include "Core/PowerPC/Jit64/Jit.h"

using namespace Gen;

void Jit64::SafeWrite(GPRRegister& reg_value, GPRRegister& reg_addr, GPRRegister& offset, int accessSize, bool swap, bool update)
{
	_assert_msg_(DYNAREC, accessSize == 8 || accessSize == 16 || accessSize == 32 || accessSize == 64, "Invalid write size: %d", accessSize);

	if (swap)
	{
		// No need to swap
		if (accessSize == 8)
		{
			swap = false;
		}

		// We can swap these ahead of time
		if (reg_value.IsImm())
		{
			swap = false;
			switch (accessSize)
			{
				case 16:
					reg_value = regs.gpr.Imm32(Common::swap16(reg_value.Imm32()));
					break;
				case 32:
					reg_value = regs.gpr.Imm32(Common::swap32(reg_value.Imm32()));
					break;
				default:
					_assert_msg_(DYNAREC, 0, "Invalid immediate size: %d", accessSize);
			}
		}
	}

	if (update)
	{
		// No need to update if the offset is zero
		if (offset.IsZero())
		{
			update = false;
		}

		// TODO: if address register is not used, don't both updating
	}

	// // Easy case: we know the address we're writing to
	// if (reg_addr.IsImm() && offset.IsImm())
	// {
	// 	WriteToConstAddress(accessSize, reg_value, reg_addr.Imm32() + offset.Imm32(), CallerSavedRegistersInUse());
	// 	return;
	// }

	// TODO: lots of optimizations in here

	// The tough case: we need to borrow a register to do all the summing
	auto scratch = regs.gpr.Borrow();
	MOV_sum(32, scratch, reg_addr, offset);

	auto scratch2 = regs.gpr.Borrow();
	MOV(accessSize, scratch2, reg_value);

	if (update)
	{
		auto xa = reg_addr.Bind(BindMode::WriteTransaction);
		MOV(32, xa, scratch);
	}

	if (swap)
		_assert_msg_(DYNAREC, 0, "swap");

	SafeWriteRegToReg(R(scratch2), scratch, accessSize, 0, CallerSavedRegistersInUse(), 0/*SAFE_LOADSTORE_NO_SWAP*/);
}

void Jit64::SafeLoad(GPRNative& reg_value, GPRRegister& reg_addr, GPRRegister& offset, int accessSize, bool signExtend, bool swap, bool update)
{
	if (swap && accessSize == 8)
	{
		swap = false;
	}

	if (swap)
	{
		// TODO: figure out what to do if you bind a borrowed reg
		auto xv = reg_value.IsRegBound() ? reg_value : reg_value.Bind(BindMode::Write);
		SafeLoad(xv, reg_addr, offset, accessSize, signExtend, true, update);
		BSWAP(accessSize, xv);
		return;
	}

	auto scratch = regs.gpr.Borrow();
	MOV_sum(32, scratch, reg_addr, offset);

	if (update)
	{
		auto xa = reg_addr.Bind(BindMode::WriteTransaction);
		MOV(32, xa, scratch);
	}

	// TODO: lots of optimizations in here

	SafeLoadToReg(reg_value, scratch, accessSize, 0, CallerSavedRegistersInUse(), signExtend, 0);
}
