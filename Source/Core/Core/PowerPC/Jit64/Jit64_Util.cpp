// Copyright 2009 Dolphin Emulator Project
// Licensed under GPLv2+
// Refer to the license.txt file included.

#include "Core/PowerPC/Jit64/Jit.h"

using namespace Gen;

void Jit64::SafeWrite(GPRRegister& reg_value, GPRRegister& reg_addr, GPRRegister& offset, int accessSize, bool swap, bool update)
{
	_assert_msg_(DYNAREC, accessSize == 8 || accessSize == 16 || accessSize == 32 || accessSize == 64, "Invalid write size: %d", accessSize);

	bool valueAddressShareRegister = reg_value.HasPPCRegister() && reg_addr.HasPPCRegister() && reg_value.PPCRegister() == reg_addr.PPCRegister();

	if (WriteClobbersRegValue(accessSize, swap))
	{
		// TODO: test for free register, otherwise swap back
		auto scratch = regs.gpr.Borrow();
		MOV(32, scratch, reg_value);
		SafeWrite(scratch, reg_addr, offset, accessSize, swap, update);
		return;
	}

	// Note that while SafeWriteRegToReg takes a register and an OpArg, it really wants things to be
	// registers or immediates.
	auto val = (reg_value.IsImm() || reg_value.IsRegBound()) ? reg_value : reg_value.Bind(BindMode::Read);

	// TODO: 

	// Optimize swap flag
	if (swap)
	{
		// No need to swap
		if (accessSize == 8)
		{
			swap = false;
		}
		else
		{
			// We can swap these ahead of time
			if (val.IsImm())
			{
				swap = false;
				switch (accessSize)
				{
					case 16:
						val = regs.gpr.Imm32(Common::swap16(val.Imm32()));
						break;
					case 32:
						val = regs.gpr.Imm32(Common::swap32(val.Imm32()));
						break;
					case 64:
						// We don't support 64-bit immediates (TODO?)
						break;
				}
			}
		}
	}

	u32 flags = swap ? 0 : SAFE_LOADSTORE_NO_SWAP;

	// Constant write (only available if swap is true)
	if (reg_addr.IsImm() && offset.IsImm())
	{
		u32 const_addr = reg_addr.Imm32() + offset.Imm32();
		if (swap)
		{
			bool exception = WriteToConstAddress(accessSize, val, const_addr, CallerSavedRegistersInUse());
			if (update)
			{
				if (jo.memcheck && exception)
				{
					auto xa = reg_addr.Bind(BindMode::ReadWrite);
					MemoryExceptionCheck();
					ADD(32, xa, offset);
				}
				else
				{
					reg_addr.SetImm32(const_addr);
				}
			}
			return;
		}
	}

	// We need to bind a register, so may as well just add the offset here
	if (reg_addr.IsImm())
	{
		if (update)
		{
			auto xa = reg_addr.Bind(BindMode::ReadWrite);
			MOV_sum(32, xa, xa, offset);
			SafeWriteRegToReg(val, xa, accessSize, 0, CallerSavedRegistersInUse(), flags);
		}
		else
		{
			auto scratch = regs.gpr.Borrow(); // can clobber RSCRATCH :/
			MOV_sum(32, scratch, reg_addr, offset);
			SafeWriteRegToReg(val, scratch, accessSize, 0, CallerSavedRegistersInUse(), flags);
		}
		return;
	}

	auto addr = reg_addr.Bind(BindMode::Read);

	// Easy: zero offset, so updates don't matter
	if (offset.IsZero())
	{
		SafeWriteRegToReg(val, addr, accessSize, 0, CallerSavedRegistersInUse(), flags);
		return;
	}

	// If we're updating we can clobber address (transactionally)
	if (update)
	{
		if (valueAddressShareRegister && val.HasPPCRegister())
		{
			auto scratch = regs.gpr.Borrow(); // can clobber RSCRATCH :/
			MOV_sum(32, scratch, reg_addr, offset);
			SafeWriteRegToReg(val, scratch, accessSize, 0, CallerSavedRegistersInUse(), flags);
			auto xa = addr.Bind(BindMode::Write);
			MOV(32, xa, scratch);
		}
		else
		{
			auto xa = addr.Bind(BindMode::ReadWrite);
			MOV_sum(32, xa, xa, offset);
			SafeWriteRegToReg(val, xa, accessSize, 0, CallerSavedRegistersInUse(), flags);
		}
		return;
	}

	// No update so we need to sum things in a scratch register. We can't just pass the offset into 
	// SafeWriteRegToReg because that will clobber the address register we pass in
	auto scratch = regs.gpr.Borrow(); // can clobber RSCRATCH :/
	MOV_sum(32, scratch, addr, offset);
	SafeWriteRegToReg(val, scratch, accessSize, 0, CallerSavedRegistersInUse(), flags);
}

void Jit64::SafeLoad(GPRNative& reg_value, GPRRegister& reg_addr, GPRRegister& offset, int accessSize, bool signExtend, bool swap, bool update)
{
	bool valueAddressShareRegister = reg_value.HasPPCRegister() && reg_addr.HasPPCRegister() && reg_value.PPCRegister() == reg_addr.PPCRegister();
	
	if (!swap && accessSize == 8)
	{
		// Swapping automatically is currently cheaper
		swap = true;
	}
	
	if (!swap)
	{
		// TODO: add a flag to disable swap in SafeLoadToReg
		SafeLoad(reg_value, reg_addr, offset, accessSize, signExtend, true, update);
		BSWAP(accessSize, reg_value);
		return;
	}

	// Easy: zero offset, so updates don't matter
	if (offset.IsZero())
	{
		if (valueAddressShareRegister)
		{
			auto scratch = regs.gpr.Borrow(RSCRATCH2);
			MOV(32, scratch, reg_addr);
			SafeLoadToReg(reg_value, scratch, accessSize, 0, CallerSavedRegistersInUse(), signExtend, 0);
		}
		else
		{
			auto addr = (reg_addr.IsImm() || reg_addr.IsRegBound()) ? reg_addr : reg_addr.Bind(BindMode::Read);
			SafeLoadToReg(reg_value, addr, accessSize, 0, CallerSavedRegistersInUse(), signExtend, 0);
		}
		return;
	}

	// If we're updating we can clobber address (transactionally)
	if (update)
	{
		if (valueAddressShareRegister)
		{
			PanicAlert("Invalid form");
		}
		else
		{
			auto xa = reg_addr.Bind(BindMode::ReadWrite);
			MOV_sum(32, xa, xa, offset);
			SafeLoadToReg(reg_value, xa, accessSize, 0, CallerSavedRegistersInUse(), signExtend, 0);
		}
		return;
	}

	// If we know the address we can pass it as an immediate
	if (reg_addr.IsImm() && offset.IsImm())
	{
		auto addr = Gen::Imm32(reg_addr.Imm32() + offset.Imm32());
		SafeLoadToReg(reg_value, addr, accessSize, 0, CallerSavedRegistersInUse(), signExtend, 0);
		return;
	}
	
	// // Not updating, but the offset is immediate
	// if (offset.IsImm())
	// {
	// 	auto addr = (reg_addr.IsImm() || reg_addr.IsRegBound()) ? reg_addr : reg_addr.Bind(BindMode::Read);
	// 	SafeLoadToReg(reg_value, addr, accessSize, offset.Imm32(), CallerSavedRegistersInUse(), signExtend, 0);
	// 	return;
	// }

	// Non-immediate offset so we need to sum things in a scratch register
	// SafeLoadToReg can clobber RSCRATCH as ABI return
	auto scratch = regs.gpr.Borrow(RSCRATCH2);
	MOV_sum(32, scratch, reg_addr, offset);
	SafeLoadToReg(reg_value, scratch, accessSize, 0, CallerSavedRegistersInUse(), signExtend, 0);
}