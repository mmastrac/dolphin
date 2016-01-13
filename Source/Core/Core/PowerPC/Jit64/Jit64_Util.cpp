// Copyright 2009 Dolphin Emulator Project
// Licensed under GPLv2+
// Refer to the license.txt file included.

#include "Core/PowerPC/Jit64/Jit.h"
#include "Core/PowerPC/Jit64/Jit64_Registers.h"

using namespace Gen;
using namespace Jit64Reg;

void Jit64::SafeWrite(Any& reg_value, Any& reg_addr, Any& offset, int accessSize, bool update)
{
	// Easy case: we know the address we're writing to
	if (reg_addr.IsImm())
	{
		u32 addr = reg_addr.Imm32() + offset.Imm32();
		reg_addr.SetTransactionally(Imm32(addr), update);
		WriteToConstAddress(accessSize, reg_value, addr, CallerSavedRegistersInUse());

		// exceptions
		return;
	}

	auto xa = reg_addr.Bind(update ? ReadWrite : Write);
	xa.AddTransactionally(offset, update);

	// if write clobbers value...

	SafeWriteRegToReg(reg_value, xa, accessSize, offset.Imm32(), CallerSavedRegistersInUse());
}

void Jit64::SafeWriteSwap(Any& reg_value, Any& reg_addr, Any& offset, int accessSize, bool update)
{
	// Easy case: we know the address we're writing to
	if (reg_addr.IsImm())
	{
		u32 addr = reg_addr.Imm32() + offset.Imm32();
		reg_addr.SetTransactionally(Imm32(addr), update);
		WriteToConstAddress(accessSize, reg_value, addr, CallerSavedRegistersInUse());

		// exceptions
		return;
	}

	auto xa = reg_addr.Bind(update ? ReadWrite : Write);
	xa.AddTransactionally(offset, update);

	// if write clobbers value...

	SafeWriteRegToReg(reg_value, xa, accessSize, offset.Imm32(), CallerSavedRegistersInUse());
}

void Jit64::SafeLoad(Any& reg_value, Any& reg_addr, Any& offset, int accessSize, bool signExtend, bool update)
{
}

void Jit64::SafeLoadSwap(Any& reg_value, Any& reg_addr, Any& offset, int accessSize, bool signExtend, bool update)
{
}
