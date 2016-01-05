// Copyright 2015 Dolphin Emulator Project
// Licensed under GPLv2+
// Refer to the license.txt file included.

#pragma once

#include "Core/PowerPC/Gekko.h"
#include "Core/PowerPC/JitCommon/Jit_Util.h"
#include "Core/PowerPC/JitCommon/JitAsmCommon.h"

class QuantizedMemoryRoutines : public EmuCodeBlock
{
public:
	void GenQuantizedLoad(bool single, EQuantizeType type, int quantize);
};

class CommonAsmRoutines : public CommonAsmRoutinesBase, public QuantizedMemoryRoutines
{
protected:
	const u8* GenQuantizedLoadRuntime(bool single, EQuantizeType type);
	void GenQuantizedLoads();
	void GenQuantizedStores();
	void GenQuantizedSingleStores();

public:
	void GenFifoWrite(int size);
	void GenFrsqrte();
	void GenFres();
	void GenMfcr();
};
