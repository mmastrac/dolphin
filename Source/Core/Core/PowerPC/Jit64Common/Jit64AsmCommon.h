// Copyright 2015 Dolphin Emulator Project
// Licensed under GPLv2+
// Refer to the license.txt file included.

#pragma once

#include "Core/PowerPC/JitCommon/JitAsmCommon.h"
#include "Core/PowerPC/JitCommon/Jit_Util.h"

enum EQuantizeType : u32;

class QuantizedMemoryRoutines : public EmuCodeBlock
{
public:
  void GenQuantizedLoad(bool single, EQuantizeType type, int quantize);
  void GenQuantizedStore(bool single, EQuantizeType type, int quantize);

private:
  void GenQuantizedLoadFloat(bool single, bool isInline);
  void GenQuantizedStoreFloat(bool single, bool isInline);
};

class CommonAsmRoutines : public CommonAsmRoutinesBase, public QuantizedMemoryRoutines
{
public:
  void GenFifoWrite(int size);
  void GenFrsqrte();
  void GenFres();
  void GenMfcr();

protected:
  const u8* GenQuantizedLoadRuntime(bool single, EQuantizeType type);
  const u8* GenQuantizedStoreRuntime(bool single, EQuantizeType type);
  void GenQuantizedLoads();
  void GenQuantizedStores();
  void GenQuantizedSingleStores();
};
