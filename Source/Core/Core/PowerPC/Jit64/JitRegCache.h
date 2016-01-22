// Copyright 2008 Dolphin Emulator Project
// Licensed under GPLv2+
// Refer to the license.txt file included.

#pragma once

#include "Common/x64Emitter.h"
#include "Core/PowerPC/Jit64/JitRegCache.h"

namespace Jit64Reg
{

static const int NUMXREGS = 16;

using namespace Gen;

typedef size_t reg_t;

enum BindMode
{
	Read,
	Write,
	ReadWrite,
	// Asserts that the register is already loaded -- doesn't load or mark it as dirty
	Reuse,
};

enum Type
{
	// PPC GPR
	GPR,
	// PPC FPU
	FPU,
	// X64 non-SSE
	X64,
	// X64 SSE
	XMM,
	// Immediate
	Imm,
};

enum XLock
{
	// This register is free for use
	Free,
	// This register has been borrowed as a scratch register
	Borrowed,
	// This register is bound to a PPC register
	Bound,
};

static const std::array<X64Reg, 11> GPR_ALLOCATION_ORDER =
{{
	// R12, when used as base register, for example in a LEA, can generate bad code! Need to look into this.
#ifdef _WIN32
	RSI, RDI, R13, R14, R15, R8, R9, R10, R11, R12, RCX
#else
	R12, R13, R14, R15, RSI, RDI, R8, R9, R10, R11, RCX
#endif
}};

static const std::array<X64Reg, 14> FPU_ALLOCATION_ORDER =
{{
	XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15, XMM2, XMM3, XMM4, XMM5
}};


// Forward
class Registers;
class PPC;
class Tx;
class Imm;
template <Type T>
class Native;
template <Type T>
class Any;

template <Type T>
class Any
{
	friend class Registers;
	friend class Native<T>;

protected:
	Registers* m_reg;

private:
	Type m_type;
	u32 m_val;

	Any(Registers* reg, Type type, u32 val): m_reg(reg), m_type(type), m_val(val) {};

public:
	Any(Any&& other): m_reg(other.m_reg), m_type(other.m_type), m_val(other.m_val) {}
	~Any() { Unlock(); }
	Any(const Any& other): m_reg(other.m_reg), m_type(other.m_type), m_val(other.m_val) {}
    Any& operator=(const Any& other) = default;

	bool IsImm();
	u32 Imm32();
	s32 SImm32();

	u32 Register()
	{
		_assert_msg_(DYNA_REC, m_type == GPR || m_type == FPU, "Cannot get a register from this");
		return m_val;
	}

	void SetImm32(u32 imm);

	// If this value is an immediate, places it into a register (if there is room) or back
	// into its long-term memory location
	void RealizeImmediate();

	void LoadIfNotImmediate();

	// True if this register is available in a native register
	bool IsRegBound();

	// Creates a transaction that commits a value if either 1) Commit is called, or 2) the transaction
	// goes out of scope without a memory exception occuring.
	// If a boolean condition is passed and the value is false, a stub transaction is 
	// created that does nothing on commit.
	void SetTransactionally(const Gen::OpArg& value, bool condition = true);
	void SetTransactionally(const Any& value, bool condition = true) { return SetTransactionally((Gen::OpArg)value, condition); }
	void SetTransactionally(u32 value, bool condition = true) { return SetTransactionally(Gen::Imm32(value), condition); }

	// Sets a transaction that commit if either 1) Commit is called, or 2) the transaction
	// goes out of scope without a memory exception occuring.
	// If a boolean condition is passed and the value is false, a stub transaction is 
	// created that does nothing on commit.
	void AddTransactionally(const Gen::OpArg& value, bool condition = true);
	void AddTransactionally(const Any& value, bool condition = true) { return AddTransactionally((Gen::OpArg)value, condition); }
	void AddTransactionally(u32 value, bool condition = true) { return AddTransactionally(Gen::Imm32(value), condition); }
	
	// Forces this register into a native register
	Native<T> Bind(BindMode mode);

	// Any register, bound or not, can be used as an OpArg. If not bound, this will return
	// a pointer into PPCSTATE.
	virtual operator Gen::OpArg() const;

	void Unlock();
};

// A register (PPC or immediate) that has been bound to a native 
template <Type T>
class Native final : public Any<T>
{
	friend class Registers;
	friend class Any<T>;

private:
	Gen::X64Reg m_xreg;

	Native(Registers* reg, Gen::X64Reg xreg): Any<T>(reg, X64, xreg), m_xreg(xreg) {}

public:
	Native(const Native& other): Any<T>(other) {}
	Native(Native&& other): Any<T>(other) {}
	~Native();// { Unlock(); }
    Native& operator=(const Native& other) = default;
	operator Gen::X64Reg() const;
};

template<Type T, std::size_t SIZE>
class RegisterClassBase
{
protected:
	virtual const std::array<X64Reg, SIZE>& GetAllocationOrder() const = 0;

public:
	// Borrows a host register. If which is omitted, an appropriate one is chosen.
	Native<T> Borrow(Gen::X64Reg which = Gen::INVALID_REG);

	// Locks a target register for the duration of this scope. This register will
	// not be moved from its current location unless it is explicitly bound.
	Any<T> Lock(size_t register);

	void BindBatch(BitSet32 regs);
	void FlushBatch(BitSet32 regs);

	BitSet32 InUse() const;
};

template<Type T, std::size_t SIZE>
class RegisterClass
{
private:
	RegisterClass() {}
};

template<std::size_t N>
class RegisterClass<FPU, N>: public RegisterClassBase<FPU, N>
{
protected:
	virtual const std::array<X64Reg, N>& GetAllocationOrder() const { return FPU_ALLOCATION_ORDER; }
};

typedef RegisterClass<FPU, 14> FPURegisters;

template<std::size_t N>
class RegisterClass<GPR, N>: public RegisterClassBase<GPR, N>
{
protected:
	virtual const std::array<X64Reg, N>& GetAllocationOrder() const { return GPR_ALLOCATION_ORDER; }

public:
	// A virtual register that contains zero and cannot be updated
	Any<GPR> Zero() { return Imm32(0); }

	// A virtual register that contains an immediate and cannot be updated
	Any<GPR> Imm32(u32 immediate);
};

typedef RegisterClass<GPR, 11> GPRRegisters;


class Registers
{
	template<Type T>
	friend class Any;
	template<Type T, std::size_t SIZE>
	friend class RegisterClass;

private:
	Gen::XEmitter* m_jit;

	struct PPCCachedReg
	{
		Gen::OpArg location;
		bool away;  // value not in source register

		// A locked register is in use by the currently generating opcode and cannot
		// be spilled back to memory
		bool locked;
	};

	struct X64CachedReg
	{
		size_t ppcReg;
		bool dirty;
		XLock lock;
	};

	// One array for GPR, one for FPU
	std::array<std::array<PPCCachedReg, 32 + 32>, 2> m_regs;
	std::array<std::array<X64CachedReg, NUMXREGS>, 2> m_xregs;

	OpArg GetDefaultLocation(Type type, reg_t reg) const;

	void LoadRegister(Type type, size_t preg, X64Reg newLoc);
	void StoreRegister(Type type, size_t preg, const OpArg& newLoc);

	BitSet32 GetRegUtilization(Type type);
	BitSet32 CountRegsIn(Type type, reg_t preg, u32 lookahead);
	void BindToRegister(Type type, reg_t preg, bool doLoad, bool makeDirty);
	float ScoreRegister(Type type, X64Reg xr);

public:
	FPURegisters fpu;
	GPRRegisters gpr;

	void Init();

	Registers(Gen::XEmitter* jit): m_jit(jit) {}

	// Create an independent copy of the register cache state for a branch
	Registers Branch();

	int SanityCheck() const;

	void Flush();

	void Commit();
	void Rollback();
};

};
