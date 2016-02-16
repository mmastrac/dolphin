// Copyright 2008 Dolphin Emulator Project
// Licensed under GPLv2+
// Refer to the license.txt file included.

#pragma once

#include "Common/x64Emitter.h"
#include "Core/PowerPC/Jit64/Jit.h"
#include "Core/PowerPC/JitCommon/JitBase.h"
#include "Core/PowerPC/JitCommon/Jit_Util.h"
#include "Core/PowerPC/PPCAnalyst.h"

// The register cache manages a set of virtual registers on both the PowerPC
// and X64 side. The important concepts for the register cache are:
//
//
// Locking: a PowerPC register must be locked before you can interact with it.
// This ensures that the register does not move at any time while you may be
// potentially generating code that references it. A locked register *may*
// move under explicit instruction from a Flush(), FlushBatch(), Bind() or
// other call, but will not move indirectly as a result of requesting a
// scratch register or binding a different register.
//
// The lock is held as long as the C++ object representing the lock is in
// scope. If a lock result (the "Any" class) is assigned to another non-
// reference variable, the lifetime of that lock is at least as long as both
// of those variables.
//
// An immediate value may be assigned to a PowerPC register which binds the
// register in a special state named "away". The register can continue to be
// used as-is, but its location will resolve as a Gen::Imm32().
//
//
// Binding: for X64 instructions that require a register, a PowerPC register
// may be bound to one as needed. As with a lock, the register will not be
// moved as long as both the original lock and the bind lock (the "Native"
// class) are in scope. When a binding goes out of scope the register
// continues to hold the PowerPC register's value, but it may be flushed to
// make way for a future binding.
//
// Immediate: a virtual register with a 32-bit value (signed or unsigned) can
// be allocated by the register cache and used (read-only) exactly the same
// way as a standard register.
//
//
// Borrowing: an X64 register may be borrowed for use as a scratch register,
// or to communicate with a method that requires inputs to be in certain
// registers. If a specific register is requested to be borrowed, that
// register will be freed unless it allocated to a register that is bound and
// locked (or a previous borrow) in which case an assertion is raised.
//
// Realization: A lock, binding or borrow is not realized until the first
// operation that queries it state. This allows the register cache to shuffle
// registers around until the register's location needs to be known which
// gives it some extra flexibility in cases where register use makes things
// tight. The register cache will throw an assertion if a lock is never used
// before it goes out of scope.

namespace Jit64Reg
{

static const int NUMXREGS = 16;

using namespace Gen;

typedef size_t reg_t;

enum class BindMode
{
	// Loads a register with the contents of this register, does not mark it dirty
	Read,
	// Allocates a register without loading the guest register, marks it dirty
	Write,
	// Loads the guest register into a native register, marks it dirty
	ReadWrite,
	// Begins a transaction on this register that will either be rolled back or committed,
	// depending on the method called on Registers. This will implicitly bind the guest
	// register to a native register for the entire scope of the transaction. Attempting to
	// flush, sync or rebind this register before rollback/commit is an error.
	WriteTransaction,
	// Asserts that the register is already loaded -- doesn't load or mark it as dirty
	Reuse,
};

enum class Type
{
	// PPC GPR
	GPR,
	// PPC FPU
	FPU,
};

enum class RegisterType
{
	Bind,
	Borrow,
	Immediate,
	PPC,	
};

enum class FlushMode
{
	FlushAll,
	FlushMaintainState
};

static const std::vector<X64Reg> GPR_ALLOCATION_ORDER =
{
	// R12, when used as base register, for example in a LEA, can generate bad code! Need to look into this.
#ifdef _WIN32
	RSI, RDI, R13, R14, R15, R8, R9, R10, R11, R12, RCX
#else
	R12, R13, R14, R15, RSI, RDI, R8, R9, R10, R11, RCX
#endif
};

static const std::vector<X64Reg> GPR_SCRATCH_ALLOCATION_ORDER =
{
	RSCRATCH, RSCRATCH2
};

static const std::vector<X64Reg> FPU_ALLOCATION_ORDER =
{
	XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15, XMM2, XMM3, XMM4, XMM5
};

static const std::vector<X64Reg> FPU_SCRATCH_ALLOCATION_ORDER =
{
	XMM0, XMM1
};

// Forward
template <Type T>
class RegisterClass;
template <Type T>
class RegisterClassBase;
class Registers;
class PPC;
class Tx;
class Imm;
template <Type T>
class Native;
template <Type T>
class Any;

struct RegisterData
{
	RegisterType type;
	X64Reg xreg;
	reg_t reg;
	u32 val;
	BindMode mode;
	BitSet32 disallowed;
};

// Every register type (borrowed, bound, PowerPC and immediate) is represented
// by an Any<T> (where T is GPR or FPU). This class, however, delegates to the
// real register implementation underneath and is mainly used for lock
// management purposes.
template <Type T>
class Any
{
	friend class Registers;
	friend class Native<T>;
	friend class RegisterClass<T>;
	friend class RegisterClassBase<T>;

private:
	Any(RegisterClassBase<T>* reg, RegisterData data): m_reg(reg), m_data(data), m_realized(false)
	{
	};

	// A lock is only realized when a caller attempts to cast it to an OpArg or bind it
	void RealizeLock();

	const Gen::OpArg Location()
	{
		if (m_data.type == RegisterType::PPC || m_data.type == RegisterType::Bind)
			return m_reg->m_regs[m_data.reg].location;
		_assert_msg_(REGCACHE, 0, "Invalid operation: not a PPC register");
		return OpArg();
	}

public:
	virtual ~Any() { Unlock(); }

	Any(Any<T>&& other): m_reg(other.m_reg), m_data(other.m_data)
	{
		// Transfer the lock (but don't actually realize it again)
		if (other.m_realized)
		{
			m_realized = true;
			other.m_realized = false;
		}
		else
		{
			m_realized = false;
		}
	}

	Any(const Any<T>& other): m_reg(other.m_reg), m_data(other.m_data), m_realized(false)
	{
		// Realize this lock as well
		if (other.m_realized)
		{
			RealizeLock();
		}
	}

	Any& operator=(const Any<T>& other)
	{
		// We need to be careful in the case of self-assignment

		RegisterData old = m_data;
		m_data = other.m_data;

		// Lock "other" first
		if (other.m_realized)
		{
			Lock(m_data);
		}

		// Then unlock our data
		if (m_realized)
		{
			Unlock(old);
		}

		// Update our realization state to match other
		m_realized = other.m_realized;

		return *this;
	}

	// TODO: This shouldn't be necessary
	void ForceRealize()
	{
		RealizeLock();
	}

	bool IsImm()
	{
		// Does not realize the lock
		if (m_data.type == RegisterType::Immediate)
			return true;
		if (m_data.type == RegisterType::PPC)
			return Location().IsImm();
		return false;
	}

	bool IsZero()
	{
		// Does not realize the lock
		if (m_data.type == RegisterType::Immediate)
			return m_data.val == 0;
		if (m_data.type == RegisterType::PPC)
			return Location().IsZero();
		return false;
	}

	u32 Imm32()
	{
		// Does not realize the lock
		if (m_data.type == RegisterType::Immediate)
			return m_data.val;
		if (m_data.type == RegisterType::PPC)
			if (Location().IsImm())
				return Location().Imm32();

		_assert_msg_(REGCACHE, 0, "Invalid operation: not an immediate");
		return 0;
	}

	s32 SImm32()
	{
		// Does not realize the lock
		if (m_data.type == RegisterType::Immediate)
			return static_cast<s32>(m_data.val);
		if (m_data.type == RegisterType::PPC)
			if (Location().IsImm())
				return Location().SImm32();
		_assert_msg_(REGCACHE, 0, "Invalid operation: not an immediate");
		return 0;
	}

	// Returns the guest register represented by this class.
	u32 PPCRegister()
	{
		if (m_data.type == RegisterType::PPC || m_data.type == RegisterType::Bind)
			return m_data.reg;
		_assert_msg_(REGCACHE, 0, "Invalid operation: not a PPC register");
		return 0;
	}

	// Returns true if this register represents a guest register
	bool HasPPCRegister()
	{
		return (m_data.type == RegisterType::PPC || m_data.type == RegisterType::Bind);
	}

	void SetImm32(u32 imm);

	// If this value is an immediate, places it into a register (if there is
	// room) or back into its long-term memory location. If placed in a
	// register, the register is marked dirty.
	void RealizeImmediate();

	// If this value is not an immediate, loads it into a register. 
	void LoadIfNotImmediate();

	// Sync the current value to the default location but otherwise doesn't move the
	// register.
	OpArg Sync();
    
    // TODO: Remove this
    void Flush();

	// True if this register is available in a native register
	bool IsRegBound();

	// Forces this register into a native register. While the binding is in scope, it is an 
	// error to access this register's information.
	Native<T> Bind(BindMode mode);

	// Shortcut for common bind pattern
	Native<T> BindWriteAndReadIf(bool cond) { return Bind(cond ? BindMode::ReadWrite : BindMode::Write); }

	// Any register, bound or not, can be used as an OpArg. If not bound, this will return
	// a pointer into PPCSTATE.
	operator Gen::OpArg();

	void Lock(RegisterData& data);
	void Unlock();
	void Unlock(RegisterData& data);

private:
	RegisterClassBase<T>* m_reg;
	RegisterData m_data;
	bool m_realized;
};

// Native is a lightweight class used to manage scope like Any<T>, but adds a
// direct cast to X64Reg for borrows and binds.
template <Type T>
class Native final : public Any<T>
{
	friend class Registers;
	friend class Any<T>;
	friend class RegisterClass<T>;
	friend class RegisterClassBase<T>;

private:
	Native(RegisterClassBase<T>* reg, RegisterData data): Any<T>(reg, data) {}

public:
	// Delegate these to Any<T>
	virtual ~Native() { }
	Native(const Native& other): Any<T>(other) { }
	Native(Native&& other): Any<T>(other) { }
	Native& operator=(const Native& other)
	{
		_assert_msg_(REGCACHE, 0, "TODO operator=");
		Any<T>::operator=(other);
		return *this;
	}
	
	operator Gen::X64Reg();
};

template<Type T>
class RegisterClassBase
{
	friend class Any<T>;
	friend class Native<T>;

	struct PPCCachedReg
	{
		Gen::OpArg location;

		// This register's value not in source register, ie: location !=
		// GetDefaultLocation().
		bool away;

		// Advises the register cache that this register will be locked
		// shortly. It still can be spilled back to memory but that may be
		// expensive. This is a reference count.
		int lockAdvise;

		// A locked register is in use by the currently-generating opcode and cannot
		// be spilled back to memory. This is a reference count.
		int locked;
	};

	struct X64CachedReg
	{
		// If locked, this points back to the bound register or INVALID_REG if
		// just borrowed.
		reg_t ppcReg;

		// If bound (ie: ppcReg != INVALID_REG), indicates that this
		// register's contents are dirty and require write-back.
		bool dirty;

		// A native register is locked if it is either explictly bound to a
		// PPC register or borrowed.
		int locked;

		bool IsFree() const
		{
			return locked == 0 && ppcReg == INVALID_REG;
		}

		bool IsBound() const
		{
			return ppcReg != INVALID_REG;
		}

		bool IsBorrowed() const
		{
			return locked > 0 && ppcReg == INVALID_REG;
		}
	};

	// One array for GPR, one for FPU
protected:
	virtual const std::vector<X64Reg>& GetAllocationOrder() const = 0;
	virtual const std::vector<X64Reg>& GetScratchAllocationOrder() const = 0;
	virtual void LoadRegister(X64Reg newLoc, const OpArg& location) = 0;
	virtual void StoreRegister(const OpArg& newLoc, const OpArg& location) = 0;
	virtual OpArg GetDefaultLocation(reg_t reg) const = 0;
	virtual BitSet32 GetRegUtilization() const = 0;
	virtual BitSet32 GetRegsIn(int i) const = 0;

	X64Reg BindToRegister(reg_t preg, bool doLoad, bool makeDirty);
	void CheckUnlocked();
	BitSet32 CountRegsIn(reg_t preg, u32 lookahead);
	X64Reg GetFreeXReg(BitSet32 disallowed);
	void Init();
	int NumFreeRegisters();
	void ReleaseXReg(X64Reg xr);
	int SanityCheck() const;
	float ScoreRegister(X64Reg xr);
	void StoreFromRegister(size_t preg, FlushMode mode);

	RegisterClassBase(Gen::XEmitter* emit, JitBase* jit): m_emit(emit), m_jit(jit) {}
	void CopyFrom(RegisterClassBase<T>& other)
	{
		m_regs = other.m_regs;
		m_xregs = other.m_xregs;

		// Unlock all PPC registers
		for (auto reg : m_regs)
		{
			reg.locked = 0;
		}

		// Free all borrowed native registers
		for (auto xreg : m_xregs)
		{
			if (xreg.ppcReg == INVALID_REG)
			{
				xreg.locked = 0;
			}
		}
	}
	// TODO: not public
	void Flush();

	// TODO: not public
	void Sync();


public:
	// Borrows a host register. If 'which' is omitted, an appropriate one is chosen.
	// Note that the register that is actually borrowed is indeterminate up until
	// the actual point where a method is called that would require it to be
	// determined. It is an error to borrow a register and never access it.
	Native<T> Borrow(Gen::X64Reg which = Gen::INVALID_REG, bool allowScratch = true);

	// Special form of borrow that requests any register except those in 'which'
	Native<T> BorrowAnyBut(BitSet32 which);

	// Locks a target register for the duration of this scope. This register will
	// no longer be implicitly moved until the end of the scope. Note that the lock
	// is advisory until a method that requires current state of the register to be
	// accessed is called and a register may be moved up until that point.
	// It is an error to lock a register and never access it.
	Any<T> Lock(size_t register);

	void BindBatch(BitSet32 regs);
	void FlushBatch(BitSet32 regs);

	BitSet32 InUse() const;

protected:
	Gen::XEmitter* m_emit;
	JitBase* m_jit;
	std::array<PPCCachedReg, 32 + 32> m_regs;
	std::array<X64CachedReg, NUMXREGS> m_xregs;
};

template<Type T>
class RegisterClass
{
};

template<>
class RegisterClass<Type::FPU>: public RegisterClassBase<Type::FPU>
{
	friend class Registers;

protected:
	virtual const std::vector<X64Reg>& GetAllocationOrder() const { return FPU_ALLOCATION_ORDER; }
	virtual const std::vector<X64Reg>& GetScratchAllocationOrder() const { return FPU_SCRATCH_ALLOCATION_ORDER; }
	virtual void LoadRegister(X64Reg newLoc, const OpArg& location)
	{
		m_emit->MOVAPD(newLoc, location);
	}
	virtual void StoreRegister(const OpArg& newLoc, const OpArg& location)
	{
		m_emit->MOVAPD(newLoc, location.GetSimpleReg());
	}
	virtual OpArg GetDefaultLocation(reg_t reg) const { return PPCSTATE(ps[reg][0]); }
	virtual BitSet32 GetRegUtilization() const { return m_jit->js.op->fprInXmm; }
	virtual BitSet32 GetRegsIn(int i) const { return m_jit->js.op[i].fregsIn; }

	RegisterClass(Gen::XEmitter* emit, JitBase* jit): RegisterClassBase(emit, jit) {}
};

template<>
class RegisterClass<Type::GPR>: public RegisterClassBase<Type::GPR>
{
	friend class Registers;

protected:
	virtual const std::vector<X64Reg>& GetAllocationOrder() const { return GPR_ALLOCATION_ORDER; }
	virtual const std::vector<X64Reg>& GetScratchAllocationOrder() const { return GPR_SCRATCH_ALLOCATION_ORDER; }
	virtual void LoadRegister(X64Reg newLoc, const OpArg& location)
	{
		m_emit->MOV(32, R(newLoc), location);
	}
	virtual void StoreRegister(const OpArg& newLoc, const OpArg& location)
	{
		m_emit->MOV(32, newLoc, location);
	}
	virtual OpArg GetDefaultLocation(reg_t reg) const { return PPCSTATE(gpr[reg]); }
	virtual BitSet32 GetRegUtilization() const { return m_jit->js.op->gprInReg; }
	virtual BitSet32 GetRegsIn(int i) const { return m_jit->js.op[i].regsIn; };

	RegisterClass(Gen::XEmitter* emit, JitBase* jit): RegisterClassBase(emit, jit) {}

public:
	// A virtual register that contains zero and cannot be updated
	Any<Type::GPR> Zero() { return Imm32(0); }

	// A virtual register that contains an immediate and cannot be updated
	Any<Type::GPR> Imm32(u32 immediate);
};

using GPRRegisters = RegisterClass<Type::GPR>;
using FPURegisters = RegisterClass<Type::FPU>;

using GPRRegister = Any<Type::GPR>;
using GPRNative = Native<Type::GPR>;
using FPURegister = Any<Type::FPU>;
using FPUNative = Native<Type::FPU>;

class Registers
{
	template<Type T>
	friend class Any;
	template<Type T>
	friend class RegisterClass;
	template<Type T>
	friend class RegisterClassBase;

public:
	void Init();

	Registers(Gen::XEmitter* emit, JitBase* jit): m_emit(emit), m_jit(jit) {}

	// Create an independent copy of the register cache state for a branch
	Registers Branch();

	int SanityCheck() const;

	void Flush() { gpr.Flush(); fpu.Flush(); }

	void Commit();
	void Rollback();

private:
	Gen::XEmitter* m_emit;
	JitBase* m_jit;

public:
	FPURegisters fpu{m_emit, m_jit};
	GPRRegisters gpr{m_emit, m_jit};
};

};
