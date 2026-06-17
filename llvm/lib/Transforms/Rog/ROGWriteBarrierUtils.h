//===- ROGWriteBarrierUtils.h - ROG write barrier helpers -------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TRANSFORMS_ROG_ROGWRITEBARRIERUTILS_H
#define LLVM_LIB_TRANSFORMS_ROG_ROGWRITEBARRIERUTILS_H

#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/MemoryLocation.h"
#include "llvm/Analysis/MemorySSA.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalAlias.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include <cassert>

namespace llvm {
namespace rog {

inline FunctionCallee getOrInsertROGWriteBarrier1(Module *M, Type *ArgTy) {
  GlobalValue *Callee = M->getNamedValue("rog_write_barrier_1");
  FunctionType *Ty =
      FunctionType::get(Type::getVoidTy(M->getContext()), {ArgTy}, false);

  if (Callee != nullptr) {
    assert(cast<Function>(Callee)->getCallingConv() == CallingConv::ROG &&
           "rog_write_barrier_1 should have ROG calling convention");
    return FunctionCallee(Ty, Callee);
  }

  Function *Fn = Function::Create(Ty, GlobalValue::ExternalWeakLinkage,
                                  "rog_write_barrier_1", *M);
  Fn->setCallingConv(CallingConv::ROG);
  return FunctionCallee(Ty, Fn);
}

inline FunctionCallee getOrInsertROGSrcBulkWriteBarrier(Module *M, Type *PtrTy,
                                                        Type *SizeTy) {
  GlobalValue *Callee = M->getNamedValue("rog_src_bulk_write_barrier");
  FunctionType *Ty = FunctionType::get(Type::getVoidTy(M->getContext()),
                                       {PtrTy, SizeTy}, false);

  if (Callee != nullptr) {
    assert(cast<Function>(Callee)->getCallingConv() == CallingConv::ROG &&
           "rog_src_bulk_write_barrier should have ROG calling convention");
    return FunctionCallee(Ty, Callee);
  }

  Function *Fn = Function::Create(Ty, GlobalValue::ExternalWeakLinkage,
                                  "rog_src_bulk_write_barrier", *M);
  Fn->setCallingConv(CallingConv::ROG);
  return FunctionCallee(Ty, Fn);
}

inline FunctionCallee getOrInsertROGWriteBarrier2(Module *M) {
  GlobalValue *Callee = M->getNamedValue("rog_write_barrier_2");
  Type *I64 = Type::getInt64Ty(M->getContext());
  FunctionType *Ty =
      FunctionType::get(Type::getVoidTy(M->getContext()), {I64, I64}, false);

  if (Callee != nullptr) {
    assert(cast<Function>(Callee)->getCallingConv() == CallingConv::ROG &&
           "rog_write_barrier_2 should have ROG calling convention");
    return FunctionCallee(Ty, Callee);
  }

  Function *Fn = Function::Create(Ty, GlobalValue::ExternalWeakLinkage,
                                  "rog_write_barrier_2", *M);
  Fn->setCallingConv(CallingConv::ROG);
  return FunctionCallee(Ty, Fn);
}

inline bool isStaticDataAddress(const Value *V) {
  if (isa<GlobalVariable>(V) || isa<GlobalAlias>(V))
    return true;

  auto *Expr = dyn_cast<ConstantExpr>(V);
  if (Expr == nullptr)
    return false;

  switch (Expr->getOpcode()) {
  case Instruction::PtrToInt:
  case Instruction::IntToPtr:
  case Instruction::BitCast:
  case Instruction::AddrSpaceCast:
  case Instruction::GetElementPtr:
    return isStaticDataAddress(Expr->getOperand(0));
  default:
    return false;
  }
}

inline bool isConstZero(const Value *V) {
  if (auto *IntVal = dyn_cast<ConstantInt>(V))
    return IntVal->isZero();
  return false;
}

inline bool areKnownEqualI64(Value *LHS, Value *RHS) {
  assert(LHS->getType()->isIntegerTy(64) && RHS->getType()->isIntegerTy(64) &&
         "rog_write_barrier_2 arguments should be i64");
  return LHS == RHS;
}

inline bool hasAllocKind(AllocFnKind Kind, AllocFnKind Wanted) {
  return (Kind & Wanted) == Wanted;
}

inline bool isNoAliasAllocCall(const CallBase *CB) {
  if (!CB || !CB->hasRetAttr(Attribute::NoAlias))
    return false;
  Attribute AllocKindAttr = CB->getFnAttr(Attribute::AllocKind);
  if (!AllocKindAttr.isValid())
    return false;
  AllocFnKind Kind = static_cast<AllocFnKind>(AllocKindAttr.getValueAsInt());
  return hasAllocKind(Kind, AllocFnKind::Alloc);
}

bool isPointerToNewlyAllocatedMemory(const Value *Ptr,
                                     const Instruction *BeforeInst,
                                     MemorySSA &MSSA, BatchAAResults &AA,
                                     LocationSize LocSize);

inline bool isWriteBarrier2Call(const CallInst *CI) {
  auto *Callee =
      dyn_cast<Function>(CI->getCalledOperand()->stripPointerCasts());
  return Callee != nullptr && Callee->getName() == "rog_write_barrier_2";
}

inline bool isBulkWriteBarrierCall(const CallInst *CI) {
  auto *Callee =
      dyn_cast<Function>(CI->getCalledOperand()->stripPointerCasts());
  return Callee != nullptr && Callee->getName() == "rog_bulk_write_barrier";
}

bool decomposeBulkWriteBarriers(Function &F);
bool removeFreezesFromArgumentLoads(Function &F);
bool simplifyBulkWriteBarrierCalls(Function &F, MemorySSA &MSSA,
                                   BatchAAResults &AA);
bool simplifyWriteBarrier2Calls(Function &F, MemorySSA &MSSA,
                                BatchAAResults &AA);

} // namespace rog
} // namespace llvm

#endif
