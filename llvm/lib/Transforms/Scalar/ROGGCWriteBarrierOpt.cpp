//===- ROGGCWriteBarrierOpt.cpp - ROG GC write barrier opts --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Scalar/ROGGCWriteBarrierOpt.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalAlias.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/ROGGC.h"
#include "llvm/InitializePasses.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/Scalar.h"
#include <cassert>

using namespace llvm;

namespace {

static FunctionCallee getOrInsertROGWriteBarrier1(Module *M, Type *ArgTy) {
  GlobalValue *Callee = M->getNamedValue("rog_write_barrier_1");
  FunctionType *Ty = FunctionType::get(Type::getVoidTy(M->getContext()),
                                       {ArgTy}, false);

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

struct ROGGCWriteBarrierOptImpl {
  static bool run(Function &F);

private:
  static bool simplifyWriteBarrierCall(CallInst *CI);
  static bool simplifyWriteBarrierCalls(Function &F);
};

} // namespace

static bool isStaticDataAddress(const Value *V) {
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

static bool isIgnorableWriteBarrierArg(const Value *V) {
  if (auto *IntVal = dyn_cast<ConstantInt>(V))
    return IntVal->isZero();

  return isStaticDataAddress(V);
}

static bool isWriteBarrier2Call(const CallInst *CI) {
  auto *Callee = dyn_cast<Function>(CI->getCalledOperand()->stripPointerCasts());
  return Callee != nullptr && Callee->getName() == "rog_write_barrier_2";
}

bool ROGGCWriteBarrierOptImpl::simplifyWriteBarrierCall(CallInst *CI) {
  assert(isWriteBarrier2Call(CI) && "expected rog_write_barrier_2 call");

  bool IgnoreOld = isIgnorableWriteBarrierArg(CI->getArgOperand(0));
  bool IgnoreNew = isIgnorableWriteBarrierArg(CI->getArgOperand(1));

  if (!IgnoreOld && !IgnoreNew)
    return false;

  if (IgnoreOld && IgnoreNew) {
    CI->eraseFromParent();
    return true;
  }

  Value *LiveArg = CI->getArgOperand(IgnoreOld ? 1 : 0);
  auto *Replacement = CallInst::Create(
      getOrInsertROGWriteBarrier1(CI->getModule(), LiveArg->getType()),
      {LiveArg}, "", CI->getIterator());
  Replacement->setCallingConv(CallingConv::ROG);
  Replacement->setTailCallKind(CI->getTailCallKind());
  Replacement->setDebugLoc(CI->getDebugLoc());
  Replacement->copyMetadata(*CI);
  CI->eraseFromParent();
  return true;
}

bool ROGGCWriteBarrierOptImpl::simplifyWriteBarrierCalls(Function &F) {
  SmallVector<CallInst *, 8> Worklist;

  for (auto &BB : F) {
    for (auto &I : BB) {
      auto *CI = dyn_cast<CallInst>(&I);
      if (CI != nullptr && isWriteBarrier2Call(CI))
        Worklist.push_back(CI);
    }
  }

  bool MadeChanges = false;
  for (CallInst *CI : Worklist)
    MadeChanges |= simplifyWriteBarrierCall(CI);
  return MadeChanges;
}

bool ROGGCWriteBarrierOptImpl::run(Function &F) {
  if (!F.hasGC() || F.getGC() != ROG_GC_NAME)
    return false;

  bool MadeChanges = simplifyWriteBarrierCalls(F);
  if (F.getAlign().valueOrOne() < 16) {
    F.setAlignment(Align(16));
    MadeChanges = true;
  }
  return MadeChanges;
}

PreservedAnalyses ROGGCWriteBarrierOptPass::run(Function &F,
                                                FunctionAnalysisManager &AM) {
  if (!ROGGCWriteBarrierOptImpl::run(F))
    return PreservedAnalyses::all();
  return PreservedAnalyses::none();
}

namespace {

class ROGGCWriteBarrierOptLegacyPass : public FunctionPass {
public:
  static char ID;

  ROGGCWriteBarrierOptLegacyPass() : FunctionPass(ID) {
    initializeROGGCWriteBarrierOptLegacyPassPass(
        *PassRegistry::getPassRegistry());
  }

  bool runOnFunction(Function &F) override {
    FunctionAnalysisManager DummyFAM;
    auto PA = Impl.run(F, DummyFAM);
    return !PA.areAllPreserved();
  }

private:
  ROGGCWriteBarrierOptPass Impl;
};

} // namespace

char ROGGCWriteBarrierOptLegacyPass::ID = 0;
INITIALIZE_PASS(ROGGCWriteBarrierOptLegacyPass, "rog-gc-write-barrier-opt",
                "ROG GC Write Barrier Optimization", false, false)

FunctionPass *llvm::createROGGCWriteBarrierOptPass() {
  return new ROGGCWriteBarrierOptLegacyPass();
}