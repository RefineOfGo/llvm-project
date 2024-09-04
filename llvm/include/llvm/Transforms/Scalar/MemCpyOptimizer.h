//===- MemCpyOptimizer.h - memcpy optimization ------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This pass performs various transformations related to eliminating memcpy
// calls, or transforming sets of stores into memset's.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_SCALAR_MEMCPYOPTIMIZER_H
#define LLVM_TRANSFORMS_SCALAR_MEMCPYOPTIMIZER_H

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/PassManager.h"

namespace llvm {

class AAResults;
class AllocaInst;
class BatchAAResults;
class AssumptionCache;
class CallBase;
class CallInst;
class DominatorTree;
class Function;
class Instruction;
class LoadInst;
class MemorySSA;
class MemorySSAUpdater;
class NonAtomicMemCpyInst;
class NonAtomicMemMoveInst;
class NonAtomicMemSetInst;
class PostDominatorTree;
class StoreInst;
class TargetLibraryInfo;
class TypeSize;
class Value;

class MemCpyOptPass : public PassInfoMixin<MemCpyOptPass> {
  TargetLibraryInfo *TLI = nullptr;
  AAResults *AA = nullptr;
  AssumptionCache *AC = nullptr;
  DominatorTree *DT = nullptr;
  PostDominatorTree *PDT = nullptr;
  MemorySSA *MSSA = nullptr;
  MemorySSAUpdater *MSSAU = nullptr;

public:
  MemCpyOptPass() = default;

  PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);

  // Glue for the old PM.
  bool runImpl(Function &F, TargetLibraryInfo *TLI, AAResults *AA,
               AssumptionCache *AC, DominatorTree *DT, PostDominatorTree *PDT,
               MemorySSA *MSSA);

private:
  // Helper functions
  bool processStore(StoreInst *SI, BasicBlock::iterator &BBI);
  bool processStoreOfLoad(StoreInst *SI, LoadInst *LI, const DataLayout &DL,
                          BasicBlock::iterator &BBI);
  bool processMemSet(NonAtomicMemSetInst *SI, BasicBlock::iterator &BBI);
  bool processMemCpy(NonAtomicMemCpyInst *M, BasicBlock::iterator &BBI);
  bool processMemMove(NonAtomicMemMoveInst *M);
  bool performCallSlotOptzn(Instruction *cpyLoad, Instruction *cpyStore,
                            Value *cpyDst, Value *cpySrc, TypeSize cpyLen,
                            Align cpyAlign, BatchAAResults &BAA,
                            std::function<CallInst *()> GetC);
  bool processMemCpyMemCpyDependence(NonAtomicMemCpyInst *M,
                                     NonAtomicMemCpyInst *MDep,
                                     BatchAAResults &BAA);
  bool processMemSetMemCpyDependence(NonAtomicMemCpyInst *MemCpy,
                                     NonAtomicMemSetInst *MemSet,
                                     BatchAAResults &BAA);
  bool performMemCpyToMemSetOptzn(NonAtomicMemCpyInst *MemCpy,
                                  NonAtomicMemSetInst *MemSet,
                                  BatchAAResults &BAA);
  bool processByValArgument(CallBase &CB, unsigned ArgNo);
  bool processImmutArgument(CallBase &CB, unsigned ArgNo);
  Instruction *tryMergingIntoMemset(Instruction *I, Value *StartPtr,
                                    Value *ByteVal);
  bool moveUp(StoreInst *SI, Instruction *P, const LoadInst *LI);
  bool performStackMoveOptzn(Instruction *Load, Instruction *Store,
                             AllocaInst *DestAlloca, AllocaInst *SrcAlloca,
                             TypeSize Size, BatchAAResults &BAA);

  void eraseInstruction(Instruction *I);
  bool iterateOnFunction(Function &F);
};

} // end namespace llvm

#endif // LLVM_TRANSFORMS_SCALAR_MEMCPYOPTIMIZER_H
