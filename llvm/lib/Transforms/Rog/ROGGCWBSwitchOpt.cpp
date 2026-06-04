//===- ROGGCWBSwitchOpt.cpp - ROG GCWB switch opts -----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Rog/ROGGCWBSwitchOpt.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalAlias.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/ROGGC.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CommandLine.h"
#include <algorithm>

using namespace llvm;

#define DEBUG_TYPE "rog-gcwb-switch-opt"

STATISTIC(NumRedundantLoads,
          "Number of redundant ROG_GCWB_SWITCH loads removed");

static cl::opt<uint32_t> LoadBudget(
    "rog-gcwb-switch-opt-budget", cl::Hidden, cl::init(32),
    cl::desc("Instruction budget for reusing older ROG_GCWB_SWITCH loads "
             "inside the dataflow analysis (default = 32)"));

namespace {

static bool shouldOptimizeROGFunction(Function &F) {
  if (F.isDeclaration() || !F.hasGC() || F.getGC() != ROG_GC_NAME)
    return false;
  return !F.hasOptNone();
}

using BudgetTy = uint32_t;
using BudgetState = SmallVector<BudgetTy, 16>;

static Value *getI8GlobalObject(Value *Switch) {
  if (Switch == nullptr)
    return nullptr;

  Value *SwitchObject = Switch->stripPointerCastsAndAliases();
  auto *SwitchGlobal = dyn_cast<GlobalVariable>(SwitchObject);
  if (SwitchGlobal == nullptr || !SwitchGlobal->getValueType()->isIntegerTy(8))
    return nullptr;

  return SwitchObject;
}

static Value *getROGGCWBSwitchObject(Module &M) {
  if (Value *SwitchObject =
          getI8GlobalObject(M.getNamedValue("ROG_GCWB_SWITCH")))
    return SwitchObject;
  return nullptr;
}

static bool isROGGCWBSwitchLoad(const LoadInst *LI, const Value *SwitchObject) {
  return LI->getPointerOperand()->stripPointerCastsAndAliases() == SwitchObject;
}

static bool isNonBlockingCall(const Function *Callee) {
  if (Callee == nullptr)
    return false;

  return StringSwitch<bool>(Callee->getName())
      .Cases(
          {
              // write barrier
              "pre_write_1",
              "pre_write_2",
              "bulk_pre_write",
              // bulk write barrier
              "dst_bulk_write_barrier",
              "src_bulk_write_barrier",
          },
          true)
      .Default(false);
}

static bool isBlockingCall(const Instruction &I) {
  auto *CB = dyn_cast<CallBase>(&I);
  if (!CB)
    return false;
  if (isa<DbgInfoIntrinsic>(CB))
    return false;
  const Function *Callee = CB->getCalledFunction();
  if (isNonBlockingCall(Callee))
    return false;
  // conservatively assume any call we don't recognize is a blocking call.
  return true;
}

static LoadInst *findRootReplacement(LoadInst *Load,
                                     DenseMap<LoadInst *, LoadInst *> &Repl) {
  SmallPtrSet<LoadInst *, 8> Seen;
  while (LoadInst *Next = Repl.lookup(Load)) {
    if (!Seen.insert(Load).second)
      break;
    Load = Next;
  }
  return Load;
}

static BudgetState getInitialBudgetState(unsigned NumLoads,
                                         BudgetTy Value = 0) {
  return BudgetState(NumLoads, Value);
}

static void clearBudgetState(BudgetState &State) {
  for (BudgetTy &Budget : State)
    Budget = 0;
}

static void decayBudgetState(BudgetState &State) {
  for (BudgetTy &Budget : State)
    if (Budget != 0)
      --Budget;
}

static void intersectBudgetState(BudgetState &State, const BudgetState &Other) {
  assert(State.size() == Other.size() && "state sizes must match");
  for (unsigned I = 0, E = State.size(); I != E; ++I)
    State[I] = std::min(State[I], Other[I]);
}

static void
transferInstruction(Instruction &I, BudgetState &State,
                    const DenseMap<LoadInst *, unsigned> &LoadToIndex) {
  if (isa<DbgInfoIntrinsic>(I))
    return;

  if (isBlockingCall(I)) {
    clearBudgetState(State);
    return;
  }

  decayBudgetState(State);

  auto *LI = dyn_cast<LoadInst>(&I);
  if (LI == nullptr)
    return;

  auto It = LoadToIndex.find(LI);
  if (It != LoadToIndex.end())
    State[It->second] = LoadBudget;
}

static void transferBlock(BasicBlock &BB, BudgetState &State,
                          const DenseMap<LoadInst *, unsigned> &LoadToIndex) {
  for (Instruction &I : BB)
    transferInstruction(I, State, LoadToIndex);
}

static void computeAvailableLoadStates(
    Function &F, const DenseMap<LoadInst *, unsigned> &LoadToIndex,
    unsigned NumLoads, DenseMap<BasicBlock *, BudgetState> &In) {
  DenseMap<BasicBlock *, BudgetState> Out;
  BudgetState Zero = getInitialBudgetState(NumLoads, 0);
  BudgetState Top = getInitialBudgetState(NumLoads, LoadBudget);

  for (BasicBlock &BB : F) {
    In[&BB] = &BB == &F.getEntryBlock() ? Zero : Top;
    Out[&BB] = Top;
  }

  bool Changed = true;
  while (Changed) {
    Changed = false;

    for (BasicBlock &BB : F) {
      BudgetState NewIn = Zero;
      if (&BB != &F.getEntryBlock()) {
        bool SawPred = false;
        for (BasicBlock *Pred : predecessors(&BB)) {
          if (!SawPred) {
            NewIn = Out[Pred];
            SawPred = true;
          } else {
            intersectBudgetState(NewIn, Out[Pred]);
          }
        }
      }

      BudgetState NewOut = NewIn;
      transferBlock(BB, NewOut, LoadToIndex);

      if (NewIn != In[&BB]) {
        In[&BB] = NewIn;
        Changed = true;
      }
      if (NewOut != Out[&BB]) {
        Out[&BB] = NewOut;
        Changed = true;
      }
    }
  }
}

static LoadInst *findBestActiveReplacement(const BudgetState &State,
                                           ArrayRef<LoadInst *> Loads,
                                           unsigned CurrentIndex) {
  LoadInst *Best = nullptr;
  BudgetTy BestBudget = 0;

  for (unsigned I = 0, E = Loads.size(); I != E; ++I) {
    if (I == CurrentIndex || State[I] == 0)
      continue;

    if (Best == nullptr || BestBudget < State[I]) {
      Best = Loads[I];
      BestBudget = State[I];
    }
  }

  return Best;
}

static bool optimizeFunction(Function &F, const Value *SwitchObject) {
  SmallVector<LoadInst *, 16> Loads;
  DenseMap<LoadInst *, unsigned> LoadToIndex;
  for (BasicBlock &BB : F) {
    for (Instruction &I : BB)
      if (auto *LI = dyn_cast<LoadInst>(&I))
        if (isROGGCWBSwitchLoad(LI, SwitchObject)) {
          LoadToIndex[LI] = Loads.size();
          Loads.push_back(LI);
        }
  }

  if (Loads.size() < 2)
    return false;

  DenseMap<BasicBlock *, BudgetState> In;
  computeAvailableLoadStates(F, LoadToIndex, Loads.size(), In);

  DenseMap<LoadInst *, LoadInst *> Replacements;
  for (BasicBlock &BB : F) {
    BudgetState State = In[&BB];
    for (Instruction &I : BB) {
      auto *LI = dyn_cast<LoadInst>(&I);
      if (LI != nullptr) {
        auto It = LoadToIndex.find(LI);
        if (It != LoadToIndex.end()) {
          unsigned CurrentIndex = It->second;
          if (LoadInst *Replacement =
                  findBestActiveReplacement(State, Loads, CurrentIndex))
            Replacements[LI] = Replacement;
        }
      }

      transferInstruction(I, State, LoadToIndex);
    }
  }

  if (Replacements.empty())
    return false;

  for (auto &Entry : Replacements)
    Entry.second = findRootReplacement(Entry.second, Replacements);

  for (auto &Entry : Replacements) {
    LoadInst *Load = Entry.first;
    LoadInst *Replacement = Entry.second;
    Load->replaceAllUsesWith(Replacement);
  }

  for (LoadInst *Load : Loads) {
    if (!Replacements.contains(Load))
      continue;
    Load->eraseFromParent();
    ++NumRedundantLoads;
  }
  return true;
}

} // namespace

PreservedAnalyses ROGGCWBSwitchOptPass::run(Function &F,
                                            FunctionAnalysisManager &AM) {
  (void)AM;

  if (!shouldOptimizeROGFunction(F))
    return PreservedAnalyses::all();

  Value *SwitchObject = getROGGCWBSwitchObject(*F.getParent());
  if (SwitchObject == nullptr)
    return PreservedAnalyses::all();

  if (!optimizeFunction(F, SwitchObject))
    return PreservedAnalyses::all();

  PreservedAnalyses PA;
  PA.preserveSet<CFGAnalyses>();
  return PA;
}
