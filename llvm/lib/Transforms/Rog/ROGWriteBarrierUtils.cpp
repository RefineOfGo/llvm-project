//===- ROGWriteBarrierUtils.cpp - ROG write barrier helpers --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "ROGWriteBarrierUtils.h"
#include "llvm/Support/CommandLine.h"

using namespace llvm;

namespace llvm {
namespace rog {
namespace {

static cl::opt<unsigned> ClobberQueryBudget(
    "rog-wb-clobber-query-budget", cl::Hidden, cl::init(4),
    cl::desc("Maximum number of getClobberingMemoryAccess queries performed "
             "by each isPointerToNewlyAllocatedMemory invocation "
             "(default = 4)"));

} // namespace


// TODO: 从 malloc 开始逐条指令分析性能应该会更好，避免大量不必要的 getClobberingMemoryAccess 查询, 且函数中通常 malloc 的数量远远少于 wb 的数量
bool isPointerToNewlyAllocatedMemory(const Value *Ptr,
                                     const Instruction *BeforeInst,
                                     MemorySSA &MSSA, BatchAAResults &AA,
                                     LocationSize LocSize) {
  if (Ptr == nullptr)
    return false;

  const Value *Base = getUnderlyingObject(Ptr);
  auto *AllocCall = dyn_cast<CallBase>(Base);
  if (!isNoAliasAllocCall(AllocCall))
    return false;

  MemoryLocation Loc(Ptr, LocSize);
  auto *MA = dyn_cast_or_null<MemoryUseOrDef>(MSSA.getMemoryAccess(BeforeInst));
  if (!MA)
    return false;

  unsigned RemainingClobberQueryBudget = ClobberQueryBudget;
  MemoryAccess *Clobber = MA->getDefiningAccess();
  while (true) {
    if (RemainingClobberQueryBudget == 0)
      return false;
    --RemainingClobberQueryBudget;

    Clobber = MSSA.getWalker()->getClobberingMemoryAccess(Clobber, Loc, AA);
    if (Clobber == nullptr)
      return false;

    if (auto *MD = dyn_cast<MemoryDef>(Clobber)) {
      if (MSSA.dominates(Clobber, MSSA.getMemoryAccess(AllocCall))) {
        assert(MD->getMemoryInst() == AllocCall);
        return true;
      }
      auto *CI = dyn_cast<CallInst>(MD->getMemoryInst());
      if (CI) {
        auto *Callee =
            dyn_cast<Function>(CI->getCalledOperand()->stripPointerCasts());
        if (Callee && (Callee->getName() == "rog_write_barrier_2" ||
                       Callee->getName() == "rog_write_barrier_1" ||
                       Callee->getName() == "rog_bulk_write_barrier" ||
                       Callee->getName() == "rog_src_bulk_write_barrier")) {
          Clobber = MD->getDefiningAccess();
          continue;
        }
      }
    }

    if (isa<MemoryPhi>(Clobber))
      return false;
    return false;
  }
}

} // namespace rog
} // namespace llvm
