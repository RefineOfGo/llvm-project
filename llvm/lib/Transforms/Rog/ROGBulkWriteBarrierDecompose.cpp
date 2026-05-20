//===- ROGBulkWriteBarrierDecompose.cpp - ROG bulk WB split --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "ROGWriteBarrierUtils.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Metadata.h"

using namespace llvm;

namespace llvm {
namespace rog {

static bool decomposeBulkWriteBarrierWithPtrMap(CallInst *CI) {
  assert(isBulkWriteBarrierCall(CI) && "expected rog_bulk_write_barrier call");

  unsigned PtrMapKind =
      CI->getContext().getMDKindID("rog.bulk_write_barrier.ptrmap");
  MDNode *MD = CI->getMetadata(PtrMapKind);
  if (!MD)
    return false;
  assert(MD->getNumOperands() == 1 && "invalid ptrmap metadata");
  auto *StrMD = cast<MDString>(MD->getOperand(0));

  StringRef Bitmap = StrMD->getString();
  if (Bitmap.empty())
    return false;

  Value *SizeVal = CI->getArgOperand(2);
  auto *SizeConst = dyn_cast<ConstantInt>(SizeVal);
  if (!SizeConst)
    return false;

  uint64_t Size = SizeConst->getZExtValue();
  uint64_t ObjectSize = 8 * Bitmap.size();

  if (ObjectSize == 0 || Size % ObjectSize != 0)
    return false;

  uint64_t NumObjects = Size / ObjectSize;

  SmallVector<unsigned, 8> PointerPositions;
  for (unsigned Pos = 0; Pos < Bitmap.size(); ++Pos)
    if (Bitmap[Pos] == '1')
      PointerPositions.push_back(Pos);

  uint64_t TotalPtrs = PointerPositions.size() * NumObjects;
  if (TotalPtrs == 0 || TotalPtrs > 4)
    return false;

  Value *Dest = CI->getArgOperand(0);
  Value *Src = CI->getArgOperand(1);
  Type *I64 = Type::getInt64Ty(CI->getContext());
  Type *I8 = Type::getInt8Ty(CI->getContext());
  const Align PtrAlign(8);

  FunctionCallee WB2 = getOrInsertROGWriteBarrier2(CI->getModule());

  IRBuilder<> Builder(CI);
  Builder.SetCurrentDebugLocation(CI->getDebugLoc());

  for (uint64_t ObjIdx = 0; ObjIdx < NumObjects; ++ObjIdx) {
    for (unsigned Pos : PointerPositions) {
      uint64_t Offset = ObjIdx * ObjectSize + Pos * 8;

      Value *DstSlot = Builder.CreateGEP(I8, Dest, Builder.getInt64(Offset));
      Value *SrcSlot = Builder.CreateGEP(I8, Src, Builder.getInt64(Offset));

      LoadInst *OldLoad = Builder.CreateAlignedLoad(I64, DstSlot, PtrAlign);
      Value *OldFrozen = Builder.CreateFreeze(OldLoad);
      LoadInst *NewLoad = Builder.CreateAlignedLoad(I64, SrcSlot, PtrAlign);

      CallInst *WB2Call = Builder.CreateCall(WB2, {OldFrozen, NewLoad});
      WB2Call->setCallingConv(CallingConv::ROG);
      WB2Call->setTailCallKind(CI->getTailCallKind());
    }
  }

  CI->eraseFromParent();
  return true;
}

bool decomposeBulkWriteBarriers(Function &F) {
  SmallVector<CallInst *, 8> BulkWorklist;

  for (auto &BB : F) {
    for (auto &I : BB) {
      auto *CI = dyn_cast<CallInst>(&I);
      if (CI && isBulkWriteBarrierCall(CI))
        BulkWorklist.push_back(CI);
    }
  }

  bool MadeChanges = false;
  for (CallInst *CI : BulkWorklist) {
    if (CI->getArgOperand(0) == CI->getArgOperand(1)) {
      CI->eraseFromParent();
      MadeChanges = true;
      continue;
    }
    MadeChanges |= decomposeBulkWriteBarrierWithPtrMap(CI);
  }
  return MadeChanges;
}

} // namespace rog
} // namespace llvm