//=== X86CallingConv.h - X86 Custom Calling Convention Routines -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the custom routines for the X86 Calling Convention that
// aren't done by tablegen.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_X86_X86CALLINGCONV_H
#define LLVM_LIB_TARGET_X86_X86CALLINGCONV_H

#include "MCTargetDesc/X86MCTargetDesc.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/IR/CallingConv.h"

namespace llvm {

inline unsigned X86GoABI0StackSlotSize(MVT VT) {
  unsigned Bits = VT.getFixedSizeInBits();
  if (Bits == 0)
    return 8;
  return (Bits + 7) / 8;
}

inline Align X86GoABI0StackSlotAlign(MVT VT) {
  unsigned Size = X86GoABI0StackSlotSize(VT);
  if (Size >= 8)
    return Align(8);
  if (Size >= 4)
    return Align(4);
  if (Size >= 2)
    return Align(2);
  return Align(1);
}

bool RetCC_X86(unsigned ValNo, MVT ValVT, MVT LocVT,
               CCValAssign::LocInfo LocInfo, ISD::ArgFlagsTy ArgFlags,
               Type *OrigTy, CCState &State);

bool CC_X86(unsigned ValNo, MVT ValVT, MVT LocVT, CCValAssign::LocInfo LocInfo,
            ISD::ArgFlagsTy ArgFlags, Type *OrigTy, CCState &State);

} // End llvm namespace

#endif
