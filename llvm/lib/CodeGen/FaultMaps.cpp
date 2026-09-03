//===- FaultMaps.cpp ------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/CodeGen/FaultMaps.h"
#include "llvm/ADT/Twine.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCSectionELF.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

#define DEBUG_TYPE "faultmaps"

static const int FaultMapVersion = 1;
const char *FaultMaps::WFMP = "Fault Maps: ";

FaultMaps::FaultMaps(AsmPrinter &AP) : AP(AP) {}

void FaultMaps::recordFaultingOp(FaultKind FaultTy,
                                 const MCSymbol *FaultingLabel,
                                 const MCSymbol *HandlerLabel) {
  MCContext &OutContext = AP.OutStreamer->getContext();

  const MCExpr *FaultingOffset = MCBinaryExpr::createSub(
      MCSymbolRefExpr::create(FaultingLabel, OutContext),
      MCSymbolRefExpr::create(AP.CurrentFnSymForSize, OutContext), OutContext);

  const MCExpr *HandlerOffset = MCBinaryExpr::createSub(
      MCSymbolRefExpr::create(HandlerLabel, OutContext),
      MCSymbolRefExpr::create(AP.CurrentFnSymForSize, OutContext), OutContext);

  FunctionInfos[AP.CurrentFnSym].emplace_back(FaultTy, FaultingOffset,
                                              HandlerOffset);
}

void FaultMaps::serializeToFaultMapSection() {
  if (FunctionInfos.empty())
    return;

  MCContext &OutContext = AP.OutStreamer->getContext();
  MCStreamer &OS = *AP.OutStreamer;

  // On ELF, emit one self-describing fault-map table per function and
  // associate it with that function's text section. This lets section GC and
  // weak/COMDAT deduplication discard a losing function's fault-map records
  // together with its code.
  if (OutContext.isELF()) {
    serializeToFaultMapSectionPerFunction();
    return;
  }

  // Create the section.
  MCSection *FaultMapSection =
      OutContext.getObjectFileInfo()->getFaultMapSection();
  OS.switchSection(FaultMapSection);

  // Emit a dummy symbol to force section inclusion.
  OS.emitLabel(OutContext.getOrCreateSymbol(Twine("__LLVM_FaultMaps")));

  LLVM_DEBUG(dbgs() << "********** Fault Map Output **********\n");

  // Header
  OS.emitIntValue(FaultMapVersion, 1); // Version.
  OS.emitIntValue(0, 1);               // Reserved.
  OS.emitInt16(0);                     // Reserved.

  LLVM_DEBUG(dbgs() << WFMP << "#functions = " << FunctionInfos.size() << "\n");
  OS.emitInt32(FunctionInfos.size());

  LLVM_DEBUG(dbgs() << WFMP << "functions:\n");

  for (const auto &FFI : FunctionInfos)
    emitFunctionInfo(FFI.first, FFI.second);
}

void FaultMaps::serializeToFaultMapSectionPerFunction() {
  MCContext &OutContext = AP.OutStreamer->getContext();
  MCStreamer &OS = *AP.OutStreamer;

  LLVM_DEBUG(dbgs() << "***** Fault Map Output (per-function) *****\n");

  for (const auto &[FnSym, FFI] : FunctionInfos) {
    const MCSymbolELF *FnSymELF = static_cast<const MCSymbolELF *>(FnSym);
    const MCSymbolELF *Group = nullptr;
    if (FnSym->isInSection())
      Group = static_cast<const MCSectionELF &>(FnSym->getSection()).getGroup();

    unsigned Flags = ELF::SHF_ALLOC | ELF::SHF_LINK_ORDER;
    if (Group)
      Flags |= ELF::SHF_GROUP;

    MCSectionELF *Sec = OutContext.getELFSection(
        ".llvm_faultmaps", ELF::SHT_PROGBITS, Flags, /*EntrySize=*/0, Group,
        /*IsComdat=*/Group != nullptr, MCSection::NonUniqueID, FnSymELF);
    OS.switchSection(Sec);

    // Each input section is independently parseable, so the linker may
    // discard any subset and concatenate the survivors in the output section.
    OS.emitIntValue(FaultMapVersion, 1); // Version.
    OS.emitIntValue(0, 1);               // Reserved.
    OS.emitInt16(0);                     // Reserved.
    OS.emitInt32(1);                     // Number of functions.
    emitFunctionInfo(FnSym, FFI);
    OS.addBlankLine();
  }
}

void FaultMaps::emitFunctionInfo(const MCSymbol *FnLabel,
                                 const FunctionFaultInfos &FFI) {
  MCStreamer &OS = *AP.OutStreamer;

  LLVM_DEBUG(dbgs() << WFMP << "  function addr: " << *FnLabel << "\n");
  OS.emitSymbolValue(FnLabel, 8);

  LLVM_DEBUG(dbgs() << WFMP << "  #faulting PCs: " << FFI.size() << "\n");
  OS.emitInt32(FFI.size());

  OS.emitInt32(0); // Reserved

  for (const auto &Fault : FFI) {
    OS.emitInt32(Fault.Kind);
    OS.emitValue(Fault.FaultingOffsetExpr, 4);
    OS.emitValue(Fault.HandlerOffsetExpr, 4);
  }
}

const char *FaultMaps::faultTypeToString(FaultMaps::FaultKind FT) {
  switch (FT) {
  default:
    llvm_unreachable("unhandled fault type!");
  case FaultMaps::FaultingLoad:
    return "FaultingLoad";
  case FaultMaps::FaultingLoadStore:
    return "FaultingLoadStore";
  case FaultMaps::FaultingStore:
    return "FaultingStore";
  }
}
