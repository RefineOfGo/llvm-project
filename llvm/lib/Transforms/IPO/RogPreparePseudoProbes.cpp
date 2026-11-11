//===- RogPreparePseudoProbes.cpp -----------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/IPO/RogPreparePseudoProbes.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/DebugProgramInstruction.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

PreservedAnalyses RogPreparePseudoProbesPass::run(Module &M,
                                                  ModuleAnalysisManager &) {
  if (M.getNamedMetadata("llvm.pseudo_probe_desc") ||
      M.getFunction("llvm.pseudoprobe"))
    report_fatal_error("ROG post-optimization probes require unprobed IR");

  std::unique_ptr<DIBuilder> Builder;
  DIFile *SyntheticFile = nullptr;
  for (Function &F : M) {
    if (F.isDeclaration())
      continue;
    // Probe locations need a function scope even when debug info is absent.
    if (!F.getSubprogram()) {
      if (!Builder) {
        Builder = std::make_unique<DIBuilder>(M);
        SyntheticFile = Builder->createFile(M.getSourceFileName(), ".");
        Builder->createCompileUnit(DISourceLanguageName(dwarf::DW_LANG_Rust),
                                   SyntheticFile, "ROG runtime BC", true, "", 0,
                                   "", DICompileUnit::LineTablesOnly);
        if (!M.getModuleFlag("Debug Info Version"))
          M.addModuleFlag(Module::Warning, "Debug Info Version",
                          DEBUG_METADATA_VERSION);
      }
      auto *Ty =
          Builder->createSubroutineType(Builder->getOrCreateTypeArray({}));
      F.setSubprogram(Builder->createFunction(
          SyntheticFile, F.getName(), F.getName(), SyntheticFile, 0, Ty, 0,
          DINode::FlagArtificial,
          DISubprogram::SPFlagDefinition | DISubprogram::SPFlagOptimized));
    }
    // Match probe descriptor names to the symbols retained in this module.
    F.getSubprogram()->replaceLinkageName(
        MDString::get(M.getContext(), F.getName()));
    for (BasicBlock &BB : F) {
      for (Instruction &I : make_early_inc_range(BB)) {
        const DILocation *DL = I.getDebugLoc().get();
        // These records describe old inline frames that the new baseline drops.
        if (isa<DbgInfoIntrinsic>(I) && DL && DL->getInlinedAt()) {
          I.eraseFromParent();
          continue;
        }
        for (DbgRecord &DR : make_early_inc_range(I.getDbgRecordRange()))
          if (DR.getDebugLoc() && DR.getDebugLoc()->getInlinedAt())
            DR.eraseFromParent();
        // Attribute previously inlined instructions to the physical caller.
        if (DL)
          I.setDebugLoc(DebugLoc(DL->getInlinedAtLocation()));
        else if (isa<CallBase>(I) && !isa<IntrinsicInst>(I))
          I.setDebugLoc(
              DILocation::get(M.getContext(), 0, 0, F.getSubprogram()));
        updateLoopMetadataDebugLocations(I, [](Metadata *MD) -> Metadata * {
          if (auto *Loc = dyn_cast_or_null<DILocation>(MD))
            return const_cast<DILocation *>(Loc->getInlinedAtLocation());
          return MD;
        });
      }
    }
  }
  if (Builder)
    Builder->finalize();
  return PreservedAnalyses::none();
}
