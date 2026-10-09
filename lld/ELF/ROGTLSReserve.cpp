//===- ROGTLSReserve.cpp --------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "ROGTLSReserve.h"
#include "Config.h"
#include "InputSection.h"
#include "OutputSections.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/BinaryFormat/ELF.h"

using namespace llvm;
using namespace llvm::ELF;
using namespace lld;
using namespace lld::elf;

namespace {
constexpr size_t rogTLSReserveSize = 16;

bool shouldReserveROGTLS(Ctx &ctx) {
  return !ctx.arg.shared && !ctx.arg.relocatable &&
         ctx.arg.emachine == EM_X86_64 && ctx.arg.ekind == ELF64LEKind &&
         ctx.arg.osabi == ELFOSABI_NONE;
}

class ROGTLSReserveSection final : public SyntheticSection {
public:
  ROGTLSReserveSection(Ctx &ctx)
      : SyntheticSection(ctx, ".tbss.rog_tls_reserve", SHT_NOBITS,
                         SHF_ALLOC | SHF_WRITE | SHF_TLS, 8) {
    bss = true;
  }

  size_t getSize() const override { return rogTLSReserveSize; }
  void writeTo(uint8_t *) override {}
};
} // namespace

void elf::maybeAddROGTLSReserve(Ctx &ctx) {
  if (!shouldReserveROGTLS(ctx))
    return;

  ctx.in.rogTLSReserve = std::make_unique<ROGTLSReserveSection>(ctx);
  ctx.inputSections.push_back(ctx.in.rogTLSReserve.get());
}

void elf::placeROGTLSReserveAtEnd(Ctx &ctx) {
  InputSection *reserve = ctx.in.rogTLSReserve.get();
  if (!reserve)
    return;

  auto *out = dyn_cast_or_null<OutputSection>(reserve->parent);
  if (!out)
    return;

  InputSectionDescription *lastISD = nullptr;
  bool found = false;
  for (SectionCommand *cmd : out->commands) {
    auto *isd = dyn_cast<InputSectionDescription>(cmd);
    if (!isd)
      continue;
    lastISD = isd;
    llvm::erase_if(isd->sections, [&](InputSection *isec) {
      if (isec != reserve)
        return false;
      found = true;
      return true;
    });
  }
  if (found && lastISD)
    lastISD->sections.push_back(reserve);
}
