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
#include "SymbolTable.h"
#include "Symbols.h"
#include "llvm/BinaryFormat/ELF.h"

using namespace llvm;
using namespace llvm::ELF;
using namespace lld;
using namespace lld::elf;

namespace {
constexpr StringRef rogTLSReserveMarker = "__rog_lld_tls_reserve";
constexpr size_t rogTLSReserveSize = 32;

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
  if (ctx.arg.relocatable || ctx.arg.emachine != EM_X86_64 ||
      ctx.arg.ekind != ELF64LEKind || ctx.arg.osabi != ELFOSABI_NONE)
    return;

  Symbol *marker = ctx.symtab->find(rogTLSReserveMarker);
  if (!marker || !marker->isDefined())
    return;

  ctx.in.rogTLSReserve = std::make_unique<ROGTLSReserveSection>(ctx);
  ctx.inputSections.push_back(ctx.in.rogTLSReserve.get());
}
