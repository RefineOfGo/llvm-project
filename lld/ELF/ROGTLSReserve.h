//===- ROGTLSReserve.h ------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLD_ELF_ROG_TLS_RESERVE_H
#define LLD_ELF_ROG_TLS_RESERVE_H

namespace lld::elf {
struct Ctx;

void maybeAddROGTLSReserve(Ctx &ctx);
void placeROGTLSReserveAtEnd(Ctx &ctx);
} // namespace lld::elf

#endif
