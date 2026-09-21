# REQUIRES: x86
# These RUN lines rely on Unix command-line backslash/quote handling.
# UNSUPPORTED: system-windows
# RUN: llvm-mc -filetype=obj -triple=x86_64 %s -o %t.o
# RUN: echo 'V1 {}; V2 {};' > %t.ver

## The default-version symbol is indexed by its unversioned spelling, but
## the escaped pattern must not match its full, versioned name.
# RUN: ld.lld -pie %t.o --version-script=%t.ver --export-dynamic-symbol 'default\*' -o %t
# RUN: llvm-readelf --dyn-syms %t | FileCheck %s --check-prefix=EMPTY
# EMPTY: Symbol table '.dynsym' contains 1 entries:

## Preserve the wildcard path's filtering for explicit version suffixes.
# RUN: ld.lld -pie %t.o --version-script=%t.ver --export-dynamic-symbol 'nondefault\*@V1' --export-dynamic-symbol 'default\*@@V2' -o %t
# RUN: llvm-nm -D --just-symbol-name %t | FileCheck %s --match-full-lines
# CHECK-NOT: {{.}}
# CHECK: nondefault*@V1
# CHECK-NOT: {{.}}

.globl _start, impl1, impl2
_start:
  ret
impl1:
  ret
impl2:
  ret
.symver impl1, "nondefault*@V1"
.symver impl2, "default*@@V2"
