# REQUIRES: x86
# These RUN lines rely on Unix command-line backslash/quote handling.
# UNSUPPORTED: system-windows
# RUN: llvm-mc -filetype=obj -triple=x86_64 %s -o %t.o
# RUN: echo 'V1 {}; V2 {};' > %t.ver

## An escaped literal reaches a symbol the same way an unescaped one does.
## insert() keys the default version "default*@@V2" by its stem "default*",
## so looking that stem up finds it, exactly as a plain literal would.
# RUN: ld.lld -pie %t.o --version-script=%t.ver --export-dynamic-symbol 'default\*' -o %t
# RUN: llvm-nm -D --just-symbol-name %t | FileCheck %s --check-prefix=STEM --match-full-lines
# STEM-NOT: {{.}}
# STEM: default*@@V2
# STEM-NOT: {{.}}

## A spelling carrying the version suffix owns no hash-table entry of its own,
## so "default\*@@V2" matches nothing while the non-default "nondefault\*@V1",
## which is its own key, still does.
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
