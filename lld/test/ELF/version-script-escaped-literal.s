# REQUIRES: x86
# RUN: split-file %s %t
# RUN: llvm-mc -filetype=obj -triple=x86_64 %t/main.s -o %t/main.o

## An escaped metacharacter can only ever match one name, so the pattern is an
## exact one and takes part in the undefined-version check. GNU ld reports
## "nonexistent*: undefined version: V1" for this script; lld used to classify
## it as a glob and stay silent.
# RUN: not ld.lld -shared %t/main.o --no-undefined-version \
# RUN:   --version-script=%t/undef.ver -o /dev/null 2>&1 | FileCheck %s --check-prefix=UNDEF
# UNDEF: error: version script assignment of 'V1' to symbol 'nonexistent*' failed: symbol not defined

## A real glob that matches nothing stays silent, as in GNU ld.
# RUN: ld.lld -shared %t/main.o --no-undefined-version --version-script=%t/glob-undef.ver -o /dev/null

## Matching is unchanged, and agrees with GNU ld: foo\* selects foo* alone.
# RUN: ld.lld -shared %t/main.o --version-script=%t/one.ver -o %t/one.so
# RUN: llvm-nm -D --just-symbol-name %t/one.so | FileCheck %s --check-prefix=ONE --match-full-lines
# ONE-NOT: {{.}}
# ONE: foo*@@V1
# ONE-NOT: {{.}}

## The unescaped glob still selects all three.
# RUN: ld.lld -shared %t/main.o --version-script=%t/all.ver -o %t/all.so
# RUN: llvm-nm -D --just-symbol-name %t/all.so | FileCheck %s --check-prefix=ALL --match-full-lines
# ALL-NOT: {{.}}
# ALL: foo*@@V1
# ALL-NEXT: fooX@@V1
# ALL-NEXT: fooY@@V1
# ALL-NOT: {{.}}

## A quoted token is left to SingleStringMatcher and keeps its classification.
# RUN: ld.lld -shared %t/main.o --no-undefined-version --version-script=%t/quoted.ver -o %t/quoted.so
# RUN: llvm-nm -D --just-symbol-name %t/quoted.so | FileCheck %s --check-prefix=ONE --match-full-lines

#--- main.s
.globl _start, "foo*", fooX, fooY
_start:
"foo*":
fooX:
fooY:
  ret

#--- undef.ver
V1 { global: nonexistent\*; local: *; };

#--- glob-undef.ver
V1 { global: nonexistent*; local: *; };

#--- one.ver
V1 { global: foo\*; local: *; };

#--- all.ver
V1 { global: foo*; local: *; };

#--- quoted.ver
V1 { global: "foo\*"; local: *; };
