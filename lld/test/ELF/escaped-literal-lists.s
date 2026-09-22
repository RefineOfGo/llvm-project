# REQUIRES: x86
# These RUN lines rely on Unix command-line backslash/quote handling.
# UNSUPPORTED: system-windows
# RUN: split-file %s %t
# RUN: llvm-mc -filetype=obj -triple=x86_64 %t/main.s -o %t/main.o

## Version-script patterns use assignWildcardVersion and includeNonDefault=false.
## foo\* must select only foo*, while the catch-all localizes fooX.
# RUN: ld.lld -shared %t/main.o --version-script=%t/version.list -o %t/version.so
# RUN: llvm-nm -D --just-symbol-name %t/version.so | FileCheck %s --check-prefix=VERSION --match-full-lines
# VERSION-NOT: {{.}}
# VERSION: foo*@@V1
# VERSION-NOT: {{.}}

## Later wildcard assignments retain their precedence even for escaped literals.
# RUN: ld.lld -shared %t/main.o --version-script=%t/precedence.list -o %t/precedence.so
# RUN: llvm-nm -D --just-symbol-name %t/precedence.so | FileCheck %s --check-prefix=LATER --match-full-lines
# LATER-NOT: {{.}}
# LATER: foo*@@V2
# LATER-NOT: {{.}}

## Both dynamic-list file entry points remove token quotes before glob matching.
# RUN: ld.lld -pie %t/main.o --dynamic-list=%t/dynamic.list -o %t/dynamic
# RUN: llvm-nm -D --just-symbol-name %t/dynamic | FileCheck %s --check-prefix=DYNAMIC --match-full-lines
# RUN: ld.lld -pie %t/main.o --export-dynamic-symbol-list=%t/dynamic.list -o %t/export
# RUN: llvm-nm -D --just-symbol-name %t/export | FileCheck %s --check-prefix=DYNAMIC --match-full-lines
# DYNAMIC-NOT: {{.}}
# DYNAMIC: foo*
# DYNAMIC-NOT: {{.}}

## A mangled and an unmangled symbol share the demangled spelling foo*.
## The extern C++ path must return both, rather than hash-lookup the raw name.
# RUN: ld.lld -shared %t/main.o --version-script=%t/cxx.list -o %t/cxx.so
# RUN: llvm-nm -D --just-symbol-name %t/cxx.so | FileCheck %s --check-prefix=CXX --match-full-lines
# CXX-NOT: {{.}}
# CXX: _Z4foo*@@V1
# CXX-NEXT: foo*@@V1
# CXX-NOT: {{.}}

## A matching Lazy archive symbol must not cause its member to be extracted.
# RUN: llvm-mc -filetype=obj -triple=x86_64 %t/archive.s -o %t/archive.o
# RUN: rm -f %t/archive.a
# RUN: llvm-ar crs %t/archive.a %t/archive.o
# RUN: ld.lld -pie %t/main.o %t/archive.a --export-dynamic-symbol 'lazy\*' -o %t/lazy
# RUN: llvm-nm --just-symbol-name %t/lazy | FileCheck %s --check-prefix=LAZY
# RUN: ld.lld -pie %t/main.o %t/archive.a --dynamic-list=%t/lazy.list -o %t/lazy-list
# RUN: llvm-nm --just-symbol-name %t/lazy-list | FileCheck %s --check-prefix=LAZY
# LAZY-NOT: lazy
# LAZY: _start
# LAZY-NOT: lazy

#--- main.s
.globl _start, "foo*", fooX, "_Z4foo*"
_start:
"foo*":
fooX:
"_Z4foo*":
  ret

#--- archive.s
.globl "lazy*"
"lazy*":
  ret

#--- version.list
V1 { global: "foo\*"; local: *; };

#--- precedence.list
V1 { global: "foo\*"; local: *; };
V2 { global: "foo\*"; };

#--- dynamic.list
{ "foo\*"; };

#--- cxx.list
V1 { global: extern "C++" { foo\*; }; local: *; };

#--- lazy.list
{ "lazy\*"; };
