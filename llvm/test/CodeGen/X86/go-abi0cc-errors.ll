; go_abi0cc rejects unsupported configurations.
;  - varargs is rejected by the IR verifier (go_abi0cc is in the no-varargs set).
;  - a non-x86-64 target is rejected during call lowering.
; Two independent modules are split out so each error is exercised in isolation
; (a varargs declaration would otherwise fail verification on every target).

; RUN: split-file %s %t
; RUN: not llc -mtriple=x86_64-unknown-linux-gnu %t/vararg.ll 2>&1 | FileCheck %s --check-prefix=VARARG
; RUN: not llc -mtriple=i386-unknown-linux-gnu   %t/m32.ll    2>&1 | FileCheck %s --check-prefix=M32

; VARARG: Calling convention does not support varargs
; M32: LLVM ERROR: go_abi0cc is only supported on x86-64

;--- vararg.ll
declare go_abi0cc i64 @abi0_vararg(i64, ...)

define i64 @caller_vararg(i64 %a) {
  %r = call go_abi0cc i64 (i64, ...) @abi0_vararg(i64 %a)
  ret i64 %r
}

;--- m32.ll
declare go_abi0cc i64 @abi0_scalar(i64)

define i64 @caller_m32(i64 %a) {
  %r = call go_abi0cc i64 @abi0_scalar(i64 %a)
  ret i64 %r
}
