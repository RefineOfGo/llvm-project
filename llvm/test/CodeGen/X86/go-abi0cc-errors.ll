; go_abi0cc rejects unsupported configurations.
;  - varargs is rejected by the IR verifier (go_abi0cc is in the no-varargs set).
;  - a function *definition* is rejected by the IR verifier: go_abi0cc returns
;    its results in caller-allocated ABI0 stack slots (modeled only on the caller
;    side), so a defined callee would silently return via the SysV registers.
;  - a non-x86-64 target is rejected during call lowering.
; Independent modules are split out so each error is exercised in isolation
; (a varargs declaration would otherwise fail verification on every target).

; RUN: split-file %s %t
; RUN: not llc -mtriple=x86_64-unknown-linux-gnu %t/vararg.ll 2>&1 | FileCheck %s --check-prefix=VARARG
; RUN: not llc -mtriple=x86_64-unknown-linux-gnu %t/define.ll 2>&1 | FileCheck %s --check-prefix=DEFINE
; RUN: not llc -mtriple=i386-unknown-linux-gnu   %t/m32.ll    2>&1 | FileCheck %s --check-prefix=M32

; VARARG: Calling convention does not support varargs
; DEFINE: go_abi0cc is only supported on external declarations
; M32: LLVM ERROR: go_abi0cc is only supported on x86-64

;--- vararg.ll
declare go_abi0cc i64 @abi0_vararg(i64, ...)

define i64 @caller_vararg(i64 %a) {
  %r = call go_abi0cc i64 (i64, ...) @abi0_vararg(i64 %a)
  ret i64 %r
}

;--- define.ll
define go_abi0cc i64 @abi0_defined(i64 %a) {
  ret i64 %a
}

;--- m32.ll
declare go_abi0cc i64 @abi0_scalar(i64)

define i64 @caller_m32(i64 %a) {
  %r = call go_abi0cc i64 @abi0_scalar(i64 %a)
  ret i64 %r
}
