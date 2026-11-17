; RUN: split-file %s %t
; RUN: not opt -passes=rog-gcwb-switch-opt -disable-output %t/scalar.ll 2>&1 | FileCheck %s --check-prefix=LAYOUT
; RUN: not opt -passes=rog-gcwb-switch-opt -disable-output %t/flag-width.ll 2>&1 | FileCheck %s --check-prefix=LAYOUT
; RUN: not opt -passes=rog-gcwb-switch-opt -disable-output %t/padding-type.ll 2>&1 | FileCheck %s --check-prefix=LAYOUT
; RUN: not opt -passes=rog-gcwb-switch-opt -disable-output %t/flag-array.ll 2>&1 | FileCheck %s --check-prefix=LAYOUT
; RUN: not opt -passes=rog-gcwb-switch-opt -disable-output %t/wrapper-fields.ll 2>&1 | FileCheck %s --check-prefix=LAYOUT
; RUN: not opt -passes=rog-gcwb-switch-opt -disable-output %t/array-storage.ll 2>&1 | FileCheck %s --check-prefix=LAYOUT
; RUN: not opt -passes=rog-gcwb-switch-opt -disable-output %t/function-symbol.ll 2>&1 | FileCheck %s --check-prefix=SYMBOL

; Layout changes in the Rust runtime must fail loudly instead of disabling reuse.
; LAYOUT: LLVM ERROR: ROG_GCWB_SWITCH must be CachePadded<AtomicBool>
; SYMBOL: LLVM ERROR: ROG_GCWB_SWITCH must resolve to a global variable

;--- scalar.ll
@ROG_GCWB_SWITCH = external global i8

define void @f() gc "rog" {
  ret void
}

;--- flag-width.ll
@ROG_GCWB_SWITCH = external global { i16, [126 x i8] }

define void @f() gc "rog" {
  ret void
}

;--- padding-type.ll
@ROG_GCWB_SWITCH = external global { [1 x i8], [127 x i16] }

define void @f() gc "rog" {
  ret void
}

;--- flag-array.ll
@ROG_GCWB_SWITCH = external global { [2 x i8], [126 x i8] }

define void @f() gc "rog" {
  ret void
}

;--- wrapper-fields.ll
@ROG_GCWB_SWITCH = external global { { i8, i8 }, [126 x i8] }

define void @f() gc "rog" {
  ret void
}

;--- array-storage.ll
@ROG_GCWB_SWITCH = external global [128 x i8]

define void @f() gc "rog" {
  ret void
}

;--- function-symbol.ll
declare void @ROG_GCWB_SWITCH()

define void @f() gc "rog" {
  ret void
}
