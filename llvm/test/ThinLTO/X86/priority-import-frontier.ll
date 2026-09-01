; Verify that priority-frontier import traversal handles a diamond, a cycle,
; and two paths to the same function with different expansion thresholds.
;
; RUN: rm -rf %t && split-file %s %t && cd %t
; RUN: opt -module-summary main.ll -o main.bc
; RUN: opt -module-summary lib.ll -o lib.bc
; RUN: llvm-lto2 run main.bc lib.bc -o out -save-temps \
; RUN:   -thinlto-threads=1 -import-instr-limit=10 \
; RUN:   -r=main.bc,root,px -r=main.bc,left, -r=main.bc,right, \
; RUN:   -r=lib.bc,left,px -r=lib.bc,right,px \
; RUN:   -r=lib.bc,shared,px -r=lib.bc,leaf,px
; RUN: llvm-dis out.1.3.import.bc -o - | FileCheck %s
;
; CHECK-DAG: define available_externally void @left()
; CHECK-DAG: define available_externally void @right()
; CHECK-DAG: define available_externally void @shared()
; The hot path reaches shared with expansion threshold 7. This is large enough
; to import leaf; the ordinary path reaches it with threshold 4 and is not.
; CHECK-DAG: define available_externally i32 @leaf(i32 %x)

;--- main.ll
target triple = "x86_64-unknown-linux-gnu"

declare void @left()
declare void @right()

define void @root() {
  call void @left()
  call void @right()
  ret void
}

;--- lib.ll
target triple = "x86_64-unknown-linux-gnu"

define void @left() {
  call void @shared()
  ret void
}

define void @right() !prof !15 {
  call void @shared(), !prof !0
  ret void
}

define void @shared() {
  call void @left()
  %unused = call i32 @leaf(i32 0)
  ret void
}

define i32 @leaf(i32 %x) {
  %a = add i32 %x, 1
  %b = add i32 %a, 2
  %c = add i32 %b, 3
  %d = add i32 %c, 4
  ret i32 %d
}

!0 = !{!"branch_weights", i32 100}
!llvm.module.flags = !{!1}
!1 = !{i32 1, !"ProfileSummary", !2}
!2 = !{!3, !4, !5, !6, !7, !8, !9, !10}
!3 = !{!"ProfileFormat", !"InstrProf"}
!4 = !{!"TotalCount", i64 10000}
!5 = !{!"MaxCount", i64 100}
!6 = !{!"MaxInternalCount", i64 100}
!7 = !{!"MaxFunctionCount", i64 100}
!8 = !{!"NumCounts", i64 1}
!9 = !{!"NumFunctions", i64 1}
!10 = !{!"DetailedSummary", !11}
!11 = !{!12, !13, !14}
!12 = !{i32 10000, i64 100, i32 1}
!13 = !{i32 999000, i64 100, i32 1}
!14 = !{i32 999999, i64 1, i32 1}
!15 = !{!"function_entry_count", i64 100}
