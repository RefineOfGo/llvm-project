; RUN: opt -S -passes=early-cse %s | FileCheck %s
; RUN: opt -S -passes='early-cse<memssa>' %s | FileCheck %s
;
; A debug-only redundant load should reuse an existing value, without keeping
; the redundant instruction or salvaging a mutable-memory DW_OP_deref.

; CHECK-LABEL: define i64 @available(
; CHECK: %first = load i64, ptr %p, align 8, !range ![[RANGE:[0-9]+]]
; CHECK-NOT: load i64
; CHECK: #dbg_value(i64 %first, ![[AVAILABLE:[0-9]+]], !DIExpression(DW_OP_LLVM_fragment, 64, 64),
; CHECK-NOT: load i64
; CHECK: ret i64 %first
define i64 @available(ptr %p) !dbg !4 {
  %first = load i64, ptr %p, align 8, !range !19
  %debug.only = load i64, ptr %p, align 8
    #dbg_value(i64 %debug.only, !7, !DIExpression(DW_OP_LLVM_fragment, 64, 64), !8)
  ret i64 %first
}

; CHECK-LABEL: define void @available_store(
; CHECK: store i64 %value, ptr %p
; CHECK-NOT: load i64
; CHECK: #dbg_value(i64 %value,
; CHECK: ret void
define void @available_store(ptr %p, i64 %value) !dbg !16 {
  store i64 %value, ptr %p, align 8
  %debug.only = load i64, ptr %p, align 8
    #dbg_value(i64 %debug.only, !17, !DIExpression(DW_OP_LLVM_fragment, 64, 64), !18)
  ret void
}

; CHECK-LABEL: define i64 @clobbered(
; CHECK: %first = load i64, ptr %p
; CHECK: call void @clobber(ptr %p)
; CHECK-NOT: load i64
; CHECK: #dbg_value(i64 poison,
; CHECK: ret i64 %first
define i64 @clobbered(ptr %p) !dbg !9 {
  %first = load i64, ptr %p, align 8
  call void @clobber(ptr %p)
  %debug.only = load i64, ptr %p, align 8
    #dbg_value(i64 %debug.only, !10, !DIExpression(DW_OP_LLVM_fragment, 64, 64), !11)
  ret i64 %first
}

; CHECK-LABEL: define void @unavailable(
; CHECK-NOT: load i64
; CHECK: #dbg_value(i64 poison,
; CHECK: ret void
define void @unavailable(ptr %p) !dbg !12 {
  %debug.only = load i64, ptr %p, align 8
    #dbg_value(i64 %debug.only, !13, !DIExpression(DW_OP_LLVM_fragment, 64, 64), !14)
  ret void
}

declare void @clobber(ptr)

; CHECK: ![[RANGE]] = !{i64 0, i64 100}

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}
!0 = distinct !DICompileUnit(language: DW_LANG_C, file: !1, producer: "regression", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "debug-only-load.c", directory: "/")
!2 = !{i32 2, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!4 = distinct !DISubprogram(name: "available", scope: !1, file: !1, line: 1, type: !5, scopeLine: 1, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!5 = !DISubroutineType(types: !6)
!6 = !{}
!7 = !DILocalVariable(name: "pair", scope: !4, file: !1, line: 1, type: !15)
!8 = !DILocation(line: 1, column: 1, scope: !4)
!9 = distinct !DISubprogram(name: "clobbered", scope: !1, file: !1, line: 2, type: !5, scopeLine: 2, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!10 = !DILocalVariable(name: "pair", scope: !9, file: !1, line: 2, type: !15)
!11 = !DILocation(line: 2, column: 1, scope: !9)
!12 = distinct !DISubprogram(name: "unavailable", scope: !1, file: !1, line: 3, type: !5, scopeLine: 3, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!13 = !DILocalVariable(name: "pair", scope: !12, file: !1, line: 3, type: !15)
!14 = !DILocation(line: 3, column: 1, scope: !12)
!15 = !DIBasicType(name: "__int128", size: 128, encoding: DW_ATE_unsigned)
!16 = distinct !DISubprogram(name: "available_store", scope: !1, file: !1, line: 4, type: !5, scopeLine: 4, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!17 = !DILocalVariable(name: "pair", scope: !16, file: !1, line: 4, type: !15)
!18 = !DILocation(line: 4, column: 1, scope: !16)
!19 = !{i64 0, i64 100}
