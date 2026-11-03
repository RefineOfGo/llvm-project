; RUN: opt -S -passes=early-cse %s | FileCheck %s
; RUN: opt -S -passes='early-cse<memssa>' %s | FileCheck %s
;
; Scalar replacement can introduce a debug-only extract of a returned
; aggregate. Salvage its existing scalar before DCE destroys the extract.

; CHECK-LABEL: define { ptr, i64, i64 } @slice_header(
; CHECK: %header = insertvalue { ptr, i64, i64 } %with.length, i64 %capacity, 2
; CHECK-NOT: extractvalue
; CHECK: #dbg_value(i64 %capacity, !{{[0-9]+}}, !DIExpression(DW_OP_LLVM_fragment, 128, 64),
; CHECK-NOT: extractvalue
; CHECK: ret { ptr, i64, i64 } %header
define { ptr, i64, i64 } @slice_header(ptr %data, i64 %length, i64 %capacity) !dbg !4 {
  %with.data = insertvalue { ptr, i64, i64 } poison, ptr %data, 0
  %with.length = insertvalue { ptr, i64, i64 } %with.data, i64 %length, 1
  %header = insertvalue { ptr, i64, i64 } %with.length, i64 %capacity, 2
  %debug.capacity = extractvalue { ptr, i64, i64 } %header, 2
    #dbg_value(i64 %debug.capacity, !7, !DIExpression(DW_OP_LLVM_fragment, 128, 64), !8)
  ret { ptr, i64, i64 } %header
}

; An opaque aggregate has no existing scalar for the debug-only extract. DCE
; must still remove the unused instruction and mark that fragment unavailable.
; CHECK-LABEL: define void @opaque_header(
; CHECK-NOT: extractvalue
; CHECK: #dbg_value(i64 poison, !{{[0-9]+}}, !DIExpression(DW_OP_LLVM_fragment, 128, 64),
; CHECK-NOT: extractvalue
; CHECK: ret void
define void @opaque_header({ ptr, i64, i64 } %header) !dbg !9 {
  %debug.capacity = extractvalue { ptr, i64, i64 } %header, 2
    #dbg_value(i64 %debug.capacity, !10, !DIExpression(DW_OP_LLVM_fragment, 128, 64), !11)
  ret void
}

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}
!0 = distinct !DICompileUnit(language: DW_LANG_C, file: !1, producer: "regression", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "debug-only-extract.c", directory: "/")
!2 = !{i32 2, !"Dwarf Version", i32 4}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!4 = distinct !DISubprogram(name: "slice_header", scope: !1, file: !1, line: 1, type: !5, scopeLine: 1, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!5 = !DISubroutineType(types: !6)
!6 = !{}
!7 = !DILocalVariable(name: "slice", scope: !4, file: !1, line: 1, type: !12)
!8 = !DILocation(line: 1, column: 1, scope: !4)
!9 = distinct !DISubprogram(name: "opaque_header", scope: !1, file: !1, line: 2, type: !5, scopeLine: 2, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!10 = !DILocalVariable(name: "slice", scope: !9, file: !1, line: 2, type: !12)
!11 = !DILocation(line: 2, column: 1, scope: !9)
!12 = !DIBasicType(name: "slice", size: 192, encoding: DW_ATE_unsigned)
