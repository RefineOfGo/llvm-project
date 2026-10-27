; RUN: llc -filetype=obj %s -o %t.o
; RUN: llvm-dwarfdump --debug-info %t.o | FileCheck %s --implicit-check-not=DW_AT_GO_kind --implicit-check-not=DW_AT_GO_elem
;
; Names reserved for Go type metadata remain ordinary annotations in other CUs.
; CHECK: DW_TAG_structure_type
; CHECK-NEXT: DW_AT_name ("Header")
; CHECK: DW_TAG_LLVM_annotation
; CHECK-NEXT: DW_AT_name ("go_kind")
; CHECK-NEXT: DW_AT_const_value (24)
; CHECK: DW_TAG_LLVM_annotation
; CHECK-NEXT: DW_AT_name ("go_elem")
; CHECK-NEXT: DW_AT_const_value ("ordinary")

target triple = "x86_64-unknown-linux-gnu"
!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4}
!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "test", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !2)
!1 = !DIFile(filename: "types.c", directory: "/")
!2 = !{!5}
!3 = !{i32 2, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !DICompositeType(tag: DW_TAG_structure_type, name: "Header", size: 128, annotations: !6)
!6 = !{!7, !8}
!7 = !{!"go_kind", i32 24}
!8 = !{!"go_elem", !"ordinary"}
