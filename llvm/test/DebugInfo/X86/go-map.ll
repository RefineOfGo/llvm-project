; RUN: llvm-as %s -o %t.bc
; RUN: llvm-dis %t.bc -o %t.ll
; RUN: llvm-as %t.ll -o %t.roundtrip.bc
; RUN: llc -filetype=obj %t.roundtrip.bc -o %t.o
; RUN: llvm-dwarfdump --debug-info %t.o | FileCheck %s
; RUN: llvm-dwarfdump --verify %t.o
; RUN: sed 's/!"go_key", !5/!"go_key", i32 0/' %s | not --crash llc -filetype=obj -o /dev/null 2>&1 | FileCheck %s --check-prefix=BAD-KEY
;
; Maps carry Go kind, key and element attributes on a typedef describing a
; pointer to the runtime header. The key and element are real type references,
; including a named map whose element recursively refers to the map itself.
;
; CHECK: DW_TAG_typedef
; CHECK: DW_AT_name ("map[int]string")
; CHECK-NEXT: DW_AT_GO_kind (0x15)
; CHECK-NEXT: DW_AT_GO_key ([[INT:0x[0-9a-f]+]])
; CHECK-NEXT: DW_AT_GO_elem ([[STRING:0x[0-9a-f]+]])
; CHECK: DW_TAG_LLVM_annotation
; CHECK-NEXT: DW_AT_name ("ordinary")
; CHECK-NEXT: DW_AT_const_value ("kept")
; CHECK: [[INT]]: DW_TAG_base_type
; CHECK-NEXT: DW_AT_name ("int")
; CHECK: [[STRING]]: DW_TAG_structure_type
; CHECK-NEXT: DW_AT_name ("string")
; CHECK-NEXT: DW_AT_GO_kind (0x18)
; CHECK: [[NAMED:0x[0-9a-f]+]]: DW_TAG_typedef
; CHECK-NEXT: DW_AT_type ([[MAP:0x[0-9a-f]+]] "map[int]M")
; CHECK-NEXT: DW_AT_name ("M")
; CHECK: [[MAP]]: DW_TAG_typedef
; CHECK: DW_AT_name ("map[int]M")
; CHECK-NEXT: DW_AT_GO_kind (0x15)
; CHECK-NEXT: DW_AT_GO_key ([[INT]])
; CHECK-NEXT: DW_AT_GO_elem ([[NAMED]])
; CHECK-NOT: DW_TAG_LLVM_annotation
;
; BAD-KEY: LLVM ERROR: go_key annotation must reference a DIType

target triple = "x86_64-unknown-linux-gnu"

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4}
!0 = distinct !DICompileUnit(language: DW_LANG_Go, file: !1, producer: "test", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !2)
!1 = !DIFile(filename: "maps.go", directory: "/")
!2 = !{!10, !20}
!3 = !{i32 2, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !DIBasicType(name: "int", size: 64, encoding: DW_ATE_signed)
!6 = !DICompositeType(tag: DW_TAG_structure_type, name: "string", size: 128, annotations: !7)
!7 = !{!8}
!8 = !{!"go_kind", i32 24}
!10 = !DIDerivedType(tag: DW_TAG_typedef, name: "map[int]string", baseType: !11, annotations: !13)
!11 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !12, size: 64)
!12 = !DICompositeType(tag: DW_TAG_structure_type, name: "mapHeader", size: 384)
!13 = !{!30, !31, !32, !34}
!20 = !DIDerivedType(tag: DW_TAG_typedef, name: "M", baseType: !21)
!21 = !DIDerivedType(tag: DW_TAG_typedef, name: "map[int]M", baseType: !11, annotations: !22)
!22 = !{!30, !31, !33}
!30 = !{!"go_kind", i32 21}
!31 = !{!"go_key", !5}
!32 = !{!"go_elem", !6}
!33 = !{!"go_elem", !20}
!34 = !{!"ordinary", !"kept"}
