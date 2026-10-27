; RUN: llvm-as %s -o %t.bc
; RUN: llvm-dis %t.bc -o %t.ll
; RUN: llvm-as %t.ll -o %t.roundtrip.bc
; RUN: llc -filetype=obj %t.roundtrip.bc -o %t.o
; RUN: llvm-dwarfdump --debug-info %t.o | FileCheck %s
; RUN: llvm-dwarfdump --verify %t.o
; RUN: sed 's/!"go_kind", i32 24/!"go_kind", !"invalid"/' %s | not --crash llc -filetype=obj -o /dev/null 2>&1 | FileCheck %s --check-prefix=BAD-KIND
; RUN: sed 's/!"go_elem", !5/!"go_elem", i32 0/' %s | not --crash llc -filetype=obj -o /dev/null 2>&1 | FileCheck %s --check-prefix=BAD-ELEM
;
; Go kind is an attribute on the header type, and the slice element is a DIE
; reference, including when the element refers recursively to the slice itself.
; Ordinary annotations retain their existing representation.
;
; CHECK: DW_TAG_structure_type
; CHECK-NEXT: DW_AT_name ("string")
; CHECK-NEXT: DW_AT_GO_kind (0x18)
; CHECK-NEXT: DW_AT_byte_size (0x10)
; CHECK: DW_TAG_LLVM_annotation
; CHECK-NEXT: DW_AT_name ("ordinary")
; CHECK-NEXT: DW_AT_const_value ("kept")
; CHECK: DW_TAG_structure_type
; CHECK-NEXT: DW_AT_name ("[]int")
; CHECK-NEXT: DW_AT_GO_kind (0x17)
; CHECK-NEXT: DW_AT_GO_elem ([[INT:0x[0-9a-f]+]])
; CHECK-NEXT: DW_AT_byte_size (0x18)
; CHECK: [[INT]]: DW_TAG_base_type
; CHECK-NEXT: DW_AT_name ("int")
; CHECK: [[SLICE:0x[0-9a-f]+]]: DW_TAG_structure_type
; CHECK-NEXT: DW_AT_name ("[]S")
; CHECK-NEXT: DW_AT_GO_kind (0x17)
; CHECK-NEXT: DW_AT_GO_elem ([[NAMED:0x[0-9a-f]+]])
; CHECK-NEXT: DW_AT_byte_size (0x18)
; CHECK: [[NAMED]]: DW_TAG_typedef
; CHECK-NEXT: DW_AT_type ([[SLICE]] "[]S")
; CHECK-NEXT: DW_AT_name ("S")
; CHECK-NOT: DW_TAG_LLVM_annotation
;
; BAD-KIND: LLVM ERROR: go_kind annotation must be an unsigned byte
; BAD-ELEM: LLVM ERROR: go_elem annotation must reference a DIType

target triple = "x86_64-unknown-linux-gnu"

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4}
!0 = distinct !DICompileUnit(language: DW_LANG_Go, file: !1, producer: "test", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !2)
!1 = !DIFile(filename: "types.go", directory: "/")
!2 = !{!10, !11, !12}
!3 = !{i32 2, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !DIBasicType(name: "int", size: 64, encoding: DW_ATE_signed)
!10 = !DICompositeType(tag: DW_TAG_structure_type, name: "string", size: 128, annotations: !20)
!11 = !DICompositeType(tag: DW_TAG_structure_type, name: "[]int", size: 192, annotations: !21)
!12 = !DICompositeType(tag: DW_TAG_structure_type, name: "[]S", size: 192, annotations: !22)
!13 = !DIDerivedType(tag: DW_TAG_typedef, name: "S", baseType: !12)
!20 = !{!30, !34}
!21 = !{!31, !32}
!22 = !{!31, !33}
!30 = !{!"go_kind", i32 24}
!31 = !{!"go_kind", i32 23}
!32 = !{!"go_elem", !5}
!33 = !{!"go_elem", !13}
!34 = !{!"ordinary", !"kept"}
