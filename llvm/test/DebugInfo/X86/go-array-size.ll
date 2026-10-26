; RUN: llvm-as %s -o %t.bc
; RUN: llc -filetype=obj %t.bc -o %t.go.o
; RUN: llvm-dwarfdump --debug-info %t.go.o | FileCheck %s --check-prefixes=COMMON,GO
; RUN: sed 's/DW_LANG_Go/DW_LANG_C99/' %s | llc -filetype=obj -o %t.c.o
; RUN: llvm-dwarfdump --debug-info %t.c.o | FileCheck %s --check-prefixes=COMMON,C
;
; Go debuggers need the total size to compute array element strides. Preserve
; explicit zero sizes, but do not turn unknown or dynamic sizes into zero.
; Other languages and vector padding retain their existing behavior.

; COMMON-LABEL: DW_TAG_array_type
; GO-NEXT: DW_AT_byte_size (0x08)
; C-NOT: DW_AT_byte_size
; COMMON: DW_AT_name ("regular")
; COMMON-LABEL: DW_TAG_array_type
; GO-NEXT: DW_AT_byte_size (0x00)
; C-NOT: DW_AT_byte_size
; COMMON: DW_AT_name ("zero_length")
; COMMON-LABEL: DW_TAG_array_type
; GO-NEXT: DW_AT_byte_size (0x00)
; C-NOT: DW_AT_byte_size
; COMMON: DW_AT_name ("zero_element")
; COMMON-LABEL: DW_TAG_array_type
; GO-NEXT: DW_AT_byte_size (0x18)
; C-NOT: DW_AT_byte_size
; COMMON: DW_AT_name ("nested")
; COMMON-LABEL: DW_TAG_array_type
; COMMON-NOT: DW_AT_byte_size
; COMMON: DW_AT_name ("unknown_size")
; COMMON-LABEL: DW_TAG_array_type
; COMMON-NOT: DW_AT_byte_size
; COMMON: DW_AT_name ("dynamic_size")
; COMMON-LABEL: DW_TAG_array_type
; COMMON-NOT: DW_AT_byte_size
; COMMON: DW_AT_name ("unknown_count")
; COMMON-LABEL: DW_TAG_array_type
; COMMON-NOT: DW_AT_byte_size
; COMMON: DW_AT_name ("dynamic_count")
; COMMON-LABEL: DW_TAG_array_type
; COMMON-NOT: DW_AT_byte_size
; COMMON: DW_AT_name ("forward")
; COMMON-LABEL: DW_TAG_array_type
; COMMON-NOT: DW_AT_byte_size
; COMMON: DW_AT_name ("missing_bounds")
; COMMON-LABEL: DW_TAG_array_type
; COMMON-NEXT: DW_AT_GNU_vector (true)
; COMMON-NOT: DW_AT_byte_size
; COMMON: DW_AT_name ("vector")
; COMMON-LABEL: DW_TAG_array_type
; COMMON-NEXT: DW_AT_GNU_vector (true)
; COMMON-NEXT: DW_AT_byte_size (0x10)
; COMMON: DW_AT_name ("padded_vector")

target triple = "x86_64-unknown-linux-gnu"

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4}
!0 = distinct !DICompileUnit(language: DW_LANG_Go, file: !1, producer: "test", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !2)
!1 = !DIFile(filename: "arrays.go", directory: "/")
!2 = !{!10, !11, !12, !13, !14, !15, !16, !17, !18, !31, !19, !20}
!3 = !{i32 2, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !DIBasicType(name: "int32", size: 32, encoding: DW_ATE_signed)
!6 = !DICompositeType(tag: DW_TAG_structure_type, name: "struct {}", size: 0, elements: !7)
!7 = !{}
!10 = !DICompositeType(tag: DW_TAG_array_type, name: "regular", baseType: !5, size: 64, elements: !21)
!11 = !DICompositeType(tag: DW_TAG_array_type, name: "zero_length", baseType: !5, size: 0, elements: !22)
!12 = !DICompositeType(tag: DW_TAG_array_type, name: "zero_element", baseType: !6, size: 0, elements: !23)
!13 = !DICompositeType(tag: DW_TAG_array_type, name: "nested", baseType: !10, size: 192, elements: !23)
!14 = !DICompositeType(tag: DW_TAG_array_type, name: "unknown_size", baseType: !5, elements: !21)
!15 = !DICompositeType(tag: DW_TAG_array_type, name: "dynamic_size", baseType: !5, size: !DIExpression(DW_OP_constu, 64), elements: !21)
!16 = !DICompositeType(tag: DW_TAG_array_type, name: "unknown_count", baseType: !5, size: 0, elements: !24)
!17 = !DICompositeType(tag: DW_TAG_array_type, name: "dynamic_count", baseType: !5, size: 0, elements: !25)
!18 = !DICompositeType(tag: DW_TAG_array_type, name: "forward", baseType: !5, size: 64, flags: DIFlagFwdDecl, elements: !21)
!19 = !DICompositeType(tag: DW_TAG_array_type, name: "vector", baseType: !5, size: 64, flags: DIFlagVector, elements: !21)
!20 = !DICompositeType(tag: DW_TAG_array_type, name: "padded_vector", baseType: !5, size: 128, flags: DIFlagVector, elements: !23)
!21 = !{!26}
!22 = !{!27}
!23 = !{!28}
!24 = !{!29}
!25 = !{!30}
!26 = !DISubrange(count: 2)
!27 = !DISubrange(count: 0)
!28 = !DISubrange(count: 3)
!29 = !DISubrange(count: -1)
!30 = !DISubrange(count: !DIExpression(DW_OP_constu, 2))
!31 = !DICompositeType(tag: DW_TAG_array_type, name: "missing_bounds", baseType: !5, size: 0, elements: !7)
