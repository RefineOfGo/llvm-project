; RUN: llvm-as %s -o %t.bc
; RUN: llvm-dis %t.bc -o %t.ll
; RUN: llc -filetype=obj %t.ll -o %t.o
; RUN: llvm-dwarfdump --debug-info --show-form %t.o | FileCheck %s
; RUN: llvm-dwarfdump --verify %t.o
; RUN: llc -dwarf-version=5 -filetype=obj %t.ll -o %t.5.o
; RUN: llvm-dwarfdump --debug-info --show-form %t.5.o | FileCheck %s
; RUN: sed 's/DW_LANG_Go/DW_LANG_C/' %s | llc -filetype=obj -o %t.c.o
; RUN: llvm-dwarfdump --debug-info %t.c.o | FileCheck %s --check-prefix=C
; RUN: sed 's/!"layout.offset"/i32 0/' %s | not --crash llc -filetype=obj -o /dev/null 2>&1 | FileCheck %s --check-prefix=BAD
; RUN: not --crash llc -dwarf-version=3 -filetype=obj %s -o /dev/null 2>&1 | FileCheck %s --check-prefix=VERSION
;
; Runtime-owned layouts use absolute symbols, not the address of stored offsets.
; The ordinary static size/offset must not also be emitted. Other languages
; preserve these annotations without changing their layout.
;
; CHECK: DW_AT_name [{{DW_FORM_str(p|x1)}}] ("header")
; CHECK-NEXT: DW_AT_byte_size [DW_FORM_data8] (0x0000000000000028)
; CHECK: DW_TAG_member
; CHECK-NEXT: DW_AT_name [{{DW_FORM_str(p|x1)}}] ("count")
; CHECK-NEXT: DW_AT_type
; CHECK-NEXT: DW_AT_data_member_location [DW_FORM_data8] (0x0000000000000018)
; CHECK-NOT: DW_TAG_LLVM_annotation
; C: DW_AT_name ("header")
; C-NEXT: DW_AT_byte_size (0x08)
; C-DAG: DW_AT_name ("go_byte_size_symbol")
; C-DAG: DW_AT_const_value ("layout.size")
; C-DAG: DW_AT_name ("go_member_offset_symbol")
; C-DAG: DW_AT_const_value ("layout.offset")
; BAD: Go layout annotation requires an absolute symbol name
; VERSION: Go layout symbols require DWARF 4 or later

target triple = "x86_64-unknown-linux-gnu"
module asm ".globl layout.size"
module asm ".set layout.size, 40"
module asm ".globl layout.offset"
module asm ".set layout.offset, 24"
!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4}
!0 = distinct !DICompileUnit(language: DW_LANG_Go, file: !1, producer: "test", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, retainedTypes: !2)
!1 = !DIFile(filename: "channel.go", directory: "/")
!2 = !{!5}
!3 = !{i32 2, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !DICompositeType(tag: DW_TAG_structure_type, name: "header", size: 64, elements: !6, annotations: !10)
!6 = !{!7}
!7 = !DIDerivedType(tag: DW_TAG_member, name: "count", scope: !5, baseType: !8, size: 64, offset: 0, annotations: !12)
!8 = !DIBasicType(name: "uint", size: 64, encoding: DW_ATE_unsigned)
!10 = !{!11}
!11 = !{!"go_byte_size_symbol", !"layout.size"}
!12 = !{!13}
!13 = !{!"go_member_offset_symbol", !"layout.offset"}
