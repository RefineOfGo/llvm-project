; RUN: opt -S -passes='thinlto-pre-link<O2>' -rog-postopt-pseudo-probe %s -o %t
; RUN: FileCheck %s --check-prefix=PROBE < %t
; RUN: not --crash opt -disable-output -passes='thinlto-pre-link<O2>' -rog-postopt-pseudo-probe %t 2>&1 | FileCheck %s --check-prefix=REPEAT
; RUN: opt -S -passes='thinlto-pre-link<O2>' %s | FileCheck %s --check-prefix=DEFAULT

; PROBE: define i32 @caller
; PROBE: call void @llvm.pseudoprobe
; PROBE: call i32 @external(i32 %x), !dbg ![[CALL:[0-9]+]]
; PROBE: !llvm.pseudo_probe_desc =
; PROBE: ![[CALL]] = !DILocation(line: 10, column: 3, scope: ![[SCOPE:[0-9]+]])
; PROBE-NOT: inlinedAt:
; REPEAT: ROG post-optimization probes require unprobed IR
; DEFAULT-NOT: llvm.pseudoprobe

declare i32 @external(i32)

define i32 @caller(i32 %x) !dbg !4 {
  %r = call i32 @external(i32 %x), !dbg !8
  ret i32 %r, !dbg !9
}

define i32 @no_debug(i32 %x) {
  %r = call i32 @external(i32 %x)
  ret i32 %r
}

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2}
!0 = distinct !DICompileUnit(language: DW_LANG_Rust, file: !1, producer: "test", isOptimized: true, runtimeVersion: 0, emissionKind: LineTablesOnly)
!1 = !DIFile(filename: "test.rs", directory: "/")
!2 = !{i32 2, !"Debug Info Version", i32 3}
!3 = !DISubroutineType(types: !{})
!4 = distinct !DISubprogram(name: "caller", scope: !1, file: !1, line: 1, type: !3, scopeLine: 1, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!5 = distinct !DISubprogram(name: "inlined", scope: !1, file: !1, line: 20, type: !3, scopeLine: 20, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0)
!6 = !DILocation(line: 10, column: 3, scope: !4)
!8 = !DILocation(line: 21, column: 5, scope: !5, inlinedAt: !6)
!9 = !DILocation(line: 11, column: 1, scope: !4)
