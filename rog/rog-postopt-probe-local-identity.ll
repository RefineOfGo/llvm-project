; RUN: sed 's/local.__uniq.1/local.__uniq.2/' %s > %t-b.ll
; RUN: opt -S -passes='thinlto-pre-link<O2>' -rog-postopt-pseudo-probe %s -o %t-a.out
; RUN: opt -S -passes='thinlto-pre-link<O2>' -rog-postopt-pseudo-probe %t-b.ll -o %t-b.out
; RUN: FileCheck %s --check-prefix=IDENTITY < %t-a.out
; RUN: FileCheck %s --check-prefix=IDENTITY < %t-b.out
; RUN: grep 'define internal.*@local' %t-a.out > %t-a.names
; RUN: grep 'define internal.*@local' %t-b.out > %t-b.names
; RUN: not cmp %t-a.names %t-b.names
; RUN: opt -S -passes=sample-profile -sample-profile-file=%S/Inputs/rog-postopt-probe-local.prof %t-a.out -o %t-profile.out
; RUN: FileCheck %s --check-prefix=PROFILE < %t-profile.out

source_filename = "unit_a.c"

define internal i32 @local.__uniq.1(i32 %x) noinline "use-sample-profile" "sample-profile-suffix-elision-policy"="none" {
  %r = add i32 %x, 1
  ret i32 %r
}

define i32 @exported(i32 %x) {
  %r = call i32 @local.__uniq.1(i32 %x)
  ret i32 %r
}

; IDENTITY: define internal {{.*}}@local.__uniq.[[HASH:[0-9]+]](
; IDENTITY: define i32 @exported(
; IDENTITY: !{i64 {{-?[0-9]+}}, i64 {{[0-9]+}}, !"local.__uniq.[[HASH]]"}

; PROFILE: define internal {{.*}}@local.__uniq.1({{.*}} !prof ![[ENTRY:[0-9]+]]
; PROFILE: ![[ENTRY]] = !{!"function_entry_count", i64 1000}
