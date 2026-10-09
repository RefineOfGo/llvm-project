# RUN: llvm-mc -filetype=obj -triple=x86_64-unknown-linux %s -o %t.o
# RUN: ld.lld %t.o -o %t.reserve -e _start
# RUN: llvm-readelf -S -l %t.reserve | FileCheck %s --check-prefix=RESERVE
# RUN: llvm-objdump -d %t.reserve | FileCheck %s --check-prefix=DISASM
# RUN: llvm-mc -filetype=obj -triple=x86_64-unknown-linux -defsym SHARED=1 %s -o %t.shared.o
# RUN: ld.lld --shared %t.shared.o -o %t.shared
# RUN: llvm-readelf -S -l %t.shared | FileCheck %s --check-prefix=NORESERVE
# RUN: llvm-mc -filetype=obj -triple=x86_64-unknown-freebsd %s -o %t.freebsd.o
# RUN: ld.lld %t.freebsd.o -o %t.freebsd -e _start
# RUN: llvm-readelf -S -l %t.freebsd | FileCheck %s --check-prefix=NORESERVE

# RESERVE:        .tdata
# RESERVE-SAME:   000008
# RESERVE:      .tbss
# RESERVE-SAME: 000018
# RESERVE:      Program Headers:
# RESERVE:      TLS
# RESERVE-SAME: 0x000008 0x000020

# DISASM:      <_start>:
# DISASM-NEXT: movq %fs:-0x18, %rax
# DISASM-NEXT: retq

# NORESERVE:      .tbss
# NORESERVE-SAME: 000008
# NORESERVE:      Program Headers:
# NORESERVE:      TLS
# NORESERVE-SAME: 0x000008 0x000010

.section .text,"ax",@progbits
.globl _start
_start:
.ifndef SHARED
  movq %fs:tls_zero@TPOFF, %rax
.endif
  retq

.section .tdata,"awT",@progbits
.p2align 3
tls_init:
  .quad 42

.section .tbss,"awT",@nobits
.p2align 3
tls_zero:
  .zero 8
