# Design: privilege levels and control registers

An operating system needs the processor to protect it from the programs
it runs. RISC-V does this with *privilege levels* and *control and
status registers* (CSRs). This document explains how riscvsim models
them (roadmap Phase 2). Modules: `Priv`, `Csr`, `Csr_file`, and the CSR
instructions in `Hart`.

## Privilege levels

A hart always runs at one of three levels:

| Level | Name | Who runs there |
|---|---|---|
| M | machine | firmware (for example OpenSBI) |
| S | supervisor | the operating system kernel |
| U | user | ordinary programs |

Higher levels can do everything lower ones can, plus more: only M can
configure the machine, and only S and M can manage memory protection.
A hart starts in M after reset.

## CSRs and their numbers

CSRs are numbered 0 to 4095. The number itself says who may use it:

```
 bits 11:10   9:8
      0b11    any   read-only
      other   0     U-mode and up    (e.g. 0x001 fflags, 0xc00 cycle)
              1     S-mode and up    (e.g. 0x100 sstatus)
              3     M-mode only      (e.g. 0x300 mstatus)
```

`Csr` holds the one table of numbers and names (`mstatus` is `0x300`),
used by the assembler, the disassembler and the hart, plus these two
rules. Accessing a CSR from too low a level, writing a read-only one, or
using one that doesn't exist is an illegal-instruction trap.

Some CSRs have extra conditions, also checked by `Csr_file.check`:

- the floating-point CSRs need the FPU on (see below);
- the counters (`cycle`, `time`, `instret`) are visible to S-mode only
  if M-mode allows it in `mcounteren`, and to U-mode only if both
  `mcounteren` and `scounteren` allow it;
- `satp` (address translation) is off limits to S-mode when M-mode sets
  `mstatus.TVM`, so a hypervisor can intercept it.

## WARL registers

Most CSR fields are *WARL*: "write any value, read a legal value". A
program can write anything, but bits that don't exist read as zero and
a field written with a reserved value keeps a legal one. Examples:

| Write | Reads back | Why |
|---|---|---|
| `mstatus.MPP = 2` | `0` (U) | 2 would be the hypervisor level, which we don't have |
| `mtvec = ...3` | `...1` | only modes 0 and 1 exist |
| `mepc = ...1` | `...0` | instructions are 2-byte aligned |
| `satp` with mode Sv39 | unchanged | translation comes in Phase 3; unsupported modes must be ignored |
| `misa`, `mhartid` | unchanged | fixed values |

A property test writes random values to every implemented CSR and checks
that writing back what was read changes nothing more: legalising twice
gives the same result as legalising once.

`sstatus`, `sie` and `sip` aren't separate registers. They are windows
onto `mstatus`, `mie` and `mip` that show only the parts S-mode may see,
which is how the specification defines them.

## The FPU on/off switch

`mstatus.FS` says whether the floating-point unit is usable and whether
its state has changed:

| FS | Meaning |
|---|---|
| 0 Off | any FP instruction or FP CSR access is an illegal instruction |
| 1 Initial | usable, untouched since the OS last set it |
| 2 Clean | usable, unchanged since the OS last saved it |
| 3 Dirty | usable and changed |

Every instruction that writes an FP register, or raises an FP exception
flag, sets FS to Dirty. Linux relies on this: when switching between
programs, it saves the FP registers only if FS is Dirty, and it starts
programs with FS = Initial. `Os` does the same.

## What stays in the hart

The FP CSRs (`fflags`, `frm`, `fcsr`) and the counters are fields of the
hart itself, because the instructions that touch them run millions of
times a second. Everything else privileged lives in `Csr_file.t`, which
holds the current privilege level and the machine and supervisor CSRs.

The counters are all the instruction count: riscvsim has no clock, so
`cycle` = `time` = `instret`. Writing `minstret` sets the value the next
instruction reads.
