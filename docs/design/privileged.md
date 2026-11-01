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

## Taking a trap

In a `System` environment, a trap doesn't stop the simulation: the hart
jumps to the guest's trap handler, as hardware does. First it picks the
level that handles it:

```
trap raised at privilege P
        |
        v
  P below M and the cause's bit set in medeleg?  -- yes -->  S-mode handler
  (mideleg for interrupts)                       -- no  -->  M-mode handler
```

So M-mode firmware can hand ordinary faults (page faults, system calls
from programs) straight to the kernel in S-mode, without running itself.
A trap never goes to a lower level than the one it was raised in.

Then, for the chosen level (M shown; S uses `sepc`, `scause`, ...):

| Step | M-mode |
|---|---|
| where it happened | `mepc` <- pc of the trapping instruction |
| why | `mcause` <- cause number (bit 63 set for interrupts) |
| detail | `mtval` <- tval (the address, or the instruction's bits) |
| interrupts | `MPIE` <- `MIE`; `MIE` <- 0 |
| previous level | `MPP` <- P |
| new level | M |
| jump | `mtvec` base, or base + 4 x cause for an interrupt in vectored mode |

`mret` undoes exactly this, so a handler can fix the problem and return.
The trapping instruction doesn't count as executed (`instret` doesn't
move), and `ecall` reports the level it came from: cause 8, 9 or 11 for
U, S or M.

## Privileged instructions

| Instruction | Does | Illegal when |
|---|---|---|
| `mret` | return from an M-mode trap handler: privilege from `mstatus.MPP`, `MIE` from `MPIE`, jump to `mepc` | not in M-mode |
| `sret` | the same for S-mode, with `SPP`, `SIE`, `SPIE` and `sepc` | in U-mode, or in S-mode with `mstatus.TSR` |
| `wfi` | wait for an interrupt | in U-mode, or in S-mode with `mstatus.TW` |
| `sfence.vma` | flush cached address translations | in U-mode, or in S-mode with `mstatus.TVM` |

After a return, `MPIE`/`SPIE` become 1 and `MPP`/`SPP` become U, so a
handler that forgets to set them can't accidentally return to a higher
level. Leaving M-mode also clears `MPRV`.

`wfi` is allowed to return at once, and does, since there are no
interrupts yet. `sfence.vma` does nothing until there is address
translation with a cache to flush (Phase 3). U-mode `wfi` is illegal,
as in Spike.

## What stays in the hart

The FP CSRs (`fflags`, `frm`, `fcsr`) and the counters are fields of the
hart itself, because the instructions that touch them run millions of
times a second. Everything else privileged lives in `Csr_file.t`, which
holds the current privilege level and the machine and supervisor CSRs.

The counters are all the instruction count: riscvsim has no clock, so
`cycle` = `time` = `instret`. Writing `minstret` sets the value the next
instruction reads.
