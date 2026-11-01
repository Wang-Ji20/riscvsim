# Design: the bus, devices and traps

This document covers the parts that prepare riscvsim to simulate a whole
computer rather than one Linux process (roadmap Phase 1). Modules:
`Trap`, `Bus`, `Device`.

## User mode and system mode

Today riscvsim runs in *user mode*: it pretends to be one Linux process.
Memory exists wherever the program writes, and the operating system is
emulated by `Os`.

To boot Linux itself, riscvsim must become a *system-mode* simulator: a
machine with a fixed amount of RAM at a fixed address, devices such as a
serial port at other addresses, and nothing at all elsewhere. A program
that touches "nothing" gets an *access fault* (a kind of trap), which
the operating system then handles.

Both modes share the same hart. They differ only in the bus the hart is
connected to.

## The bus

The *bus* is the hart's path to everything outside it. Each load,
store, atomic operation and instruction fetch goes through `Bus`, which
decides where the address leads:

```
 user mode:    [ RAM everywhere ........................................ ]

 system mode:  0x0      0x0200_0000  0x1000_0000          0x8000_0000
               | fault  | device     | device  | fault... | RAM        | fault
```

- **User mode** (`Bus.user ()`): every address is RAM. Pages appear on
  first write, as before.
- **System mode** (`Bus.system ~ram_base ~ram_size`): RAM is one
  region. Devices are attached at their own addresses with
  `Bus.attach`. Any other address faults.

An access must lie entirely inside one region. An 8-byte store that
starts 4 bytes before the end of RAM faults, and **writes nothing**:
the bus checks the whole range before writing. This keeps traps
*precise* (see below).

Code runs only from RAM, and atomic instructions work only on RAM.
Anything else is an access fault, as on most real machines.

### Why a region check instead of a page table of devices

The hot path is a RAM access. In user mode the check is a single
boolean (`all_ram`). In system mode it is one range comparison. Devices
all sit below RAM, so an address is compared with the device table only
when it lies below `mmio_limit`. A lookup structure for devices would
make device accesses faster, but they are rare compared to RAM accesses.

## Devices

A device is a module with this signature (`Device.S`):

```ocaml
module type S = sig
  type t
  val name : string
  val size : int64                                  (* bytes of address space *)
  val read : t -> offset:int64 -> size:int -> int64
  val write : t -> offset:int64 -> size:int -> int64 -> unit
  val tick : t -> now:int -> unit                   (* advance to time [now] *)
end
```

`Device.pack (module Uart) state` turns a device and its state into a
plain record of functions (`Device.t`), so the bus can keep devices of
different types in one list. Offsets are relative to the device's base
address, so a device doesn't need to know where it was placed.

`tick` lets a device do work over time, for example a timer raising an
interrupt. Time is counted in instructions, so a run is deterministic.

## Traps

A *trap* is what happens when an instruction can't complete: an illegal
instruction, a misaligned atomic, an access fault, `ebreak`, `ecall`.
`Trap` lists every kind with the number RISC-V assigns to it (the
*cause*), and carries a value called *tval* with more detail:

| Trap | Cause | tval |
|---|---:|---|
| illegal instruction | 2 | the instruction's bits |
| breakpoint (`ebreak`) | 3 | its address |
| load / store access fault | 5 / 7 | the address |
| misaligned atomic | 4 / 6 | the address |
| instruction access fault | 1 | the address that couldn't be fetched |

The hart raises `Trap.Trap { cause; tval }` as an OCaml exception. In
user mode, the run stops and riscvsim reports it. In system mode (roadmap
Phase 2) the hart will instead jump to the operating system's trap
handler, writing the cause and tval into control registers.

**Precise traps.** A trapping instruction must change nothing:
registers, the program counter, memory, control registers and the
instruction counter stay as they were. The operating system relies on
this to fix the problem (for example, load a missing page) and run the
instruction again. A property test executes thousands of random
instructions on random machine states and checks that whenever one
traps, every part of the state is unchanged.

One subtle case: a 4-byte instruction can start 2 bytes before the end
of RAM. Its second half is outside RAM, so the fault is reported at that
address (`pc + 2`), not at the instruction's start, as the RISC-V
specification requires.
