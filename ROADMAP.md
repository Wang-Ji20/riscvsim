# Roadmap: self-hosting riscvsim inside simulated RISC-V Linux

**End goal**: `riscvsim` boots an unmodified RV64GC Linux kernel to a shell.
Inside that guest, we build riscvsim from source with the OCaml toolchain and
run it, ideally booting a second, nested Linux.

Today riscvsim is a *user-mode* simulator: one hart, a flat address space,
and syscalls emulated by the host. Linux needs a *system-mode* machine:
privilege levels, traps, virtual memory, timers, interrupts, devices,
firmware, and enough speed to boot in minutes rather than days.

The phases below are ordered by dependency. Each step ends with something
testable. A step marked ✅ is done in the user-mode simulator.

---

## Phase 0: User-mode foundation (current work)

0.1 ✅ RV64I/M/A/F/D/C/Zicsr/Zifencei/Zba/Zbb/Zbs decode, encode, execute
0.2 ✅ Assembler, ELF writer/loader, Linux-ABI syscall emulation
0.3 ✅ Property tests (QCheck2) and a byte-exact differential test of all
    encodings against clang
0.4 ✅ Pass the official **riscv-tests**: 150/150 (rv64u{i,m,a,f,d,c},
    rv64uzba/zbb/zbs), using a user-mode environment instead of HTIF.
0.5 ✅ **riscv-arch-test** 2.7.4: 265/265 signatures match the shipped
    references (I, M, C, D, Zifencei). Spike is a second oracle: random
    differential testing (`dune build @spike`) compares complete
    architectural state on random programs. ACT4's Sail-based flow remains
    to do.
0.6 ✅ Static C programs in user mode, with the Linux initial stack
    (argc, argv, envp, auxv) and about 60 syscalls. Programs are checked
    on our own freestanding libc (`guest/libc`, the "musl-like" one) and on
    static glibc 2.44, both against a native host build.
0.7 ✅ OCaml 5.4.1's bytecode runtime (`ocamlrun`, static riscv64) runs
    host-compiled bytecode with output identical to the host
    (`dune build @ocaml`). The stress program takes about 65 s, roughly
    400M instructions, which motivates Phase 8. Multi-domain programs
    (`Domain.spawn`) will need `clone(CLONE_THREAD)`, that is, real guest
    threads.

## Phase 1: Architecture refactor for system mode

1.1 Split `Cpu` into `Hart` (architectural state) and `Machine` (harts, bus,
    devices, clock).
1.2 Introduce a `Bus` interface with `load/store : addr -> size -> ...`, and
    route RAM and MMIO through it with a region table, so devices are
    first-class modules:
    `module type DEVICE = sig val read : ... val write : ... val tick : ... end`
1.3 Make memory access return `(value, exception) result`, or raise a typed
    `Trap` exception, so page faults and access faults can propagate.
1.4 Model exceptions precisely: on a trap, no architectural state changes,
    and `*epc`/`*tval` are correct.
1.5 Replace the user-mode syscall layer with a pluggable "environment". User
    mode keeps the Linux-ABI emulation; system mode delivers `ecall` as a
    real trap.
1.6 Add a checkpoint/snapshot format (registers, CSRs, RAM pages, device
    state) for fast iteration on long boots.

## Phase 2: Privileged ISA (M, S, U modes)

2.1 Privilege levels and `mstatus`/`sstatus` (MIE, SIE, MPIE, SPIE, MPP, SPP,
    SUM, MXR, FS, XS, SD, UXL/SXL).
2.2 Machine CSRs: `misa`, `mvendorid`, `marchid`, `mimpid`, `mhartid`,
    `mtvec`, `mepc`, `mcause`, `mtval`, `mscratch`, `mie`, `mip`, `medeleg`,
    `mideleg`, `mcounteren`, `menvcfg`, `mconfigptr`.
2.3 Supervisor CSRs: `stvec`, `sepc`, `scause`, `stval`, `sscratch`, `sie`,
    `sip`, `satp`, `scounteren`, `senvcfg`.
2.4 Trap entry: vectored and direct `mtvec`/`stvec`, delegation through
    `medeleg`/`mideleg`, and interrupt priority ordering.
2.5 `mret`, `sret`, `wfi`, and `sfence.vma`. Raise illegal-instruction for
    privileged ops in the wrong mode, and enforce CSR access permissions
    (read-only, privilege level).
2.6 FP state tracking: `mstatus.FS` off/initial/clean/dirty. An FP
    instruction with FS=off must trap, because Linux relies on this for
    lazy FP context switching.
2.7 Counters: `cycle`, `time`, `instret`, plus `mcounteren` gating.
2.8 PMP: `pmpcfg0..15`, `pmpaddr0..63`. Implement at least TOR/NAPOT,
    because OpenSBI programs them.
2.9 Pass `rv64mi-p-*` and `rv64si-p-*` from riscv-tests.

## Phase 3: Virtual memory (Sv39, then Sv48)

3.1 Sv39 page-table walk: 3 levels, 4 KiB/2 MiB/1 GiB pages, and PTE bits
    V R W X U G A D.
3.2 Permission checks: U/S access, SUM, MXR, and execute-only pages.
3.3 A/D bits. Either update them in hardware (Svadu) or raise page faults
    (Linux handles both).
3.4 Instruction, load, and store/AMO page faults with the correct `stval`.
3.5 A TLB (direct-mapped or set-associative cache of translations) with
    correct `sfence.vma` semantics (by ASID and by address).
3.6 Instruction fetches that cross a page boundary. A 32-bit instruction can
    straddle two pages under RVC, so the second half can fault.
3.7 Add Sv48 and `satp` mode switching, then rerun the tests
    (`rv64si-p-*`, plus custom page-table torture tests).

## Phase 4: Interrupts and platform devices (QEMU `virt`-compatible)

Matching QEMU's `virt` memory map lets us reuse its device trees, kernel
configs, and OpenSBI's `generic` platform as they are.

4.1 **CLINT/ACLINT**: `mtime`, `mtimecmp`, `msip` at `0x0200_0000`, with
    timer interrupts driven by a virtual clock tied to the instruction count
    (deterministic) or the host clock (interactive).
4.2 **Sstc**: `stimecmp`, so the kernel can program timers without SBI
    calls. Optional, but a big speedup.
4.3 **PLIC** at `0x0c00_0000`: priorities, pending, per-context enable,
    threshold, claim/complete.
4.4 **UART 16550** at `0x1000_0000`: THR/RBR/IER/IIR/LCR/LSR, an RX FIFO fed
    from host stdin in raw mode, and its interrupt wired to the PLIC.
4.5 **Device tree**: generate an FDT blob (DTB) in OCaml for memory, cpus
    (with `riscv,isa` and `riscv,isa-extensions`), CLINT, PLIC, UART, virtio,
    `chosen/bootargs`, and the initrd location.
4.6 **Syscon/test device** (`sifive,test`) for poweroff and reboot.
4.7 **RTC** (goldfish) so the guest knows wall-clock time.

## Phase 5: Firmware and boot flow

5.1 Boot ROM stub: set `a0 = hartid`, `a1 = &dtb`, and jump to firmware at
    `0x8000_0000`.
5.2 Run **OpenSBI** (`fw_jump.bin`, generic platform). Milestone: the
    OpenSBI banner prints on our UART.
5.3 Alternatively, write our own minimal SBI in OCaml, with M-mode handled
    "magically" by the simulator: base, TIME, IPI, RFENCE, HSM, SRST, and
    the legacy console extensions. This is faster to bring up and removes
    the firmware from the debug loop. Keep both options.
5.4 Load the kernel `Image` at `0x8020_0000` and the initramfs high in RAM,
    and pass both through the DTB.
5.5 Milestone: earlycon output from the kernel
    (`earlycon=sbi` or `earlycon=uart8250,mmio,0x10000000`).

## Phase 6: Booting Linux

6.1 Build a minimal kernel (`tinyconfig` + RISC-V + 8250 serial + initramfs)
    with a cross toolchain on the host, plus a BusyBox initramfs.
6.2 Debug the boot. Expect weeks of this: trace modes, a symbolised PC
    trace using `System.map`, and a "last N instructions" ring buffer dumped
    on panic.
6.3 Implement a **GDB remote stub** (the RSP protocol over TCP) so
    `gdb vmlinux` can attach to the guest. This is the single most valuable
    debugging tool for this phase.
6.4 Milestone: the BusyBox shell prompt over the UART.
6.5 Switch to `defconfig` with the full extension set (Zba/Zbb/Zbs in
    `riscv,isa-extensions`) and confirm the kernel's alternatives patching
    works.
6.6 Run the Linux Test Project (LTP) subset and `stress-ng` for basic
    stability.

## Phase 7: Storage, networking, and a real userland

7.1 **virtio-mmio** transport (version 2): device discovery, feature
    negotiation, split virtqueues, used/avail rings, and notifications.
7.2 **virtio-blk** backed by a host disk image (ext4 root filesystem).
7.3 **virtio-net** backed by a user-mode network stack (slirp-style) or a
    TAP device, so the guest can fetch packages.
7.4 **virtio-rng** and **virtio-console** (optional).
7.5 Boot a real distribution root filesystem (Debian riscv64 or Alpine
    riscv64) with systemd or OpenRC.
7.6 Milestone: `apt`/`apk` installs packages inside the guest.

## Phase 8: Performance (needed to compile OCaml in reasonable time)

The detailed targets, measurement method and step-by-step plan are in
[docs/perf](docs/perf/README.md).

Booting Debian takes billions of instructions, and compiling OCaml takes
hundreds of billions. An interpreter that boxes `int64` everywhere runs at
maybe 5–20 MIPS, and we want hundreds.

8.1 Benchmarking harness: MIPS on CoreMark, Dhrystone, and a kernel boot to
    a fixed point. Micro benchmarks and `bench/run.exe` are done;
    **baseline (2026-11-01): 5.7 MIPS** geometric mean, half of it spent
    decoding (docs/perf/reports). C kernels and a boot benchmark remain.
8.2 ✅ Represent registers with unboxed storage: a `Bytes`/Bigarray register
    file accessed with `Bytes.get_int64_le`, or OCaml 5 unboxed types where
    they are available. Build with flambda `-O3`.
8.3 ✅ **Decoded-instruction cache**, per page, so decoding happens
    once. Stores to code pages invalidate it (5.7 → 31 MIPS).
8.4 **Basic-block cache**: pre-decode straight-line blocks into closures
    ("closure compilation"), which removes per-instruction dispatch.
8.5 Make the TLB fast path a single array lookup.
8.6 Run devices and interrupt checks per block, not per instruction.
8.7 Optional: a JIT that emits x86-64 or ARM64, or generates OCaml or C for
    hot blocks. This is a large project; only do it if the closure
    compiler isn't fast enough.
8.8 Multi-hart SMP: harts as OCaml 5 domains, with correct LR/SC and AMO
    semantics across domains and memory-ordering (RVWMO) considerations.

## Phase 9: Toolchain in the guest

9.1 Install `gcc`, `binutils`, `make`, and `git` inside the guest from
    distribution packages.
9.2 Build **OCaml 5.x from source natively on riscv64** inside the guest.
    OCaml has a native riscv64 backend. Running its test suite in the guest
    is a strong whole-system stress test.
9.3 Bootstrap **opam** in the guest and install `dune`, `cmdliner`, and
    `qcheck-core`.
9.4 Milestone: `dune build` of riscvsim succeeds *inside the simulated
    machine*.

## Phase 10: Self-hosting

10.1 Run riscvsim's own test suite inside the guest (user-mode examples
     running on a simulator running on a simulator).
10.2 Run the riscv-tests suite inside the guest copy.
10.3 **Nested boot**: the guest riscvsim boots a tiny Linux (tinyconfig +
     BusyBox). This needs Phase 8 to be fast enough for nested slowdown,
     roughly 100x per level.
10.4 Check that a binary built in the guest matches one built by the host
     cross-compiler, as far as builds are deterministic.
10.5 Write up the results: boot time, MIPS per level, bugs found.

## Stretch goals

- Hypervisor extension (H) to run KVM guests inside the guest.
- Vector extension (RVV 1.0), which RVA23 makes mandatory.
- RVA23 profile completeness: Zicond, Zfa, Zicbom/Zicboz/Zicbop, Zawrs,
  Zihintpause, Zcb, Zimop, Svpbmt, Svinval, Svnapot, Sscofpmf.
- A cycle-approximate timing model (caches, branch prediction, an
  out-of-order core) to estimate performance on chips like the SiFive P870.
- Differential testing against Spike and QEMU in lock-step, comparing
  architectural state after every instruction (the approach riscv-dv and
  Dromajo use).

## Cross-cutting practices

- **Differential testing** against Spike and QEMU at every phase. This is the
  cheapest way to find bugs.
- **Property tests** (QCheck) for decode, translation, and CSR side effects.
- **Determinism**: the virtual clock comes from the instruction count by
  default, so boots replay exactly. That makes bisecting a panic possible.
- **Tracing tools**: instruction trace, CSR-write trace, trap trace, MMIO
  trace, and symbolisation from ELF/`System.map`.
