# Design: the assembler

How riscvsim turns assembly text into a loadable program. Modules:
`Lexer`, `Parser`, `Assembler`.

## Why riscvsim has its own assembler

Without one, running even a three-line test would need a RISC-V
toolchain installed. With one, `riscvsim run file.s` works anywhere OCaml
does, and the assembler doubles as a test of the encoder: its output is
checked byte for byte against clang.

## The pipeline

```
 text --Lexer--> tokens --Parser--> statements --Assembler--> Image
```

1. **Lexer.** Splits a line into tokens: names, numbers (decimal, `0x`,
   `0b`, up to 64 bits), floats, strings, characters, punctuation. `#`
   and `//` start comments. `1f` and `2b` become local-label references.
2. **Parser.** Groups tokens into a statement: its labels, then a
   mnemonic or directive with operands. Operands stay *generic*: an
   expression, a memory reference `offset(reg)`, or a string. The parser
   doesn't know whether `a0` is a register or a label; that depends on
   the instruction, so the assembler decides. This keeps the grammar tiny.
3. **Local labels.** `1:` may appear many times. Before assembling, each
   definition gets a unique hidden name, and each `1b` or `1f` is
   rewritten to point at the right one.

## Two passes

An instruction can refer to a label defined later, so its address isn't
known when it is first seen. The assembler therefore makes two passes.

**Pass 1: layout.** Walk the statements, give every instruction and data
item an offset within its section, and record each label's position. An
instruction is stored as a small function, `ctx -> Instr.t`, that
produces the final instruction once addresses are known. If that
function works without any addresses (as `add a0, a1, a2` does), it runs
immediately. That's what lets the assembler compress the instruction to
16 bits with `--rvc`, since its size must be fixed in this pass.

**Placement.** Sections are laid out in order (`.text`, `.rodata`,
`.data`, `.bss`), each starting on a new 4 KiB page, beginning at address
`0x10000`, the usual start for Linux programs.

**Pass 2: emit.** Every stored function runs with the real addresses, and
its result is encoded into bytes.

## Pseudo-instructions

Many convenient instructions don't exist in hardware. The assembler
expands them:

- **`li` (load immediate)** must build any 64-bit constant from 12- and
  20-bit pieces. riscvsim uses LLVM's method: a 32-bit value takes
  `lui` + `addiw`, and a larger one builds its upper bits first, then
  shifts left and adds the next 12 bits, repeatedly:

  ```
  li a0, 0x123456789abcdef0     li a1, 5
    lui   a0, 0x247               addi a1, zero, 5
    addiw a0, a0, -1875
    slli  a0, a0, 14            li a2, 0x12345
    addi  a0, a0, -947            lui   a2, 0x12
    slli  a0, a0, 12              addiw a2, a2, 837
    addi  a0, a0, 1511
    slli  a0, a0, 13
    addi  a0, a0, -272
  ```

  A property test checks that the sequence for any 64-bit value really
  produces that value, in 8 instructions or fewer.

- **Addresses** (`la`, `call`, `tail`, `ld rd, label`) use a pair of
  instructions: `auipc` adds the upper part of the distance to the
  current address, and the next instruction adds the lower 12 bits. This
  works wherever the program is loaded. The `%pcrel_hi` / `%pcrel_lo`,
  `%hi` / `%lo` operators are available for writing such pairs by hand.

## Branch targets

A branch's operand can be written two ways. A **label** (`beq a0, a1,
loop`) means "jump to this address". A **plain number** (`beq a0, a1, 8`)
means "jump 8 bytes forward". This is the same convention as LLVM's
assembler, and it lets disassembled output be assembled again unchanged.
A property test does exactly that for random instructions.

## Errors

Each statement is assembled independently, and errors are collected
rather than stopping at the first one. You see every problem at once,
each with its line number:

```
bad.s:2: error: addi a0, a0, 9999: immediate 9999 out of range [-2048, 2047]
bad.s:3: error: undefined symbol missing
```

## Debug calls

Images made by this assembler contain a marker symbol,
`__riscvsim_debug_calls`. It switches on the simple print syscalls (see
the [user manual](../user-manual.md)), which would otherwise collide with
real Linux syscall numbers.

## Limits and alternatives

- **No branch relaxation.** Branches to labels are never compressed,
  because their distance isn't known in pass 1. Real assemblers iterate
  ("relaxation") to shrink them. That's possible later but adds
  complexity for a modest gain.
- **One-pass with fix-ups** (patch addresses once labels appear) is the
  classic alternative. Deferred functions are simpler in OCaml and handle
  multi-instruction pseudo-instructions naturally.
