# Design: instructions and their encodings

How riscvsim represents RISC-V instructions, and how it converts them to
and from bits. Modules: `Instr`, `Opcode`, `Encode`, `Decode`,
`Compressed`.

## What an instruction looks like

Every RISC-V instruction is a number. Most are 32 bits, split into
fields. For example, `add a0, a1, a2` (register a0 := a1 + a2) uses the
**R format**:

```
 31      25 24   20 19   15 14  12 11    7 6       0
+----------+-------+-------+------+-------+---------+
|  funct7  |  rs2  |  rs1  |funct3|  rd   | opcode  |
| 0000000  | 01100 | 01011 | 000  | 01010 | 0110011 |
+----------+-------+-------+------+-------+---------+
            a2=x12  a1=x11   add   a0=x10    OP
```

The *opcode* picks a broad class ("register-register arithmetic"), and
the *funct* fields pick the exact operation. There are six formats (R, I,
S, B, U, J). They differ in where an *immediate* (a constant inside the
instruction) is stored. Branch and jump immediates are split into
scattered pieces, so that the hardware can always find the sign bit in
bit 31.

The compressed extension adds 16-bit versions of common instructions,
recognisable because their lowest two bits aren't `11`.

## Representing instructions: `Instr.t`

Inside riscvsim an instruction isn't a number but a value of the OCaml
type `Instr.t`, a *variant* with one case per kind of instruction:

```ocaml
type t =
  | Op of { op : reg_op; rd : Reg.t; rs1 : Reg.t; rs2 : Reg.t }
  | Load of { op : load_op; rd : Reg.t; rs1 : Reg.t; offset : int }
  | Branch of { op : branch_op; rs1 : Reg.t; rs2 : Reg.t; offset : int }
  | Farith of { op : farith_op; fmt : fmt; rd : Freg.t; ...; rm : rm }
  ...
```

Operations that share a shape share a case: all 51 register-register
operations (`add`, `mul`, `sh1add`, ...) are one `Op` case, told apart by
the small enumeration `reg_op`.

Why a typed value instead of raw bits:

- **Impossible values can't exist.** A floating-point register can't end
  up where an integer register belongs, because `Reg.t` and `Freg.t` are
  different types.
- **The compiler checks completeness.** The encoder, decoder, compressor
  and executor each `match` on `Instr.t`. Add a new instruction and the
  compiler points at every place that must handle it.
- **Immediates are plain numbers.** A branch offset is stored as `-8`,
  not as scattered bit fields. The one exception is `lui`/`auipc`, whose
  20-bit field is kept exactly as written in assembly
  (`lui a0, 0x12345`).

## One source of truth: `Opcode`

Every opcode and funct value lives in one table in `Opcode`:

```ocaml
let reg_funct = [ (Add, (op, 0, 0x00)); (Sub, (op, 0, 0x20)); ... ]
```

`Encode` reads the tables forwards (operation to bits) and `Decode` reads
them backwards (bits to operation). Because both use the same table, they
can't disagree. The same pattern holds for spelling: `Instr` has one
table of mnemonics, used by both the assembler and the disassembler.

## Encoding and decoding

- `Encode.encode : Instr.t -> (int, string) result` builds the 32-bit
  word. It checks every immediate's range and returns an error such as
  `immediate 5000 out of range [-2048, 2047]`. The assembler shows that
  message with a line number.
- `Decode.decode : int -> Instr.t option` returns `None` for words that
  aren't valid instructions. It is **strict**: it rejects reserved
  encodings instead of guessing. A property test checks that every word
  it accepts re-encodes to exactly the same bits.

## Compressed instructions

A compressed instruction is just a shorter spelling of an ordinary one:
`c.addi a0, 1` means exactly `addi a0, a0, 1`. So there's no separate
type. `Compressed.expand` turns a 16-bit value directly into the
equivalent `Instr.t`, and the CPU never knows the difference, except that
the program counter advances by 2 instead of 4.

The reverse, `Compressed.compress`, is used by the assembler with
`--rvc`. Sometimes it picks an *equivalent* rather than identical
instruction. `addi a0, a1, 0` becomes `c.mv a0, a1`, which expands to
`add a0, zero, a1`; the result is the same, though the instruction
differs. Where several compressed forms fit, it makes the same choice as
clang, so that our output is byte-identical to clang's.

## How it is verified

- Random instructions survive `decode (encode i) = i` (20,000 per run).
- All 65,536 possible 16-bit values are expanded and checked.
- About 6,000 random instructions, plus 4,000 compression candidates, are
  assembled by both clang and riscvsim, and the bytes must match exactly.

## Alternatives considered

- **Decode straight into executable closures.** Faster, but it gives up
  the printable, comparable `Instr.t` that the assembler, disassembler and
  tests rely on. It is planned as a separate cache layer instead (see
  [the optimization plan](../perf/plan.md), step O5).
- **A separate type for compressed instructions.** It would double the
  executor for no gain, since every compressed instruction has an exact
  32-bit meaning.
