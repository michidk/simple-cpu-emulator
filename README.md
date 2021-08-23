Simple CPU Emulator
====

A simple CPU emulator written in Rust.

This is mainly a learning project. The architecture is very simple and completely made up. Some inspiration came from the [6502](https://en.wikipedia.org/wiki/MOS_Technology_6502) chip and the book ["Compiler Design: Virtual Machines"](https://www.springer.com/de/book/9783642149085).

The code is fully documented. A list of instructions can be found [here](https://github.com/michidk/simple-cpu-emulator/blob/main/src/processor.rs#L9). Examples are in the `examples/` folder.

## Examples

Count from 10 to 1:
```assembly
0x1fff:
    PUSHC
    !10
    # print the number
    PRINTN
    # subtract one
    PUSHC
    !1
    NEG
    ADD
    DUP
    # check if we are done
    JUMPZ
    !W0x200D
    # jump back to the print statement
    JUMP
    !W0x2001
    # halt
    HCF
```

More examples can be found in the `examples/` folder.
They can be run with `cargo run --example <example name>`.

## Usage

The assembly instructions can either be specified directly in Rust or loaded from a file.

### Parse assembly file

Parse an `.asm` file:

```rust
let mut mem = StdMem::from_file("examples/programs/add.asm").unwrap();
let mut cpu = Processor::new(ENTRYPOINT);

cpu.execute_until_hcf(&mut mem)?;
```

An example is located at `examples/parse.rs`.

### Define instructions in Rust

Define instructions in Rust using the `write_instructions` macro:

```rust
let mut mem = StdMem::default();
let mut cpu = Processor::new(ENTRYPOINT);

use cpu::processor::Instruction::*;
write_instructions!(mem : ENTRYPOINT =>
    PUSHC,
    42,
    HCF
);

cpu.execute_until_hcf(&mut mem)?;
```

An example is located at `examples/add.rs`.
