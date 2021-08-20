Simple CPU Emulator
====

A simple CPU emulator written in Rust.

This is mainly a learning project. The architecture is very simple and completely made up. Some inspiration came from the [6502](https://en.wikipedia.org/wiki/MOS_Technology_6502) chip and the book ["Compiler Design: Virtual Machines"](https://www.springer.com/de/book/9783642149085).

The code is fully documented. A list of instructions can be found [here](https://github.com/michidk/simple-cpu-emulator/blob/main/src/processor.rs#L9). Examples are in the `examples/` folder.

## Example

Count from 10 to 1:
```assembly
0x1fff:
    PUSHC
    10
    # print the number
    PRINTN
    # subtract one
    PUSHC
    1
    NEG
    ADD
    DUP
    # check if we are done
    JUMPZ
    0x0D
    0x20
    # jump back to the print statement
    JUMP
    0x01
    0x20
    # halt
    HCF
```
