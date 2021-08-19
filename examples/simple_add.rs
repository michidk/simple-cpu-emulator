use std::f64::INFINITY;

use color_eyre::eyre::Result;

use cpu::memory::{Byte, StdMem, Word};
use cpu::processor::{Instruction, Processor};
use cpu::write_instructions;

/// The main entrypoit. First instruction should be placed here.
const ENTRYPOINT: Word = 0x1FFF;

fn main() -> Result<()> {
    color_eyre::install()?; // rust error handling

    let mut mem = StdMem::default();
    let mut cpu = Processor::new(ENTRYPOINT);

    use cpu::processor::Instruction::*;
    write_instructions!(mem : ENTRYPOINT =>
        NOP,
        LOADC,
        42,
        LOADC,
        58,
        ADD,
        HCF
    );

    while !cpu.t {
        cpu.execute(&mut mem)?;
    }

    let result = mem.read_byte(0x0000);
    println!("Programm Terminated. Result: 0x{:04X} / {}", result, result);

    Ok(())
}
