use color_eyre::eyre::Result;

use cpu::memory::{Byte, StdMem, Word};
use cpu::processor::{Instruction, Processor};

/// The main entrypoit. First instruction should be placed here.
const ENTRYPOINT: Word = 0x1FFF;

fn main() -> Result<()> {
    color_eyre::install()?; // rust error handling

    let mut mem = StdMem::default();
    let mut cpu = Processor::new(ENTRYPOINT);

    mem.write_byte(ENTRYPOINT, Instruction::NOP as Byte); // opcode for first instruction
    mem.write_byte(ENTRYPOINT + 1, Instruction::LOADC as Byte);
    mem.write_byte(ENTRYPOINT + 2, 42);
    mem.write_byte(ENTRYPOINT + 3, Instruction::LOADC as Byte);
    mem.write_byte(ENTRYPOINT + 4, 1);
    mem.write_byte(ENTRYPOINT + 5, Instruction::ADD as Byte);
    mem.write_byte(ENTRYPOINT + 10, Instruction::HCF as Byte);

    while !cpu.t {
        cpu.execute(&mut mem)?;
    }

    let result = mem.read_byte(0x0000);
    println!("Programm Terminated. Result: 0x{:04X} / {}", result, result);

    Ok(())
}
