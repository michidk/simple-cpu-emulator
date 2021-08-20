use color_eyre::eyre::Result;

use cpu::memory::{Byte, StdMem, Word};
use cpu::processor::Processor;
use cpu::write_instructions;
use log::LevelFilter;
use simple_logger::SimpleLogger;

/// The main entrypoit. First instruction should be placed here.
const ENTRYPOINT: Word = 0x1FFF;

fn main() -> Result<()> {
    color_eyre::install()?; // rust error handling
    SimpleLogger::new()
        .with_level(LevelFilter::Info)
        .init()
        .unwrap(); // logging

    let mut mem = StdMem::default();
    let mut cpu = Processor::new(ENTRYPOINT);

    use cpu::processor::Instruction::*;
    write_instructions!(mem : ENTRYPOINT =>
        PUSHC,
        10,
        PRINTN,
        PUSHC,
        1,
        NEG,
        ADD,
        DUP,
        JUMPZ,
        0x0D,
        0x20,
        JUMP,
        0x01,
        0x20,
        HCF
    );

    cpu.execute_until_hcf(&mut mem)?;

    Ok(())
}
