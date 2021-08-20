use color_eyre::eyre::Result;

use cpu::memory::{Byte, StdMem, Word};
use cpu::processor::Processor;
use cpu::write_instructions;
use simple_logger::SimpleLogger;

/// The main entrypoit. First instruction should be placed here.
const ENTRYPOINT: Word = 0x1FFF;

fn main() -> Result<()> {
    color_eyre::install()?; // rust error handling
    SimpleLogger::new().init().unwrap(); // logging

    let mut mem = StdMem::from_file("examples/programs/readme.asm").unwrap();
    mem.dump();
    let mut cpu = Processor::new(ENTRYPOINT);

    cpu.execute_until_hcf(&mut mem)?;

    Ok(())
}
