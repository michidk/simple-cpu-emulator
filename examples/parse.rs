use color_eyre::eyre::Result;

use cpu::memory::{StdMem, Word};
use cpu::processor::Processor;
use simple_logger::SimpleLogger;

/// The main entrypoit. First instruction should be placed here.
const ENTRYPOINT: Word = 0x1FFF;

fn main() -> Result<()> {
    color_eyre::install()?; // rust error handling
    SimpleLogger::new().init().unwrap(); // logging

    let mut mem = StdMem::from_file("examples/programs/add.asm").unwrap();
    let mut cpu = Processor::new(ENTRYPOINT);

    cpu.execute_until_hcf(&mut mem)?;

    Ok(())
}
