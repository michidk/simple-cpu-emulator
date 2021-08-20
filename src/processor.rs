use std::convert::TryFrom;

use crate::memory::{Memory, Word};
use color_eyre::eyre::{Result, WrapErr};
use log::*;
use num_enum::IntoPrimitive;
use num_enum::TryFromPrimitive;

/// Emulates a CPU
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Processor {
    /// Program counter
    pub pc: Word,
    /// Stack Pointer
    pub sp: Word,
    /// Termination flag. Should be set to true when the program is finished
    pub t: bool,
}

impl Default for Processor {
    /// Initializes a new CPU
    fn default() -> Self {
        Self::new(0x1FFF)
    }
}

impl Processor {
    /// Initializes a new CPU
    /// @param entrypoint The start of the program
    pub fn new(entrypoint: Word) -> Self {
        Self {
            pc: entrypoint,
            sp: 0x0000,
            t: false,
        }
    }

    /// Executes a single instruction
    pub fn execute_instruction<const S: usize>(
        &mut self,
        instruction: Instruction,
        memory: &mut Memory<S>,
    ) -> Result<()> {
        match instruction {
            Instruction::NOP => {
                self.pc += 1;

                debug!("NOP");
            }
            Instruction::HCF => {
                self.t = true; // set termination flag
                self.pc += 1;

                debug!("HCF");
            }
            Instruction::PRINTN => {
                let value = memory.read_byte(self.sp - 1);
                self.pc += 1;

                info!("{}", value.to_string());
            }
            Instruction::PUSHC => {
                let value = memory.read_byte(self.pc + 1);
                self.pc += 2;

                // write value to stack
                memory.write_byte(self.sp, value);
                self.sp += 1;

                debug!("LOADC {}", value);
            }
            Instruction::DUP => {
                let value = memory.read_byte(self.sp - 1);
                self.pc += 1;

                // write result to stack
                memory.write_byte(self.sp, value);
                self.sp += 1;

                debug!("DUP {}", value);
            }
            Instruction::ADD => {
                let a = memory.read_byte(self.sp - 2);
                let b = memory.read_byte(self.sp - 1);
                self.pc += 1;

                // write result to stack
                self.sp -= 1;
                let result = a.wrapping_add(b);
                memory.write_byte(self.sp - 1, result);

                debug!("ADD {} {}: {}", a, b, result);
            }
            Instruction::NEG => {
                let value = memory.read_byte(self.sp - 1);
                self.pc += 1;

                // write result to stack
                let result = !value + 0b1;
                memory.write_byte(self.sp - 1, result);

                debug!("NEG {}: {}", value, result);
            }
            Instruction::JUMP => {
                let addr = memory.read_word(self.pc + 1);
                self.pc = addr;

                debug!("JUMP {}", addr);
            }
            Instruction::JUMPZ => {
                let addr = memory.read_word(self.pc + 1);
                self.pc += 3;
                let value = memory.read_byte(self.sp - 1);
                self.sp -= 1;

                if value == 0 {
                    self.pc = addr;
                }

                debug!("JUMPZ {}: {}", addr, value);
            }
        }

        Ok(())
    }

    /// Runs one execution step
    pub fn execute<const S: usize>(&mut self, memory: &mut Memory<S>) -> Result<()> {
        let opcode = memory.read_byte(self.pc); // Read opcode where PC is
        let instruction = Instruction::try_from(opcode)
            .wrap_err_with(|| format!("Invalid opcode: 0x{:02X}", opcode))?;
        self.execute_instruction(instruction, memory)
    }

    /// Run program until a termination condition is met
    pub fn execute_until_hcf<const S: usize>(&mut self, memory: &mut Memory<S>) -> Result<()> {
        while !self.t {
            self.execute(memory)?;
        }

        let result = memory.read_byte(0x0000);
        info!("Programm Terminated. Result: 0x{:04X} / {}", result, result);

        Ok(())
    }
}

macro_rules! instructions {
    ( $( $( #[doc = $doc:expr] )+ $name:ident = $repr:literal , )+ ) => {
        /// Defines then instructions
        /// For now the instructions all operate on the stacks, without registers
        #[repr(u8)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
        #[derive(TryFromPrimitive, IntoPrimitive)]
        pub enum Instruction {
            $(
                $( #[doc = $doc] )+
                $name = $repr,
            )+
        }

        impl Instruction {
            pub const ALL: &'static [Self] = &[
                $( Self::$name , )+
            ];

            pub fn name(&self) -> &'static str {
                match self {
                    $( Self::$name => stringify!($name) , )+
                }
            }
        }

        impl ::std::fmt::Display for Instruction {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                match self {
                    $( Self::$name => f.write_str(stringify!($name)) , )+
                }
            }
        }
    }
}

instructions! {
    /// No operation
    NOP = 0x00,
    /// Stop the execution of the program
    HCF = 0x01, // https://en.wikipedia.org/wiki/Halt_and_Catch_Fire_(computing)
    /// Prints a decimal number from the stack to the console without consuming the value
    PRINTN = 0x05,
    /// Load constant onto stack
    /// @param value The value to load
    PUSHC = 0x10,
    /// Duplicates a value on the stack
    DUP = 0x15,
    /// Add two values on the stack and write result to stack
    ADD = 0x20,
    /// Negates an integer on the stack
    NEG = 0x30,
    /// Jump to an address
    /// @param adress The adress to jump to
    JUMP = 0x40,
    /// Jump conditionally to an address depending on the value on the stack
    /// @param adress The adress to jump to
    JUMPZ = 0x41,
}

#[cfg(test)]
mod tests {
    use crate::memory::{Byte, StdMem};

    use super::*;
    use color_eyre::eyre::Result;

    #[test]
    fn test_no_operation() -> Result<()> {
        let mut mem = StdMem::default();
        let mut cpu = Processor::default();

        mem.data[0x1FFF] = Instruction::NOP as Byte;
        cpu.execute(&mut mem)?;

        assert_eq!(mem, StdMem::default());
        let mut cpu2 = Processor::default();
        cpu2.pc += 1;
        assert_eq!(cpu, cpu2);

        Ok(())
    }

    #[test]
    fn test_halt_and_catch_fire() -> Result<()> {
        let mut mem = StdMem::default();
        let mut cpu = Processor::default();

        mem.data[0x1FFF] = Instruction::HCF as Byte;
        cpu.execute(&mut mem)?;

        assert!(cpu.t);

        Ok(())
    }

    #[test]
    fn test_load_constant() -> Result<()> {
        let mut mem = StdMem::default();
        let mut cpu = Processor::default();

        mem.data[0x1FFF] = Instruction::PUSHC as Byte;
        mem.data[0x2000] = 42;
        cpu.execute(&mut mem)?;

        assert_eq!(mem.read_byte(0), 42);

        Ok(())
    }

    #[test]
    fn test_add() -> Result<()> {
        let mut mem = StdMem::default();
        let mut cpu = Processor::default();

        mem.data[0x0000] = 1;
        mem.data[0x0001] = 2;
        cpu.sp = 0x0002;
        mem.data[0x1FFF] = Instruction::ADD as Byte;
        cpu.execute(&mut mem)?;

        assert_eq!(mem.read_byte(0x0000), 3);

        Ok(())
    }

    #[test]
    fn test_neg() -> Result<()> {
        let mut mem = StdMem::default();
        let mut cpu = Processor::default();

        mem.data[0x0000] = 10;
        cpu.sp = 0x0001;
        mem.data[0x1FFF] = Instruction::NEG as Byte;
        cpu.execute(&mut mem)?;

        assert_eq!(mem.data[0x0000], -10i8 as Byte);

        Ok(())
    }
}
