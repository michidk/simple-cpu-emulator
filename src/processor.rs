use crate::memory::{Byte, Memory, Word};
use color_eyre::eyre::Result;

/// Enumerates the instructions
pub enum Instruction {
    /// No operation
    NOP,
    /// Stop the execution of the program
    HCF, // https://en.wikipedia.org/wiki/Halt_and_Catch_Fire_(computing)
    /// Load constant onto stack
    LOADC,
    /// Add two values on the stack and write result to stack
    ADD,
    /// Negates an integer on the stack
    NEG
}

impl From<Byte> for Instruction {
    /// Converts a byte to an instruction
    fn from(byte: Byte) -> Self {
        match byte {
            0x00 => Instruction::NOP,
            0x01 => Instruction::HCF,
            0x02 => Instruction::LOADC,
            0x03 => Instruction::ADD,
            0x04 => Instruction::NEG,
            _ => panic!("Unknown opcode: 0x{:04X}", byte),
        }
    }
}

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

                println!("NOP");
            }
            Instruction::HCF => {
                self.t = true; // set termination flag
                self.pc += 1;

                println!("HCF");
            }
            Instruction::LOADC => {
                let value = memory.read_byte(self.pc + 1);
                self.pc += 2;

                // write value to stack
                memory.write_byte(self.sp, value);
                self.sp += 1;

                println!("LOADC {}", value);
            }
            Instruction::ADD => {
                let a = memory.read_byte(self.sp - 2);
                let b = memory.read_byte(self.sp - 1);
                self.pc += 1;

                // write result to stack
                self.sp -= 1;
                let result = a + b;
                memory.write_byte(self.sp - 1, result);

                println!("ADD {} {}: {}", a, b, result);
            }
            Instruction::NEG => {
                let value = memory.read_byte(self.sp - 1);
                self.pc += 1;

                // write result to stack
                let result = !value + 0b1;
                memory.write_byte(self.sp - 1, result);

                println!("NEG {}: {}", value, result);
            }
        }

        Ok(())
    }

    /// Runs one execution step
    pub fn execute<const S: usize>(&mut self, memory: &mut Memory<S>) -> Result<()> {
        let opcode = memory.read_byte(self.pc); // Read opcode
        self.execute_instruction(opcode.into(), memory)
    }
}

#[cfg(test)]
mod tests {
    use crate::memory::StdMem;

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

        mem.data[0x1FFF] = Instruction::LOADC as Byte;
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
