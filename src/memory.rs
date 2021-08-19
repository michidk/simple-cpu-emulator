use std::ops::Deref;

pub mod parse;

pub type Byte = u8; // 1 byte
pub type Word = u16; // 2 bytes

/// Default memory
pub type StdMem = Memory<0xFFFF>;

/// Emulates memory for use with the CPU
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Memory<const S: usize> {
    /// The actual data of the memory
    pub data: [Byte; S],
}

impl<const S: usize> Default for Memory<S> {
    /// Initializes the memory
    fn default() -> Self {
        Memory { data: [0; S] }
    }
}

impl<const S: usize> Memory<S> {
    /// Reads a byte from the memory
    pub fn read_byte(&mut self, position: Word) -> Byte {
        self.data[position as usize]
    }

    /// Writes a byte to the memory
    pub fn write_byte(&mut self, position: Word, value: Byte) {
        self.data[position as usize] = value;
    }

    /// Reads a word from the memory (little endian)
    pub fn read_word(&mut self, position: Word) -> Word {
        (self.data[position as usize + 1] as Word) << 8 | (self.data[position as usize] as Word)
    }

    /// Writes a word to the memory (litte endian)
    pub fn write_word(&mut self, position: Word, value: Word) {
        self.data[position as usize] = (value & 0xFF) as Byte;
        self.data[position as usize + 1] = (value >> 8) as Byte;
    }

    /// Writes an array of bytes to the memory
    pub fn write_array(&mut self, position: Word, data: &[Byte]) {
        (&mut self.data[position as usize..position as usize + data.len() as usize])
            .copy_from_slice(data);
    }
}

/// Writes a block of instructions directly into the memory
// Thanks for @Shemnei for helping me with this!
#[macro_export]
macro_rules! write_instructions {
    ( $mem:ident : $pos:expr => $( $byte:expr ),+ ) => {
        // use cpu::processor::Instruction::*;
        // use cpu::memory::Byte;

        $mem.write_array($pos, &[
            $(
                $byte as Byte,
            )+
        ]);
    };
}

#[cfg(test)]
mod tests {
    use crate::processor::Instruction;

    use super::*;
    use color_eyre::eyre::Result;

    #[test]
    fn test_read_byte() -> Result<()> {
        let mut mem = StdMem::default();
        mem.data[0x2] = 0x12;
        assert_eq!(mem.read_byte(0x2), 0x12);

        Ok(())
    }

    #[test]
    fn test_write_byte() -> Result<()> {
        let mut mem = StdMem::default();
        mem.write_byte(0x44, 12);
        assert_eq!(mem.data[0x44], 12);

        Ok(())
    }

    #[test]
    fn test_read_word() -> Result<()> {
        let mut mem = StdMem::default();
        mem.data[0] = 0x12;
        mem.data[1] = 0x34;
        assert_eq!(mem.read_word(0), 0x3412); // little endian

        Ok(())
    }

    #[test]
    fn test_write_word() -> Result<()> {
        let mut mem = StdMem::default();
        mem.write_word(0x44, 0x1234);
        assert_eq!(mem.data[0x44], 0x34); // little endian
        assert_eq!(mem.data[0x45], 0x12);

        Ok(())
    }

    #[test]
    fn test_write_array() -> Result<()> {
        let mut mem = StdMem::default();
        mem.write_array(0x44, &[0x12, 0x34, 0x56, 0x78]);
        assert_eq!(mem.data[0x44], 0x12);
        assert_eq!(mem.data[0x45], 0x34);
        assert_eq!(mem.data[0x46], 0x56);
        assert_eq!(mem.data[0x47], 0x78);

        Ok(())
    }

    #[test]
    fn test_write_instructions() -> Result<()> {
        let mut mem = StdMem::default();

        mem.write_array(
            0x1FFF,
            &[
                Instruction::NOP as Byte,
                Instruction::PUSHC as Byte,
                42,
                Instruction::PUSHC as Byte,
                58,
                Instruction::ADD as Byte,
                Instruction::HCF as Byte,
            ],
        );

        let mut mem2 = StdMem::default();
        use crate::processor::Instruction::*;
        write_instructions!(mem2 : 0x1FFF => NOP, PUSHC, 42, PUSHC, 58, ADD, HCF);

        assert_eq!(mem, mem2);

        Ok(())
    }
}
