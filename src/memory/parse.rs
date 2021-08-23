//! 0x1fff:
//!     LOADC 7
//!     LOADC 8
//!     ADD
//!     HCF

use std::borrow::Cow;
use std::error;
use std::{fmt, str::Lines};

use crate::processor::Instruction;

use super::{Byte, Memory, Word};

macro_rules! propagate {
    ( $res:expr ) => {
        match $res {
            Ok(value) => value,
            Err(err) => return Some(Err(err)),
        }
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseErrorKind {
    InvalidAddress { address: usize },
    InvalidConstant,
    InvalidLiteral,
    InvalidMetaCommand,
    InvalidNumber { radix: u32 },
    InvalidInstruction,
    InvalidAddressLabel,
}

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseErrorKind::InvalidAddress { address } => {
                write!(f, "memory has no address `0x{:x}`", address)
            }
            ParseErrorKind::InvalidConstant => f.write_str("invalid constant"),
            ParseErrorKind::InvalidLiteral => f.write_str("invalid literal"),
            ParseErrorKind::InvalidMetaCommand => f.write_str("invalid meta command"),
            ParseErrorKind::InvalidNumber { radix } => {
                write!(f, "failed to parse number with radix `{}`", radix)
            }
            ParseErrorKind::InvalidInstruction => f.write_str("failed to resolve instruction"),
            ParseErrorKind::InvalidAddressLabel => f.write_str("invalid address label"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    kind: ParseErrorKind,
    context: Option<Cow<'static, str>>,
    line_nr: usize,
}

impl ParseError {
    fn new<C, S>(kind: ParseErrorKind, context: C, line_nr: usize) -> Self
    where
        C: Into<Option<S>>,
        S: Into<Cow<'static, str>>,
    {
        Self {
            kind,
            context: context.into().map(|inner| inner.into()),
            line_nr,
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(context) = &self.context {
            write!(
                f,
                "error [ln: {}]: {} - {}",
                self.line_nr, self.kind, context
            )
        } else {
            write!(f, "error [ln: {}]: {}", self.line_nr, self.kind)
        }
    }
}

impl error::Error for ParseError {}

pub type Result<T, E = ParseError> = std::result::Result<T, E>;

macro_rules! parse_number {
    ( $ty:ty: $s:expr ) => {{
        let line = $s;

        if line.trim().is_empty() {
            return None;
        }

        let (radix, offset) = match line.as_bytes() {
            [b'0', b'b', ..] => (2, 2),
            [b'0', b'o', ..] => (8, 2),
            [b'0', b'x', ..] => (16, 2),
            _ => (10, 0),
        };

        Some(<$ty>::from_str_radix(&line[offset..], radix).map_err(|_| radix))
    }};
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Endianess {
    Little,
    Big,
}

impl Default for Endianess {
    fn default() -> Self {
        Self::Big
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ParseSession {
    endianess: Endianess,
}

impl ParseSession {
    fn new(endianess: Endianess) -> Self {
        Self { endianess }
    }
}

#[derive(Debug, Clone)]
pub struct Parser<'a, const S: usize> {
    lines: Lines<'a>,
    line_nr: usize,
    sp: u16,
    memory: Memory<S>,
    endianess: Endianess,
    session: ParseSession,
}

impl<'a, const S: usize> Parser<'a, S> {
    /// Creates a new parse for `data` which will try to populate `memory`.
    pub fn new(data: &'a str, memory: Memory<S>, endianess: Endianess) -> Self {
        Self {
            lines: data.lines(),
            line_nr: 0,
            sp: 0,
            memory,
            endianess,
            session: ParseSession::new(endianess),
        }
    }

    /// Consumes `self` and tries to parse all `self.data` into memory.
    ///
    /// # Errors
    ///
    /// All errors which may occur are collected and returned at the end.
    pub fn parse(mut self) -> Result<Memory<S>, Vec<ParseError>> {
        let mut errors = Vec::new();

        while let Some(res) = self.parse_next_line() {
            if let Err(err) = res {
                log::error!("{}", err);
                errors.push(err);
            }
        }

        if errors.is_empty() {
            Ok(self.memory)
        } else {
            Err(errors)
        }
    }

    /// Tries to parse the next line of [`Parser::data`]. Each instruction
    /// should be located on it's own line.
    fn parse_next_line(&mut self) -> Option<Result<()>> {
        let line = self.lines.next()?.trim();
        self.line_nr += 1;

        if line.is_empty() || line.starts_with('#') {
            // Comment or empty line; skip
            Some(Ok(()))
        } else if line.starts_with('%') {
            // Line is a literal
            self.parse_meta_command(line)
        } else if line.starts_with('!') {
            // Line is a literal
            self.parse_literal(line)
        } else if line.ends_with(':') {
            // Line is a address label.
            self.parse_address_label(line)
        } else {
            // Line is an instruction.
            self.parse_instruction(line)
        }
    }

    /// Tries to parse line a meta command. The `line` should be the whole line
    /// whithout any modifications.
    ///
    /// # Examples
    ///
    /// - `%endianess(le)`
    /// - `%endianess(be)`
    fn parse_meta_command(&mut self, line: &str) -> Option<Result<()>> {
        let line = line.strip_prefix('%').expect("Line is not a meta command");

        log::debug!("[{}] Found meta command", self.line_nr);

        match line {
            "endianess" => {
                self.session.endianess = self.endianess;
            }
            "endianess(le)" => {
                self.session.endianess = Endianess::Little;
            }
            "endianess(be)" => {
                self.session.endianess = Endianess::Big;
            }
            _ => {
                return Some(Err(ParseError::new(
                    ParseErrorKind::InvalidMetaCommand,
                    format!("unknown command `{}`", line),
                    self.line_nr,
                )))
            }
        }

        Some(Ok(()))
    }

    /// Tries to parse line as literal. The `line` should be the whole line
    /// whithout any modifications.
    ///
    /// # Examples
    ///
    /// - `! 0x22`
    /// - `!W0xdead`
    fn parse_literal(&mut self, line: &str) -> Option<Result<()>> {
        let line = line.strip_prefix('!').expect("Line is not a literal line");

        if let Some(line) = line.strip_prefix('W') {
            // Literal is a word
            log::debug!("[{}] Found word literal", self.line_nr);

            let line = line.trim();

            let word =
                propagate!(
                    propagate!(parse_number!(u16: line).ok_or_else(|| ParseError::new(
                        ParseErrorKind::InvalidLiteral,
                        "a literal needs to have a number set",
                        self.line_nr
                    )))
                    .map_err(|radix| {
                        ParseError::new(
                            ParseErrorKind::InvalidLiteral,
                            format!("failed to parse literal as word with radix `{}`", radix),
                            self.line_nr,
                        )
                    })
                );

            match self.session.endianess {
                Endianess::Little => Some(self.write_le_word(word)),
                Endianess::Big => Some(self.write_be_word(word)),
            }
        } else {
            // Literal is a byte
            log::debug!("[{}] Found byte literal", self.line_nr);

            let line = line.trim();

            let byte =
                propagate!(
                    propagate!(parse_number!(u8: line).ok_or_else(|| ParseError::new(
                        ParseErrorKind::InvalidLiteral,
                        "a literal needs to have a number set",
                        self.line_nr
                    )))
                    .map_err(|radix| {
                        ParseError::new(
                            ParseErrorKind::InvalidLiteral,
                            format!("failed to parse literal as byte with radix `{}`", radix),
                            self.line_nr,
                        )
                    })
                );

            Some(self.write_byte(byte))
        }
    }

    /// Tries to parse line as an address label. The `line` should be the whole line
    /// whithout any modifications.
    ///
    /// # Examples
    ///
    /// - `0x22:`
    /// - `0o44:`
    fn parse_address_label(&mut self, line: &str) -> Option<Result<()>> {
        let line = line
            .strip_suffix(':')
            .expect("Line is not an address label");

        log::debug!("[{}] Found address label", self.line_nr);

        // The address is intentionally parsed as an u16 to detect if it's
        // a valid address.
        let address =
            propagate!(
                propagate!(parse_number!(u16: line).ok_or_else(|| ParseError::new(
                    ParseErrorKind::InvalidAddressLabel,
                    "an address label needs to have an address set",
                    self.line_nr
                )))
                .map_err(|radix| {
                    ParseError::new(
                        ParseErrorKind::InvalidAddress {
                            address: usize::MAX,
                        },
                        format!("failed to parse the address with radix `{}`", radix),
                        self.line_nr,
                    )
                })
            );

        log::debug!("[{}] Address label `0x{:x}`", self.line_nr, address);

        self.sp = address;

        Some(Ok(()))
    }

    /// Tries to parse line as a instruction. The `line` should be the whole line
    /// whithout any modifications.
    ///
    /// # Examples
    ///
    /// - `PUSHC 0x22`
    /// - `ADD`
    fn parse_instruction(&mut self, line: &str) -> Option<Result<()>> {
        let line = line.trim();

        let instruction = *propagate!(Instruction::ALL
            .iter()
            .find(|instruction| line == instruction.name())
            .ok_or_else(|| ParseError::new(
                ParseErrorKind::InvalidInstruction,
                "no instruction matching that name was found",
                self.line_nr
            )));

        log::debug!("[{}] Found instruction {}", self.line_nr, instruction);

        Some(self.write_byte(instruction))
    }

    /// Adds `amount` to the position of pointer into memory.
    ///
    /// # Errors
    ///
    /// This will return an error if an overflow would have occurred while
    /// adding [self.sp](`Parser::sp`) and `amount`.
    fn add_memory_position(&mut self, amount: u16) -> Result<()> {
        match self.sp.checked_add(amount) {
            Some(new) => {
                self.sp = new;
                Ok(())
            }
            None => Err(ParseError::new(
                ParseErrorKind::InvalidAddress {
                    address: usize::MAX,
                },
                "address is outside of memory",
                self.line_nr,
            )),
        }
    }

    /// Increments the position of pointer into memory by one.
    ///
    /// # Errors
    ///
    /// This will return an error if an overflow would have occurred while
    /// incrementing [self.sp](`Parser::sp`).
    fn inc_memory_position(&mut self) -> Result<()> {
        self.add_memory_position(1)
    }

    /// Writes `byte` into memory at (self.sp)[`Parser::sp`]. Then it
    /// increments the stack pointer by one.
    ///
    /// # Errors
    ///
    /// This will return an error if an overflow would have occurred while
    /// incrementing [self.sp](`Parser::sp`).
    ///
    /// # Related
    ///
    /// - [`Parser::inc_memory_position`]
    fn write_byte<B: Into<Byte>>(&mut self, byte: B) -> Result<()> {
        self.memory.write_byte(self.sp, byte.into());
        self.inc_memory_position()
    }

    /// Writes big endian `word` into memory at (self.sp)[`Parser::sp`]. Then
    /// it increments the stack pointer by two.
    ///
    /// # Errors
    ///
    /// This will return an error if an overflow would have occurred while
    /// incrementing [self.sp](`Parser::sp`).
    ///
    /// # Related
    ///
    /// - [`Parser::inc_memory_position`]
    fn write_be_word<B: Into<Word>>(&mut self, word: B) -> Result<()> {
        self.memory.write_word(self.sp, word.into());
        self.add_memory_position(2)
    }

    /// Writes little endian `word` into memory at (self.sp)[`Parser::sp`]. Then
    /// it increments the stack pointer by two.
    ///
    /// # Errors
    ///
    /// This will return an error if an overflow would have occurred while
    /// incrementing [self.sp](`Parser::sp`).
    ///
    /// # Related
    ///
    /// - [`Parser::inc_memory_position`]
    fn write_le_word<B: Into<Word>>(&mut self, word: B) -> Result<()> {
        self.memory.write_word(self.sp, word.into().rotate_left(8));
        self.add_memory_position(2)
    }
}

#[cfg(test)]
mod tests {
    use crate::memory::StdMem;
    use std::str::FromStr;

    use super::*;
    use color_eyre::Result;

    #[test]
    fn parse_add() -> Result<()> {
        let data = r#"
            0:
                PUSHC
                !1
                PUSHC
                !2
                ADD
                HCF
        "#;

        let mem = StdMem::from_str(data).unwrap();

        assert_eq!(mem.read_byte(0), Instruction::PUSHC.into());
        assert_eq!(mem.read_byte(1), 1);
        assert_eq!(mem.read_byte(2), Instruction::PUSHC.into());
        assert_eq!(mem.read_byte(3), 2);
        assert_eq!(mem.read_byte(4), Instruction::ADD.into());
        assert_eq!(mem.read_byte(5), Instruction::HCF.into());

        Ok(())
    }

    #[test]
    fn parse_add_offset_decimal() -> Result<()> {
        const ENTRYPOINT: u16 = 1111;
        let data = r#"
            1111:
                PUSHC
                !1
                PUSHC
                !2
                ADD
                HCF
        "#;

        let mem = StdMem::from_str(data).unwrap();

        assert_eq!(mem.read_byte(ENTRYPOINT + 0), Instruction::PUSHC.into());
        assert_eq!(mem.read_byte(ENTRYPOINT + 1), 1);
        assert_eq!(mem.read_byte(ENTRYPOINT + 2), Instruction::PUSHC.into());
        assert_eq!(mem.read_byte(ENTRYPOINT + 3), 2);
        assert_eq!(mem.read_byte(ENTRYPOINT + 4), Instruction::ADD.into());
        assert_eq!(mem.read_byte(ENTRYPOINT + 5), Instruction::HCF.into());

        Ok(())
    }

    #[test]
    fn parse_add_offset_binary() -> Result<()> {
        const ENTRYPOINT: u16 = 0b110011;
        let data = r#"
            0b110011:
                PUSHC
                !1
                PUSHC
                !2
                ADD
                HCF
        "#;

        let mem = StdMem::from_str(data).unwrap();

        assert_eq!(mem.read_byte(ENTRYPOINT + 0), Instruction::PUSHC.into());
        assert_eq!(mem.read_byte(ENTRYPOINT + 1), 1);
        assert_eq!(mem.read_byte(ENTRYPOINT + 2), Instruction::PUSHC.into());
        assert_eq!(mem.read_byte(ENTRYPOINT + 3), 2);
        assert_eq!(mem.read_byte(ENTRYPOINT + 4), Instruction::ADD.into());
        assert_eq!(mem.read_byte(ENTRYPOINT + 5), Instruction::HCF.into());

        Ok(())
    }

    #[test]
    fn parse_add_offset_octal() -> Result<()> {
        const ENTRYPOINT: u16 = 0o711;
        let data = r#"
            0o711:
                PUSHC
                !1
                PUSHC
                !2
                ADD
                HCF
        "#;

        let mem = StdMem::from_str(data).unwrap();

        assert_eq!(mem.read_byte(ENTRYPOINT + 0), Instruction::PUSHC.into());
        assert_eq!(mem.read_byte(ENTRYPOINT + 1), 1);
        assert_eq!(mem.read_byte(ENTRYPOINT + 2), Instruction::PUSHC.into());
        assert_eq!(mem.read_byte(ENTRYPOINT + 3), 2);
        assert_eq!(mem.read_byte(ENTRYPOINT + 4), Instruction::ADD.into());
        assert_eq!(mem.read_byte(ENTRYPOINT + 5), Instruction::HCF.into());

        Ok(())
    }

    #[test]
    fn parse_add_offset_hexadecimal() -> Result<()> {
        const ENTRYPOINT: u16 = 0xdead;
        let data = r#"
            0xdead:
                PUSHC
                !1
                PUSHC
                !2
                ADD
                HCF
        "#;

        let mem = StdMem::from_str(data).unwrap();

        assert_eq!(mem.read_byte(ENTRYPOINT + 0), Instruction::PUSHC.into());
        assert_eq!(mem.read_byte(ENTRYPOINT + 1), 1);
        assert_eq!(mem.read_byte(ENTRYPOINT + 2), Instruction::PUSHC.into());
        assert_eq!(mem.read_byte(ENTRYPOINT + 3), 2);
        assert_eq!(mem.read_byte(ENTRYPOINT + 4), Instruction::ADD.into());
        assert_eq!(mem.read_byte(ENTRYPOINT + 5), Instruction::HCF.into());

        Ok(())
    }

    #[test]
    fn parse_add_literals() -> Result<()> {
        const ENTRYPOINT: u16 = 0xdead;
        let data = r#"
            %endianess(be)
            0xdead:
                !W 0x0110
                !W 0x0210
                !  0x20
                !  0x01
        "#;

        let mem = StdMem::from_str(data).unwrap();

        assert_eq!(mem.read_byte(ENTRYPOINT + 0), Instruction::PUSHC.into());
        assert_eq!(mem.read_byte(ENTRYPOINT + 1), 1);
        assert_eq!(mem.read_byte(ENTRYPOINT + 2), Instruction::PUSHC.into());
        assert_eq!(mem.read_byte(ENTRYPOINT + 3), 2);
        assert_eq!(mem.read_byte(ENTRYPOINT + 4), Instruction::ADD.into());
        assert_eq!(mem.read_byte(ENTRYPOINT + 5), Instruction::HCF.into());

        Ok(())
    }

    #[test]
    fn parse_add_meta_commands() -> Result<()> {
        const ENTRYPOINT: u16 = 0xdead;
        let data = r#"
            0xdead:
                %endianess(be)
                !W 0x0110
                %endianess(le)
                !W 0x1002
                !  0x20
                !  0x01
        "#;

        let mem = StdMem::from_str(data).unwrap();

        assert_eq!(mem.read_byte(ENTRYPOINT + 0), Instruction::PUSHC.into());
        assert_eq!(mem.read_byte(ENTRYPOINT + 1), 1);
        assert_eq!(mem.read_byte(ENTRYPOINT + 2), Instruction::PUSHC.into());
        assert_eq!(mem.read_byte(ENTRYPOINT + 3), 2);
        assert_eq!(mem.read_byte(ENTRYPOINT + 4), Instruction::ADD.into());
        assert_eq!(mem.read_byte(ENTRYPOINT + 5), Instruction::HCF.into());

        Ok(())
    }
}
