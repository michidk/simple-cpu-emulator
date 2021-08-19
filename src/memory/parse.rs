//! 0x1fff:
//!     LOADC 7
//!     LOADC 8
//!     ADD
//!     HCF

use std::convert::TryFrom;
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
    InvalidInstruction,
    InvalidAddressLabel { radix: u32 },
}

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseErrorKind::InvalidAddress { address } => {
                write!(f, "memory has no address `0x{:x}`", address)
            }
            ParseErrorKind::InvalidInstruction => f.write_str("failed to resolve instruction"),
            ParseErrorKind::InvalidAddressLabel { radix } => {
                write!(f, "failed to parse address label with radix `{}`", radix)
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParseError {
    kind: ParseErrorKind,
    line_nr: usize,
}

impl ParseError {
    fn new(kind: ParseErrorKind, line_nr: usize) -> Self {
        Self { kind, line_nr }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error [ln: {}]: {}", self.line_nr, self.kind)
    }
}

pub type Result<T, E = ParseError> = std::result::Result<T, E>;

pub struct Parser<'a, const T: usize> {
    lines: Lines<'a>,
    line_nr: usize,
    sp: usize,
    memory: Memory<T>,
}

impl<'a, const T: usize> Parser<'a, T> {
    pub fn new(data: &'a str, memory: Memory<T>) -> Self {
        Self {
            lines: data.lines(),
            line_nr: 0,
            sp: 0,
            memory,
        }
    }

    pub fn parse(mut self) -> Result<Memory<T>, Vec<ParseError>> {
        let mut errors = Vec::new();

        while let Some(res) = self.parse_next_line() {
            if let Err(err) = res {
                println!("error: {}", err);
                errors.push(err);
            }
        }

        if errors.is_empty() {
            Ok(self.memory)
        } else {
            Err(errors)
        }
    }

    fn parse_next_line(&mut self) -> Option<Result<()>> {
        let line = self.lines.next()?.trim();
        self.line_nr += 1;

        if line.ends_with(':') {
            // Line is likely a address label.

            let radix = match line.as_bytes() {
                &[b'0', b'b', ..] => 2,
                &[b'0', b'o', ..] => 8,
                &[b'0', b'x', ..] => 16,
                _ => 10,
            };

            self.sp = propagate!(u16::from_str_radix(&line[..line.len() - 1], radix).map_err(
                |_| ParseError::new(ParseErrorKind::InvalidAddressLabel { radix }, self.line_nr)
            )) as usize;
        } else {
            // Line is likely an instruction.
            if line == "NOP" {
                self.memory.write_byte(propagate!(self.get_sp()), Instruction::NOP.into());
            } else if line == "HCF" {
                self.memory.write_byte(propagate!(self.get_sp()), Instruction::HCF.into());
            } else if line == "PRINTN" {
            }

            if self.sp.checked_add(1).is_none() {
                // Overflow
            }
        }

        Some(Ok(()))
    }

    fn get_sp(&self) -> Result<u16> {
        u16::try_from(self.sp).map_err(|_| {
            ParseError::new(
                ParseErrorKind::InvalidAddress { address: self.sp },
                self.line_nr,
            )
        })
    }
}
