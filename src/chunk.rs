use std::convert::TryFrom;

use num_enum::TryFromPrimitive;

use crate::object::ObjectPtr;

#[derive(Debug, TryFromPrimitive, Copy, Clone)]
#[repr(u8)]
pub enum OpCode {
    Return,
    Constant,
    True,
    False,
    Nil,
    Negate,
    Not,
    And,
    Or,
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    Less,
    Greater,
    Print,
    Pop,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
}

#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<ObjectPtr>,
    // Using run-length encoding
    // The first element of tuple represents the line number, and the second, number of repeats
    lines: Vec<(usize, usize)>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: vec![],
            constants: vec![],
            lines: vec![],
        }
    }

    pub fn write(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.encode_line(line);
    }

    fn encode_line(&mut self, line: usize) {
        match self.lines.last_mut() {
            Some((prev_line, ref mut repeat)) if *prev_line == line => *repeat += 1,
            _ => self.lines.push((line, 1)),
        }
    }

    pub fn add_constant(&mut self, ptr: ObjectPtr) -> usize {
        let index = self.constants.len();
        self.constants.push(ptr);
        index
    }

    pub fn disassemble(&self, name: &str) {
        println!("=== {} ===", name);

        if !self.code.is_empty() {
            let mut offset = 0;

            let mut lines_index = 0;
            let mut count = self.lines[lines_index].1;

            while offset < self.code.len() {
                // Some line if changed line, otherwise None (same as before)
                let line = if offset >= count {
                    lines_index += 1;
                    count += self.lines[lines_index].1;

                    Some(self.lines[lines_index].0)
                } else if offset == 0 {
                    Some(self.lines[lines_index].0)
                } else {
                    None
                };

                self.disassemble_instruction(&mut offset, line);
            }
        }
    }

    pub fn get_line(&self, offset: usize) -> usize {
        let mut count = 0;
        for (line, repeats) in &self.lines {
            count += repeats;
            if offset < count {
                return *line;
            }
        }

        panic!("Line not encoded for offset {}.", offset);
    }
}

impl Chunk {
    fn disassemble_instruction(&self, offset: &mut usize, line: Option<usize>) {
        let instruction = self.code[*offset];

        let line_str = if let Some(line) = line {
            format!("{:4}", line)
        } else {
            "   |".to_string()
        };

        print!("{:04} {} ", offset, line_str);

        match OpCode::try_from(instruction) {
            Ok(op) => match op {
                OpCode::Return => self.simple_instruction("RETURN", offset),
                OpCode::Constant => self.constant_instruction("CONSTANT", offset),
                OpCode::True => self.simple_instruction("TRUE", offset),
                OpCode::False => self.simple_instruction("FALSE", offset),
                OpCode::Nil => self.simple_instruction("NIL", offset),
                OpCode::Negate => self.simple_instruction("NEGATE", offset),
                OpCode::Not => self.simple_instruction("NOT", offset),
                OpCode::And => self.simple_instruction("AND", offset),
                OpCode::Or => self.simple_instruction("OR", offset),
                OpCode::Add => self.simple_instruction("ADD", offset),
                OpCode::Subtract => self.simple_instruction("SUBTRACT", offset),
                OpCode::Multiply => self.simple_instruction("MULTIPLY", offset),
                OpCode::Divide => self.simple_instruction("DIVIDE", offset),
                OpCode::Equal => self.simple_instruction("EQUAL", offset),
                OpCode::Less => self.simple_instruction("LESS", offset),
                OpCode::Greater => self.simple_instruction("GREATER", offset),
                OpCode::Print => self.simple_instruction("PRINT", offset),
                OpCode::Pop => self.simple_instruction("POP", offset),
                OpCode::DefineGlobal => self.constant_instruction("DEFINE_GLOBAL", offset),
                OpCode::GetGlobal => self.constant_instruction("GET_GLOBAL", offset),
                OpCode::SetGlobal => self.constant_instruction("SET_GLOBAL", offset),
                OpCode::GetLocal => self.byte_instruction("GET_LOCAL", offset),
                OpCode::SetLocal => self.byte_instruction("SET_LOCAL", offset),
                // OpCode::GetLocal => self.constant_instruction("SET_GLOBAL", offset),
            },
            Err(_) => {
                println!("Unknown opcode {}", instruction);
                *offset += 1
            },
        }
    }

    fn simple_instruction(&self, name: &str, offset: &mut usize) {
        println!("{:<16}", name);
        *offset += 1;
    }

    fn constant_instruction(&self, name: &str, offset: &mut usize) {
        let constant = self.code[*offset + 1];
        println!("{:<16} {:4} '{:?}'", name, constant, self.constants[constant as usize]);
        *offset += 2
    }

    fn byte_instruction(&self, name: &str, offset: &mut usize) {
        let byte = self.code[*offset + 1];
        println!("{:<16} {:4}", name, byte);
        *offset += 2
    }
}
