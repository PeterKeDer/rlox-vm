use std::convert::TryFrom;
use std::collections::HashMap;

use crate::error::{Error, Result};
use crate::chunk::{Chunk, OpCode};
use crate::object::{Object, ObjectType, ObjectPtr};
use crate::allocator::ObjectAllocator;

macro_rules! binary_op {
    ( $vm:expr, $op:tt, $ident_ty:ident, $res_ty:ident ) => {
        {
            let b = $vm.pop();
            let a = $vm.pop();

            match (&*a, &*b) {
                (Object::$ident_ty(a), Object::$ident_ty(b)) => Ok($vm.alloc_push(Object::$res_ty(*a $op *b))),
                _ => Err(Error::new(
                    format!("The operator '{}' can only be used on two numbers.", stringify!($op)),
                    $vm.chunk.get_line($vm.index),
                )),
            }
        }
    };
}

pub struct VM<Allocator: ObjectAllocator> {
    pub allocator: Allocator,
    chunk: Chunk,
    index: usize,
    stack: Vec<ObjectPtr>,
    singletons: Vec<ObjectPtr>,
    globals: HashMap<String, ObjectPtr>,
}

impl<Allocator> VM<Allocator>
    where Allocator: ObjectAllocator
{
    pub fn new(chunk: Chunk, allocator: Allocator) -> VM<Allocator> {
        let mut vm = VM {
            allocator,
            chunk,
            index: 0,
            stack: vec![],
            singletons: vec![],
            globals: HashMap::new(),
        };

        for obj in vec![Object::Nil, Object::Bool(true), Object::Bool(false)] {
            let ptr = vm.alloc(obj);
            vm.singletons.push(ptr);
        }

        vm
    }

    pub fn interpret(&mut self, chunk: Chunk) -> Result<()> {
        self.chunk = chunk;
        self.index = 0;
        self.stack = vec![];

        loop {
            // Return if reached the end of bytecode.
            let byte = match self.read_byte() {
                Some(byte) => byte,
                None => return Ok(()),
            };

            let code = match OpCode::try_from(byte) {
                Ok(code) => code,
                Err(_) => return Err(Error::new(
                    format!("Invalid byte {}.", byte),
                    self.chunk.get_line(self.index),
                )),
            };

            match code {
                OpCode::Return => return Ok(()),
                OpCode::Constant => {
                    let value = self.read_constant();
                    self.push(value);
                },
                OpCode::True => self.push(self.get_bool(true)),
                OpCode::False => self.push(self.get_bool(false)),
                OpCode::Nil => self.push(self.get_nil()),
                OpCode::Negate => {
                    match self.pop().as_number() {
                        Some(n) => self.alloc_push(Object::Number(-n)),
                        _ => return Err(Error::new(
                            "The '-' unary operator can only be used on numbers.".to_string(),
                            self.chunk.get_line(self.index),
                        )),
                    }
                },
                OpCode::Not => {
                    let value = self.stack.pop().unwrap();
                    self.push(self.get_bool(is_falsey(&*value)));
                },
                OpCode::And => binary_op!(self, &&, Bool, Bool)?,
                OpCode::Or => binary_op!(self, ||, Bool, Bool)?,
                OpCode::Add => {
                    match (self.peek(1).get_type(), self.peek(0).get_type()) {
                        (ObjectType::Number, ObjectType::Number) => binary_op!(self, +, Number, Number)?,
                        (ObjectType::String, ObjectType::String) => {
                            let b = self.pop();
                            let a = self.pop();
                            let string = format!("{}{}", a.unwrap_string(), b.unwrap_string());
                            self.alloc_push(Object::String(string));
                        }
                        _ => return Err(Error::new(
                            "The '+' operator can only be used by two Strings or two Numbers.".to_string(),
                            self.chunk.get_line(self.index),
                        )),
                    }
                },
                OpCode::Subtract => binary_op!(self, -, Number, Number)?,
                OpCode::Multiply => binary_op!(self, *, Number, Number)?,
                OpCode::Divide => binary_op!(self, /, Number, Number)?,
                OpCode::Equal => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(self.get_bool(self.values_equal(&*a, &*b)));
                },
                OpCode::Less => binary_op!(self, <, Number, Bool)?,
                OpCode::Greater => binary_op!(self, >, Number, Bool)?,
                OpCode::Print => {
                    let value = self.pop();
                    self.print_value(&*value);
                },
                OpCode::Pop => {
                    self.pop();
                },
                OpCode::DefineGlobal => {
                    let name = self.read_constant().unwrap_string().clone();
                    let value = self.pop();

                    self.globals.insert(name, value);
                },
                OpCode::GetGlobal => {
                    let name_constant = self.read_constant();
                    let name = name_constant.unwrap_string();

                    let value = match self.globals.get(name) {
                        Some(value) => value.clone(),
                        None => return Err(Error::new(
                            format!("Undefined variable {}.", name),
                            self.chunk.get_line(self.index - 1),
                        )),
                    };

                    self.push(value);
                },
                OpCode::SetGlobal => {
                    let name = self.read_constant().unwrap_string().clone();
                    let value = self.peek(0).clone();

                    if self.globals.contains_key(&name) {
                        self.globals.insert(name, value);
                    } else {
                        return Err(Error::new(
                            format!("Undefined variable {}.", name),
                            self.chunk.get_line(self.index - 1),
                        ));
                    }
                },
                OpCode::GetLocal => {
                    let slot = self.read_byte().expect("Expect local slot.") as usize;
                    self.push(self.stack[slot].clone());
                },
                OpCode::SetLocal => {
                    let slot = self.read_byte().expect("Expect local slot.") as usize;
                    // Peek is used here since assignments are also expressions
                    self.stack[slot] = self.peek(0).clone();
                },
                OpCode::Jump => {
                    let offset = self.read_short() as usize;
                    self.index += offset;
                },
                OpCode::JumpIfFalse => {
                    let offset = self.read_short() as usize;
                    if is_falsey(self.peek(0)) {
                        self.index += offset;
                    }
                },
                OpCode::Loop => {
                    let offset = self.read_short() as usize;
                    self.index -= offset;
                }
            }
        }
    }

    pub fn free_objects(self) {
        self.allocator.destroy();
    }

    fn values_equal(&self, a: &Object, b: &Object) -> bool {
        match (a, b) {
            (Object::Nil, Object::Nil) => true,
            (Object::Bool(a), Object::Bool(b)) => a == b,
            (Object::Number(a), Object::Number(b)) => a == b,
            (Object::String(a), Object::String(b)) => a == b,
            _ => false,
        }
    }

    fn peek(&self, n: usize) -> &ObjectPtr {
        &self.stack[self.stack.len() - 1 - n]
    }
}

impl<Allocator> VM<Allocator>
    where Allocator: ObjectAllocator
{
    fn pop(&mut self) -> ObjectPtr {
        self.stack.pop().unwrap()
    }

    fn push(&mut self, value: ObjectPtr) {
        self.stack.push(value);
    }

    fn read_byte(&mut self) -> Option<u8> {
        if self.index < self.chunk.code.len() {
            let value = self.chunk.code[self.index];
            self.index += 1;
            Some(value)
        } else {
            None
        }
    }

    fn read_short(&mut self) -> u16 {
        let value = ((self.chunk.code[self.index] as u16) << 8) | self.chunk.code[self.index + 1] as u16;
        self.index += 2;
        value
    }

    fn read_constant(&mut self) -> ObjectPtr {
        let byte = self.read_byte().expect("Constant not encoded.");
        self.chunk.constants[byte as usize].clone()
    }

    fn print_value(&self, object: &Object) {
        println!("{}", object);
    }

    fn get_nil(&self) -> ObjectPtr {
        self.singletons[0].clone()
    }

    fn get_bool(&self, value: bool) -> ObjectPtr {
        if value {
            self.singletons[1].clone()
        } else {
            self.singletons[2].clone()
        }
    }

    fn alloc(&mut self, object: Object) -> ObjectPtr {
        self.allocator.allocate(object)
    }

    fn alloc_push(&mut self, object: Object) {
        let ptr = self.alloc(object);
        self.push(ptr);
    }
}

fn is_falsey(value: &Object) -> bool {
    match value {
        Object::Bool(false) | Object::Nil => true,
        _ => false,
    }
}
