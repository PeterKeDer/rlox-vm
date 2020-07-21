use std::convert::TryFrom;

use crate::error::Error;
use crate::chunk::{Chunk, OpCode};
use crate::object::{Object, ObjectType, ObjectPtr};

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

pub struct VM {
    chunk: Chunk,
    index: usize,
    stack: Vec<ObjectPtr>,
    objects: Vec<ObjectPtr>,
    singletons: Vec<ObjectPtr>,
}

impl VM {
    pub fn new(chunk: Chunk) -> VM {
        let mut vm = VM {
            chunk,
            index: 0,
            stack: vec![],
            objects: vec![],
            singletons: vec![],
        };

        for obj in vec![Object::Nil, Object::Bool(true), Object::Bool(false)] {
            let ptr = ObjectPtr::alloc(obj);
            vm.singletons.push(ptr);
        }

        vm
    }

    pub fn interpret(&mut self, chunk: Chunk, mut objects: Vec<ObjectPtr>) -> Result<(), Error> {
        self.chunk = chunk;
        self.index = 0;
        self.stack = vec![];
        self.objects.append(&mut objects);

        loop {
            let byte = self.read_byte();
            let code = match OpCode::try_from(byte) {
                Ok(code) => code,
                Err(_) => return Err(Error::new(
                    format!("Invalid byte {}.", byte),
                    self.chunk.get_line(self.index),
                )),
            };

            match code {
                OpCode::Return => {
                    let value = self.pop();
                    self.print_value(&*value);
                    return Ok(());
                },
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
            }
        }
    }

    pub fn free_objects(self) {
        self.objects.into_iter().for_each(ObjectPtr::dealloc);
        self.singletons.into_iter().for_each(ObjectPtr::dealloc);
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

impl VM {
    fn pop(&mut self) -> ObjectPtr {
        self.stack.pop().unwrap()
    }

    fn push(&mut self, value: ObjectPtr) {
        self.stack.push(value);
    }

    fn read_byte(&mut self) -> u8 {
        let value = self.chunk.code[self.index];
        self.index += 1;
        value
    }

    fn read_constant(&mut self) -> ObjectPtr {
        let index = self.read_byte() as usize;
        self.chunk.constants[index]
    }

    fn print_value(&self, object: &Object) {
        println!("{}", object);
    }

    fn get_nil(&self) -> ObjectPtr {
        self.singletons[0]
    }

    fn get_bool(&self, value: bool) -> ObjectPtr {
        if value {
            self.singletons[1]
        } else {
            self.singletons[2]
        }
    }

    fn alloc(&mut self, object: Object) -> ObjectPtr {
        let ptr = ObjectPtr::alloc(object);
        self.objects.push(ptr);
        ptr
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
