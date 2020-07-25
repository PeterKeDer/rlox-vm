use std::convert::TryFrom;
use std::collections::HashMap;

use crate::error::{Error, Result};
use crate::chunk::{Chunk, OpCode};
use crate::object::{Object, ObjectType, ObjectPtr, Function};
use crate::allocator::ObjectAllocator;

macro_rules! binary_op {
    ( $vm:expr, $op:tt, $ident_ty:ident, $res_ty:ident ) => {
        {
            let b = $vm.pop();
            let a = $vm.pop();

            match (&*a, &*b) {
                (Object::$ident_ty(a), Object::$ident_ty(b)) => Ok($vm.alloc_push(Object::$res_ty(*a $op *b))),
                _ => Err(Error::new(
                    format!(
                        "The operator '{}' can only be used on two objects of type {}.",
                        stringify!($op), stringify!($ident_ty),
                    ),
                    $vm.chunk().get_line($vm.index()),
                )),
            }
        }
    };
}

static MAX_FRAMES: usize = 255;

pub struct CallFrame {
    function: ObjectPtr,
    instruction_index: usize,
    slots_index: usize,
}

impl CallFrame {
    fn slot(&self, offset: usize) -> usize {
        self.slots_index + offset
    }
}

pub struct VM<Allocator: ObjectAllocator> {
    pub allocator: Allocator,
    stack: Vec<ObjectPtr>,
    singletons: Vec<ObjectPtr>,
    globals: HashMap<String, ObjectPtr>,
    frames: Vec<CallFrame>,
}

impl<Allocator> VM<Allocator>
where
    Allocator: ObjectAllocator,
{
    pub fn new(allocator: Allocator) -> VM<Allocator> {
        let mut vm = VM {
            allocator,
            stack: vec![],
            singletons: vec![],
            globals: HashMap::new(),
            frames: vec![],
        };

        for obj in vec![Object::Nil, Object::Bool(true), Object::Bool(false)] {
            let ptr = vm.alloc(obj);
            vm.singletons.push(ptr);
        }

        vm
    }

    pub fn interpret(&mut self, function: Function) -> Result<()> {
        self.frames.clear();
        self.stack.clear();

        let function = self.alloc(Object::Function(function));
        self.push(function.clone());

        // Create and push the call frame
        self.call(function, 0)?;

        loop {
            let byte = self.read_byte();

            let code = match OpCode::try_from(byte) {
                Ok(code) => code,
                Err(_) => return Err(Error::new(
                    format!("Invalid byte {}.", byte),
                    self.chunk().get_line(self.index()),
                )),
            };

            match code {
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
                            self.chunk().get_line(self.index()),
                        )),
                    }
                },
                OpCode::Not => {
                    let value = self.pop();
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
                            self.chunk().get_line(self.index()),
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
                            self.chunk().get_line(self.index() - 1),
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
                            self.chunk().get_line(self.index() - 1),
                        ));
                    }
                },
                OpCode::GetLocal => {
                    let slot = self.read_byte() as usize;
                    self.push(self.get(self.frame().slot(slot)).clone());
                },
                OpCode::SetLocal => {
                    let slot = self.read_byte() as usize;
                    // Peek is used here since assignments are also expressions
                    self.set(self.frame().slot(slot), self.peek(0).clone());
                },
                OpCode::Jump => {
                    let offset = self.read_short() as usize;
                    self.frame_mut().instruction_index += offset;
                },
                OpCode::JumpIfFalse => {
                    let offset = self.read_short() as usize;
                    if is_falsey(self.peek(0)) {
                        self.frame_mut().instruction_index += offset;
                    }
                },
                OpCode::Loop => {
                    let offset = self.read_short() as usize;
                    self.frame_mut().instruction_index -= offset;
                },
                OpCode::Call => {
                    let arg_count = self.read_byte();
                    self.call_value(self.peek(arg_count as usize).clone(), arg_count)?;
                },
                OpCode::Return => {
                    let result = self.pop();

                    let frame = self.frames.pop().unwrap();

                    // Pop arguments passed to the function and the function itself
                    while self.stack.len() > frame.slots_index {
                        self.pop();
                    }

                    if self.frames.is_empty() {
                        return Ok(());
                    }

                    self.push(result);
                },
            }
        }
    }

    pub fn print_stack_trace(&self) {
        for frame in self.frames.iter().rev() {
            let function = frame.function.unwrap_function();
            println!(
                "[line {}] in {}",
                function.chunk.get_line(frame.instruction_index),
                match &function.name {
                    Some(name) => name,
                    None => "script",
                },
            );
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

    fn call_value(&mut self, ptr: ObjectPtr, arg_count: u8) -> Result<()> {
        match &*ptr {
            Object::Function(_) => self.call(ptr, arg_count),
            _ => Err(Error::new(
                "Can only call functions and classes.".to_string(),
                self.chunk().get_line(self.index()),
            )),
        }
    }

    /// Call a function.
    /// Precondition: the `ObjectType` of `function` is `ObjectType::Function`
    fn call(&mut self, function: ObjectPtr, arg_count: u8) -> Result<()> {
        let arity = function.unwrap_function().arity;
        if arity != arg_count {
            return Err(Error::new(
                format!("Function {} expects {} arguments but got {}.", *function, arity, arg_count),
                self.chunk().get_line(self.index()),
            ));
        }

        if self.frames.len() == MAX_FRAMES {
            return Err(Error::new(
                format!("Stack overflow."),
                self.chunk().get_line(self.index()),
            ));
        }

        // Push new frame
        let frame = CallFrame {
            function,
            instruction_index: 0,
            slots_index: self.stack.len() - (arg_count as usize) - 1,
        };
        self.frames.push(frame);

        Ok(())
    }
}

impl<Allocator> VM<Allocator>
where
    Allocator: ObjectAllocator,
{
    fn frame(&self) -> &CallFrame {
        &self.frames[self.frames.len() - 1]
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        let index = self.frames.len() - 1;
        &mut self.frames[index]
    }

    fn chunk(&self) -> &Chunk {
        &self.frame().function.unwrap_function().chunk
    }

    fn index(&self) -> usize {
        self.frame().instruction_index
    }

    fn pop(&mut self) -> ObjectPtr {
        self.stack.pop().unwrap()
    }

    fn push(&mut self, value: ObjectPtr) {
        self.stack.push(value);
    }

    fn get(&self, index: usize) -> &ObjectPtr {
        &self.stack[index]
    }

    fn set(&mut self, index: usize, item: ObjectPtr) {
        self.stack[index] = item;
    }

    fn peek(&self, n: usize) -> &ObjectPtr {
        &self.stack[self.stack.len() - 1 - n]
    }

    fn read_byte(&mut self) -> u8 {
        let value = self.chunk().code[self.index()];
        self.frame_mut().instruction_index += 1;
        value
    }

    fn read_short(&mut self) -> u16 {
        let value = ((self.chunk().code[self.index()] as u16) << 8) | self.chunk().code[self.index() + 1] as u16;
        self.frame_mut().instruction_index += 2;
        value
    }

    fn read_constant(&mut self) -> ObjectPtr {
        let byte = self.read_byte();
        self.chunk().constants[byte as usize].clone()
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
