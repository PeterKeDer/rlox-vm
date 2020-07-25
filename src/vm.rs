use std::convert::TryFrom;
use std::collections::HashMap;

use crate::error::{Error, Result};
use crate::chunk::{Chunk, OpCode};
use crate::object::{Value, ValueType, Object, ObjectType, ObjectPtr, Function, NativeFn};
use crate::allocator::ObjectAllocator;

macro_rules! binary_op {
    ( $vm:expr, $op:tt, $ident_ty:ident, $res_ty:ident ) => {
        {
            let b = $vm.pop();
            let a = $vm.pop();

            match (a, b) {
                (Value::$ident_ty(a), Value::$ident_ty(b)) => Ok($vm.push(Value::$res_ty(a $op b))),
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
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
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
            globals: HashMap::new(),
            frames: vec![],
        };

        vm.define_native("clock".to_string(), Box::new(clock_native));

        vm
    }

    pub fn interpret(&mut self, function: Function) -> Result<()> {
        self.frames.clear();
        self.stack.clear();

        let function = self.alloc(Object::Function(function));
        self.push(function.clone());

        // Create and push the call frame
        self.call_value(function, 0)?;

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
                OpCode::True => self.push(Value::Bool(true)),
                OpCode::False => self.push(Value::Bool(false)),
                OpCode::Nil => self.push(Value::Nil),
                OpCode::Negate => {
                    match self.pop().as_number() {
                        Some(n) => self.push(Value::Number(-n)),
                        _ => return Err(Error::new(
                            "The '-' unary operator can only be used on numbers.".to_string(),
                            self.chunk().get_line(self.index()),
                        )),
                    }
                },
                OpCode::Not => {
                    let value = self.pop();
                    self.push(Value::Bool(is_falsey(&value)));
                },
                OpCode::And => binary_op!(self, &&, Bool, Bool)?,
                OpCode::Or => binary_op!(self, ||, Bool, Bool)?,
                OpCode::Add => {
                    match (self.peek(1).get_type(), self.peek(0).get_type()) {
                        (ValueType::Number, ValueType::Number) => {
                            binary_op!(self, +, Number, Number)?;
                            continue;
                        },
                        (ValueType::Object, ValueType::Object) => {
                            match (self.peek(1).unwrap_object().get_type(), self.peek(0).unwrap_object().get_type()) {
                                (ObjectType::String, ObjectType::String) => {
                                    let b = self.pop();
                                    let a = self.pop();
                                    let string = format!("{}{}", a.unwrap_object().unwrap_string(), b.unwrap_object().unwrap_string());
                                    self.alloc_push(Object::String(string));
                                    continue;
                                },
                                _ => (),
                            }
                        }
                        _ => (),
                    }

                    return Err(Error::new(
                        "The '+' operator can only be used by two Strings or two Numbers.".to_string(),
                        self.chunk().get_line(self.index()),
                    ));
                },
                OpCode::Subtract => binary_op!(self, -, Number, Number)?,
                OpCode::Multiply => binary_op!(self, *, Number, Number)?,
                OpCode::Divide => binary_op!(self, /, Number, Number)?,
                OpCode::Equal => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::Bool(self.values_equal(&a, &b)));
                },
                OpCode::Less => binary_op!(self, <, Number, Bool)?,
                OpCode::Greater => binary_op!(self, >, Number, Bool)?,
                OpCode::Print => {
                    let value = self.pop();
                    self.print_value(&value);
                },
                OpCode::Pop => {
                    self.pop();
                },
                OpCode::DefineGlobal => {
                    let name = self.read_constant().unwrap_object().unwrap_string().clone();
                    let value = self.pop();

                    self.globals.insert(name, value);
                },
                OpCode::GetGlobal => {
                    let name_constant = self.read_constant();
                    let name = name_constant.unwrap_object().unwrap_string();

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
                    let name = self.read_constant().unwrap_object().unwrap_string().clone();
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
                    let arg_count = self.read_byte() as usize;
                    self.call_value(self.peek(arg_count).clone(), arg_count)?;
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

    fn define_native(&mut self, name: String, function: NativeFn) {
        let ptr = self.alloc(Object::Native(function));
        self.globals.insert(name, ptr);
    }

    fn values_equal(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Object(a), Value::Object(b)) => match (&**a, &**b) {
                (Object::String(a), Object::String(b)) => a == b,
                _ => false,
            },
            _ => false,
        }
    }

    fn call_value(&mut self, value: Value, arg_count: usize) -> Result<()> {
        match value {
            Value::Object(ptr) => match &*ptr {
                Object::Function(_) => return self.call(ptr, arg_count),
                Object::Native(native) => {
                    // Pop arguments and add them to vector
                    let mut args = vec![];
                    for _ in 0..arg_count {
                        args.insert(0, self.pop());
                    }

                    // Pop native function
                    self.pop();

                    let result = native(&mut self.allocator, arg_count, args)?;
                    self.push(result);

                    return Ok(());
                },
                _ => (),
            },
            _ => (),
        }

        Err(Error::new(
            "Can only call functions and classes.".to_string(),
            self.chunk().get_line(self.index()),
        ))
    }

    /// Call a function.
    /// Precondition: the `ObjectType` of `function` is `ObjectType::Function`
    fn call(&mut self, function: ObjectPtr, arg_count: usize) -> Result<()> {
        let arity = function.unwrap_function().arity;
        if arity as usize != arg_count {
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
    #[inline(never)]
    fn frame(&self) -> &CallFrame {
        &self.frames[self.frames.len() - 1]
    }

    #[inline(never)]
    fn frame_mut(&mut self) -> &mut CallFrame {
        let index = self.frames.len() - 1;
        &mut self.frames[index]
    }

    #[inline(never)]
    fn chunk(&self) -> &Chunk {
        &self.frame().function.unwrap_function().chunk
    }

    #[inline(never)]
    fn index(&self) -> usize {
        self.frame().instruction_index
    }

    #[inline(never)]
    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    #[inline(never)]
    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    #[inline(never)]
    fn get(&self, index: usize) -> &Value {
        &self.stack[index]
    }

    #[inline(never)]
    fn set(&mut self, index: usize, item: Value) {
        self.stack[index] = item;
    }

    #[inline(never)]
    fn peek(&self, n: usize) -> &Value {
        &self.stack[self.stack.len() - 1 - n]
    }

    #[inline(never)]
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

    fn read_constant(&mut self) -> Value {
        let byte = self.read_byte();
        self.chunk().constants[byte as usize].clone()
    }

    fn print_value(&self, value: &Value) {
        println!("{}", value);
    }

    #[inline(never)]
    fn alloc(&mut self, object: Object) -> Value {
        Value::Object(self.allocator.allocate(object))
    }

    fn alloc_push(&mut self, object: Object) {
        let ptr = self.alloc(object);
        self.push(ptr);
    }
}

fn is_falsey(value: &Value) -> bool {
    match value {
        Value::Bool(false) | Value::Nil => true,
        _ => false,
    }
}

fn clock_native(_allocator: &mut dyn ObjectAllocator, _arg_count: usize, _args: Vec<Value>) -> Result<Value> {
    use std::time::{SystemTime, UNIX_EPOCH};

    let ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();

    Ok(Value::Number(ms as f64))
}
