use std::mem;
use std::convert::TryFrom;
use std::cell::RefCell;
use std::collections::HashMap;

use intrusive_collections::{LinkedList, LinkedListLink, intrusive_adapter};

use crate::error::{Error, Result};
use crate::chunk::{Chunk, Chunks, OpCode};
use crate::object::{Value, Object, Function, NativeFn, Closure, ObjectPtr, Upvalue};
use crate::allocator::ObjectAllocator;

macro_rules! binary_op {
    ( $vm:expr, $op:tt, $ident_ty:ident, $res_ty:ident ) => {
        {
            let b = $vm.pop();
            let a = $vm.pop();

            match (a, b) {
                (Value::$ident_ty(a), Value::$ident_ty(b)) => Ok($vm.push(Value::$res_ty(a $op b))),
                _ => $vm.error(
                    format!(
                        "The operator '{}' can only be used on two objects of type {}.",
                        stringify!($op), stringify!($ident_ty),
                    ),
                ),
            }
        }
    };
}

static MAX_FRAMES: usize = 255;

struct CallFrame<'c> {
    chunk: &'c Chunk,
    instruction_index: usize,
    slots_index: usize,
}

impl CallFrame<'_> {
    fn slot(&self, offset: usize) -> usize {
        self.slots_index + offset
    }
}

pub trait VMState {}

pub struct VMInitializedState;
impl VMState for VMInitializedState {}

#[derive(Debug)]
struct ObjectPtrElement {
    link: LinkedListLink,
    value: ObjectPtr,
}

impl ObjectPtrElement {
    fn new(value: ObjectPtr) -> ObjectPtrElement {
        ObjectPtrElement {
            link: LinkedListLink::new(),
            value,
        }
    }
}

intrusive_adapter!(ObjectPtrAdapter = Box<ObjectPtrElement>: ObjectPtrElement { link: LinkedListLink });

pub struct VMRunningState<'a> {
    chunks: &'a Chunks,
    stack: Vec<Value>,
    frames: Vec<CallFrame<'a>>,
    open_upvalues: LinkedList<ObjectPtrAdapter>,
}
impl<'a> VMState for VMRunningState<'a> {}

pub struct VM<Allocator, State>
where
    Allocator: ObjectAllocator,
    State: VMState,
{
    pub allocator: Allocator,
    globals: HashMap<String, Value>,
    state: State,
}

// Functions for initialized VM

impl<Allocator> VM<Allocator, VMInitializedState>
where
    Allocator: ObjectAllocator,
{
    pub fn new(allocator: Allocator) -> VM<Allocator, VMInitializedState> {
        let mut vm = VM {
            allocator,
            globals: HashMap::new(),
            state: VMInitializedState,
        };

        vm.define_native("clock".to_string(), Box::new(clock_native));

        vm
    }

    pub fn interpret(self,
        function: Function,
        chunks: &'_ Chunks,
        print_stack_trace: bool,
    ) -> (VM<Allocator, VMInitializedState>, Result<()>)
    {
        let mut vm = VM {
            allocator: self.allocator,
            globals: self.globals,
            state: VMRunningState {
                chunks,
                stack: vec![],
                frames: vec![],
                open_upvalues: LinkedList::new(ObjectPtrAdapter::new()),
            },
        };

        let result = vm.run(function);

        if result.is_err() && print_stack_trace {
            vm.print_stack_trace();
        }

        let prev_vm = VM {
            allocator: vm.allocator,
            globals: vm.globals,
            state: VMInitializedState,
        };

        (prev_vm, result)
    }
}

// Functions only when VM is running

impl<'a, Allocator> VM<Allocator, VMRunningState<'a>>
where
    Allocator: ObjectAllocator,
{
    pub fn print_stack_trace(&self) {
        println!("=== Stack Trace ===");
        println!("Printing stack trace, from last called:");

        for frame in self.state.frames.iter().rev() {
            // Can either be a closure or a function
            let name = match &**self.get(frame.slot(0)).unwrap_object() {
                Object::Function(function) => &function.name,
                Object::Closure(closure) => &closure.function.unwrap_function().name,
                x => panic!("Expect Function or Closure type in callee spot, got type {:?}", x.get_type()),
            };

            println!(
                "   [line {}] in {}",
                frame.chunk.get_line(frame.instruction_index),
                match name {
                    Some(name) => name,
                    None => "script",
                },
            );
        }
    }

    fn run(&mut self, function: Function) -> Result<()> {
        let function = self.alloc(Object::Function(function));
        self.push(function.clone());

        // Create and push the call frame
        self.call_value(function, 0)?;

        loop {
            let byte = self.read_byte();

            let code = match OpCode::try_from(byte) {
                Ok(code) => code,
                Err(_) => return self.error(format!("Invalid byte {}.", byte)),
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
                        _ => return self.error("The '-' unary operator can only be used on numbers.".to_string()),
                    }
                },
                OpCode::Not => {
                    let value = self.pop();
                    self.push(Value::Bool(self.is_falsey(&value)));
                },
                OpCode::And => binary_op!(self, &&, Bool, Bool)?,
                OpCode::Or => binary_op!(self, ||, Bool, Bool)?,
                OpCode::Add => {
                    let b = self.pop();
                    let a = self.pop();

                    match (a, b) {
                        (Value::Number(a), Value::Number(b)) => {
                            self.push(Value::Number(a + b));
                            continue;
                        },
                        (Value::Object(a), Value::Object(b)) => match (&*a, &*b) {
                            (Object::String(a), Object::String(b)) => {
                                let string = format!("{}{}", a, b);
                                self.alloc_push(Object::String(string));
                                continue;
                            },
                            _ => (),
                        },
                        _ => (),
                    }

                    return self.error("The '+' operator can only be used by two Strings or two Numbers.".to_string());
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
                        None => return self.error(format!("Undefined variable '{}'.", name)),
                    };

                    self.push(value);
                },
                OpCode::SetGlobal => {
                    let name = self.read_constant().unwrap_object().unwrap_string().clone();
                    let value = self.peek(0).clone();

                    if self.globals.contains_key(&name) {
                        self.globals.insert(name, value);
                    } else {
                        return self.error(format!("Undefined variable '{}'.", name));
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
                OpCode::GetUpvalue => {
                    let slot = self.read_byte() as usize;

                    let upvalue = self.closure().upvalues[slot].unwrap_upvalue().borrow();

                    let value = match &*upvalue {
                        Upvalue::Open(index) => self.get(*index).clone(),
                        Upvalue::Closed(value) => value.clone(),
                    };

                    mem::drop(upvalue);
                    self.push(value);
                },
                OpCode::SetUpvalue => {
                    let slot = self.read_byte() as usize;
                    let value = self.peek(0).clone();

                    let mut upvalue = self.closure().upvalues[slot].unwrap_upvalue().borrow_mut();

                    match &*upvalue {
                        Upvalue::Open(index) => {
                            let index = *index;
                            mem::drop(upvalue);
                            self.set(index, value);
                        },
                        Upvalue::Closed(_) => {
                            *upvalue = Upvalue::Closed(value);
                        },
                    }
                },
                OpCode::Jump => {
                    let offset = self.read_short() as usize;
                    self.frame_mut().instruction_index += offset;
                },
                OpCode::JumpIfFalse => {
                    let offset = self.read_short() as usize;
                    if self.is_falsey(self.peek(0)) {
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
                    let frame = self.state.frames.pop().unwrap();

                    // Close all open upvalues whose values will be poppped from the stack
                    self.close_upvalues(frame.slots_index);

                    // Pop arguments passed to the function and the function itself
                    self.state.stack.truncate(frame.slots_index);

                    if self.state.frames.is_empty() {
                        return Ok(());
                    }

                    self.push(result);
                },
                OpCode::Closure => {
                    let ptr = self.read_constant().unwrap_object().clone();
                    let function = ptr.unwrap_function();

                    let upvalues = (0..function.upvalue_count)
                        .map(|_| {
                            let is_local = self.read_byte() == 1;
                            let index = self.read_byte();

                            if is_local {
                                self.capture_upvalue(self.frame().slot(index as usize))
                            } else {
                                self.closure().upvalues[index as usize].clone()
                            }
                        })
                        .collect();

                    let closure = Closure::new(ptr, upvalues);
                    self.alloc_push(Object::Closure(closure));
                },
                OpCode::CloseUpvalue => {
                    self.close_upvalues(self.state.stack.len() - 1);
                    self.pop();
                },
            }
        }
    }

    /// Capture the value at `index` on the stack as an open upvalue.
    fn capture_upvalue(&mut self, index: usize) -> ObjectPtr {
        let mut cursor = self.state.open_upvalues.front_mut();

        while let Some(next) = cursor.get() {
            match *next.value.unwrap_upvalue().borrow() {
                Upvalue::Open(location) => {
                    if location == index {
                        // Found the upvalue
                        return next.value.clone();

                    } else if location < index {
                        // Went past the required stack location, so create new upvalue
                        break;
                    }
                },
                Upvalue::Closed(_) => panic!("Closed upvalue found in 'open_upvalues'."),
            }
            cursor.move_next();
        }

        let upvalue = self.allocator.allocate(Object::Upvalue(RefCell::new(Upvalue::Open(index))));

        cursor.insert_before(Box::new(ObjectPtrElement::new(upvalue.clone())));

        upvalue
    }

    /// Pop and close all open upvalues whose `location` is less than or equal to `index`.
    fn close_upvalues(&mut self, index: usize) {
        let mut cursor = self.state.open_upvalues.front_mut();

        while let Some(next) = cursor.get() {
            let location = match *next.value.unwrap_upvalue().borrow() {
                Upvalue::Open(location) => {
                    if location < index {
                        // Went past index, stop popping
                        break;
                    }

                    location
                },
                Upvalue::Closed(_) => panic!("Closed upvalue found in 'open_upvalues'."),
            };

            let value = self.state.stack[location].clone();

            // Close the upvalue by setting object pointed
            *cursor.remove().unwrap().value.unwrap_upvalue().borrow_mut() = Upvalue::Closed(value);
        }
    }

    fn call_value(&mut self, value: Value, arg_count: usize) -> Result<()> {
        match value {
            Value::Object(ptr) => match &*ptr {
                Object::Function(function) => return self.call_function(function, arg_count),
                Object::Closure(closure) => return self.call_function(closure.function.unwrap_function(), arg_count),
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

        self.error("Can only call functions and classes.".to_string())
    }

    /// Call a function by pushing a new frame.
    fn call_function(&mut self, function: &Function, arg_count: usize) -> Result<()> {
        if function.arity as usize != arg_count {
            return self.error(format!("Function {} expects {} arguments but got {}.", function, function.arity, arg_count));
        } else if self.state.frames.len() == MAX_FRAMES {
            return self.error(format!("Stack overflow."));
        }

        // Push new frame
        self.push_frame(
            self.state.chunks.get_chunk(function.chunk_index),
            self.state.stack.len() - arg_count - 1,
        );

        Ok(())
    }

    fn push_frame(&mut self, chunk: &'a Chunk, slots_index: usize) {
        let frame = CallFrame {
            chunk,
            instruction_index: 0,
            slots_index,
        };
        self.state.frames.push(frame);
    }

    // Utility Functions

    fn frame(&self) -> &CallFrame<'a> {
        &self.state.frames[self.state.frames.len() - 1]
    }

    fn frame_mut(&mut self) -> &mut CallFrame<'a> {
        let index = self.state.frames.len() - 1;
        &mut self.state.frames[index]
    }

    fn chunk(&self) -> &'a Chunk {
        &self.frame().chunk
    }

    fn closure(&self) -> &Closure {
        self.state.stack[self.frame().slots_index].unwrap_object().unwrap_closure()
    }

    fn index(&self) -> usize {
        self.frame().instruction_index
    }

    fn pop(&mut self) -> Value {
        self.state.stack.pop().unwrap()
    }

    fn push(&mut self, value: Value) {
        self.state.stack.push(value);
    }

    fn get(&self, index: usize) -> &Value {
        &self.state.stack[index]
    }

    fn set(&mut self, index: usize, item: Value) {
        self.state.stack[index] = item;
    }

    fn peek(&self, n: usize) -> &Value {
        &self.state.stack[self.state.stack.len() - 1 - n]
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

    fn read_constant(&mut self) -> Value {
        let byte = self.read_byte();
        self.chunk().constants[byte as usize].clone()
    }

    fn alloc_push(&mut self, object: Object) {
        let ptr = self.alloc(object);
        self.push(ptr);
    }

    fn error(&self, message: String) -> Result<()> {
        Err(Error::new(message, self.chunk().get_line(self.index())))
    }
}

// General functions that work on all states

impl<Allocator, State> VM<Allocator, State>
where
    Allocator: ObjectAllocator,
    State: VMState,
{
    pub fn free_objects(self) {
        self.allocator.destroy();
    }

    fn define_native(&mut self, name: String, function: NativeFn) {
        let ptr = self.alloc(Object::Native(function));
        self.globals.insert(name, ptr);
    }

    fn print_value(&self, value: &Value) {
        println!("{}", value);
    }

    fn alloc(&mut self, object: Object) -> Value {
        Value::Object(self.alloc_obj(object))
    }

    fn alloc_obj(&mut self, object: Object) -> ObjectPtr {
        self.allocator.allocate(object)
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

    fn is_falsey(&self, value: &Value) -> bool {
        match value {
            Value::Bool(false) | Value::Nil => true,
            _ => false,
        }
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
