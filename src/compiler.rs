use crate::scanner::Token;
use crate::chunk::Chunk;
use crate::object::{Function, FunctionType};

pub struct Local<'t> {
    pub name: Token<'t>,
    // When depth is None, the local is uninitialized
    pub depth: Option<usize>,
}

impl<'t> Local<'t> {
    pub fn new(name: Token<'t>, depth: Option<usize>) -> Local<'t> {
        Local {
            name,
            depth,
        }
    }
}

pub struct Compiler<'src> {
    pub locals: Vec<Local<'src>>,
    pub scope_depth: usize,
    pub function: Function,
    pub function_type: FunctionType,
}

impl<'src> Compiler<'src> {
    pub fn new(function_type: FunctionType) -> Compiler<'src> {
        Compiler {
            locals: vec![],
            scope_depth: 0,
            function: Function::new(None, 0, Chunk::new()),
            function_type,
        }
    }

    pub fn emit_byte(&mut self, byte: u8, line: usize) {
        self.chunk().write(byte, line);
    }

    pub fn chunk(&mut self) -> &mut Chunk {
        &mut self.function.chunk
    }
}
