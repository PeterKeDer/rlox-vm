use crate::object::{Function, FunctionType};

pub struct Local<'s> {
    pub name: &'s str,
    // When depth is None, the local is uninitialized
    pub depth: Option<usize>,
}

impl<'s> Local<'s> {
    pub fn new(name: &'s str, depth: Option<usize>) -> Local<'s> {
        Local {
            name,
            depth,
        }
    }

    fn placeholder() -> Local<'static> {
        Local {
            name: "",
            depth: Some(0),
        }
    }
}

pub struct Compiler<'src> {
    pub enclosing: Option<Box<Compiler<'src>>>,
    pub locals: Vec<Local<'src>>,
    pub scope_depth: usize,
    pub function: Function,
    pub function_type: FunctionType,
}

impl<'src> Compiler<'src> {
    pub fn new(
        enclosing: Option<Box<Compiler<'src>>>,
        function_type: FunctionType,
        function_name: Option<String>,
        chunk_index: usize,
    ) -> Compiler<'src> {
        // The placeholder is reserved for the VM to put the function being executed
        let locals = vec![Local::placeholder()];

        Compiler {
            enclosing,
            locals,
            scope_depth: 0,
            function: Function::new(function_name, 0, chunk_index),
            function_type,
        }
    }
}
