use crate::object::{Function, FunctionType};
use crate::error::{Error, Result};
use crate::scanner::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct Upvalue {
    pub index: usize,
    pub is_local: bool,
}

pub struct Local<'s> {
    pub name: &'s str,
    // When depth is None, the local is uninitialized
    pub depth: Option<usize>,
    pub is_captured: bool,
}

impl<'s> Local<'s> {
    pub fn new(name: &'s str, depth: Option<usize>) -> Local<'s> {
        Local {
            name,
            depth,
            is_captured: false,
        }
    }

    fn placeholder() -> Local<'static> {
        Local {
            name: "",
            depth: Some(0),
            is_captured: false,
        }
    }
}

pub struct Compiler<'src> {
    pub enclosing: Option<Box<Compiler<'src>>>,
    pub locals: Vec<Local<'src>>,
    pub scope_depth: usize,
    pub function: Function,
    pub function_type: FunctionType,
    pub upvalues: Vec<Upvalue>,
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
            upvalues: vec![],
        }
    }

    pub fn add_local(&mut self, name: Token<'src>) -> Result<()> {
        // There are a max number of local variables (for now) since bytecode are u8
        if self.locals.len() > u8::MAX as usize {
            return Err(Error::new(
                "Too many local variables.".to_string(),
                name.line,
            ));
        }

        let local = Local::new(name.lexeme, None);
        self.locals.push(local);
        Ok(())
    }

    /// Return whether there exists a local with given name, otherwise None.
    pub fn resolve_local(&self, name: &Token<'_>) -> Result<Option<usize>> {
        for (index, local) in self.locals.iter().enumerate().rev() {
            if name.lexeme == local.name {
                if local.depth.is_none() {
                    return Err(Error::new(
                        format!("Cannot use variable '{}' in its own initializer.", name.lexeme),
                        name.line,
                    ));
                } else {
                    return Ok(Some(index));
                }
            }
        }

        Ok(None)
    }

    pub fn add_upvalue(&mut self, index: usize, is_local: bool, line: usize) -> Result<usize> {
        if let Some(i) = self.upvalues.iter().position(|x| x.index == index && x.is_local == is_local) {
            // Return the previous index if a same upvalue exists
            Ok(i)

        } else if self.upvalues.len() >= u8::MAX as usize {
            // Error: more than u8 max amount of upvalues
            Err(Error::new(
                "Too many closure variables in function.".to_string(),
                line,
            ))

        } else {
            // Push the new one
            self.upvalues.push(Upvalue {
                index,
                is_local,
            });
            self.function.upvalue_count += 1;

            Ok(self.upvalues.len() - 1)
        }
    }

    /// Return whether there exists an upvalue with given name, otherwise None.
    /// An upvalue is a local variable that is not in the current compiler (aka up the scope).
    pub fn resolve_upvalue(&mut self, name: &Token<'_>) -> Result<Option<usize>> {
        let result = if let Some(enclosing) = &mut self.enclosing {
            // First try to find if enclosing has a local with name
            // Otherwise, try to find an upvalue instead
            // If none exist, then cannot resolve the upvalue
            if let Some(local) = enclosing.resolve_local(name)? {
                enclosing.locals[local].is_captured = true;
                Some(self.add_upvalue(local, true, name.line)?)

            } else if let Some(upvalue) = enclosing.resolve_upvalue(name)? {
                Some(self.add_upvalue(upvalue, false, name.line)?)

            } else {
                None
            }
        } else {
            None
        };

        Ok(result)
    }
}
