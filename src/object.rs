use std::fmt;
use std::ops::{Deref, DerefMut};

use crate::chunk::Chunk;

macro_rules! generate_object_enum {
    ( $( $name:ident $(($ty:ty))?, )* ) => {

        pub enum Object {
            $(
                $name $(($ty))?,
            )*
        }

        impl Object {
            paste::item! {
                pub fn get_type(&self) -> ObjectType {
                    match self {
                        $(
                            Object::$name $(([<_ $ty:lower>]))* => ObjectType::$name,
                        )*
                    }
                }

                $(
                    #[allow(unused_parens)]
                    pub fn [<unwrap_ $name:lower>](&self) -> &($($ty)?) {
                        if let Object::$name $(([<value_ $ty:snake>]))* = self {
                            &($([<value_ $ty:lower>])*)
                        } else {
                            panic!("Tried to unwrap {} from type {:?}.", stringify!($name), self.get_type());
                        }
                    }

                    #[allow(unused_parens)]
                    pub fn [<take_ $name:lower>](self) -> ($($ty)?) {
                        if let Object::$name $(([<value_ $ty:snake>]))* = self {
                            $([<value_ $ty:lower>])*
                        } else {
                            panic!("Tried to unwrap {} from type {:?}.", stringify!($name), self.get_type());
                        }
                    }

                    #[allow(unused_parens)]
                    pub fn [<as_ $name:lower>](&self) -> Option<&($($ty)?)> {
                        if let Object::$name $(([<value_ $ty:snake>]))* = self {
                            Some(&($([<value_ $ty:lower>])*))
                        } else {
                            None
                        }
                    }
                )*
            }
        }

        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub enum ObjectType {
            $(
                $name,
            )*
        }
    };
}

// This generates the enums `Object` and `ObjectType`.
// Implements methods `get_type`, `unwrap_<variant>`, `take_<variant>`, and `as_<variant>` for `Object`.
generate_object_enum! {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Function(Function),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Nil => write!(f, "nil"),
            Object::Bool(value) => write!(f, "{}", value),
            Object::Number(value) => write!(f, "{}", value),
            Object::String(value) => write!(f, "\"{}\"", value),
            Object::Function(function) => match &function.name {
                Some(name) => write!(f, "<fn {}>", name),
                None => write!(f, "<script>"),
            }
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct ObjectPtr {
    ptr: *mut Object,
}

impl ObjectPtr {
    pub fn alloc(object: Object) -> ObjectPtr {
        ObjectPtr {
            ptr: Box::into_raw(Box::new(object)),
        }
    }

    pub fn dealloc(ptr: ObjectPtr) {
        unsafe {
            Box::from_raw(ptr.ptr);
        }
    }
}

impl fmt::Debug for ObjectPtr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ObjectPtr {{ ptr: {:?} -> {} }}", self.ptr, self.deref())
    }
}

impl Deref for ObjectPtr {
    type Target = Object;

    fn deref(&self) -> &Object {
        unsafe {
            &*self.ptr
        }
    }
}

impl DerefMut for ObjectPtr {
    fn deref_mut(&mut self) -> &mut Object {
        unsafe {
            &mut *self.ptr
        }
    }
}

pub struct Function {
    pub name: Option<String>,
    pub arity: u8,
    pub chunk: Chunk,
}

impl Function {
    pub fn new(name: Option<String>, arity: u8, chunk: Chunk) -> Function {
        Function {
            name,
            arity,
            chunk,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FunctionType {
    Function,
    Script,
}
