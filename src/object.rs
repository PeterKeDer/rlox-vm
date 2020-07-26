use std::fmt;
use std::ops::{Deref, DerefMut};

use crate::error::Result;
use crate::allocator::ObjectAllocator;

macro_rules! generate_enum {
    ( $enum_name:ident | $enum_type_name:ident, $( derive($( $trt:ident ),*) )? { $( $name:ident $(($ty:ty))?, )* } ) => {
        $( #[derive($( $trt ),*)] )?
        pub enum $enum_name {
            $(
                $name $(($ty))?,
            )*
        }

        impl $enum_name {
            paste::item! {
                pub fn get_type(&self) -> $enum_type_name {
                    match self {
                        $(
                            $enum_name::$name $(([<_ $ty:lower>]))* => $enum_type_name::$name,
                        )*
                    }
                }

                $(
                    #[allow(unused_parens)]
                    pub fn [<unwrap_ $name:snake>](&self) -> &($($ty)?) {
                        if let $enum_name::$name $(([<value_ $ty:snake>]))* = self {
                            &($([<value_ $ty:snake>])*)
                        } else {
                            panic!("Tried to unwrap {} from type {:?}.", stringify!($name), self.get_type());
                        }
                    }

                    #[allow(unused_parens)]
                    pub fn [<take_ $name:snake>](self) -> ($($ty)?) {
                        if let $enum_name::$name $(([<value_ $ty:snake>]))* = self {
                            $([<value_ $ty:snake>])*
                        } else {
                            panic!("Tried to unwrap {} from type {:?}.", stringify!($name), self.get_type());
                        }
                    }

                    #[allow(unused_parens)]
                    pub fn [<as_ $name:snake>](&self) -> Option<&($($ty)?)> {
                        if let $enum_name::$name $(([<value_ $ty:snake>]))* = self {
                            Some(&($([<value_ $ty:snake>])*))
                        } else {
                            None
                        }
                    }
                )*
            }
        }

        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub enum $enum_type_name {
            $(
                $name,
            )*
        }
    };
}

generate_enum!(Value | ValueType, derive(Clone) {
    Nil,
    Bool(bool),
    Number(f64),
    Object(ObjectPtr),
});

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(value) => write!(f, "{}", value),
            Value::Number(value) => write!(f, "{}", value),
            Value::Object(obj) => write!(f, "{}", **obj),
        }
    }
}

// This generates the enums `Object` and `ObjectType`.
// Implements methods `get_type`, `unwrap_<variant>`, `take_<variant>`, and `as_<variant>` for `Object`.
generate_enum!(Object | ObjectType, {
    String(String),
    Function(Function),
    Native(NativeFn),
});

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::String(value) => write!(f, "\"{}\"", value),
            Object::Function(function) => write!(f, "{}", function),
            Object::Native(_) => write!(f, "<native  fn>"),
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
    pub chunk_index: usize,
}

impl Function {
    pub fn new(name: Option<String>, arity: u8, chunk_index: usize) -> Function {
        Function {
            name,
            arity,
            chunk_index,
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.name {
            Some(name) => write!(f, "<fn {}>", name),
            None => write!(f, "<script>"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FunctionType {
    Function,
    Script,
}

pub type NativeFn = Box<dyn Fn(&mut dyn ObjectAllocator, usize, Vec<Value>) -> Result<Value>>;
