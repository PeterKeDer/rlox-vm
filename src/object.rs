use std::fmt;
use std::ops::{Deref, DerefMut};
use std::cell::RefCell;
use std::collections::HashMap;

use intrusive_collections::{LinkedList, LinkedListLink, intrusive_adapter};

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

generate_enum!(Value | ValueType, derive(Debug, Clone, PartialEq) {
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

type RefCellUpvalue = RefCell<Upvalue>;
type RefCellInstance = RefCell<Instance>;

// This generates the enums `Object` and `ObjectType`.
// Implements methods `get_type`, `unwrap_<variant>`, `take_<variant>`, and `as_<variant>` for `Object`.
generate_enum!(Object | ObjectType, {
    String(String),
    Function(Function),
    Native(NativeFn),
    Closure(Closure),
    Upvalue(RefCellUpvalue),
    Class(Class),
    Instance(RefCellInstance),
});

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::String(value) => write!(f, "\"{}\"", value),
            Object::Function(function) => write!(f, "{}", function),
            Object::Native(_) => write!(f, "<native fn>"),
            Object::Closure(closure) => write!(f, "{}", closure),
            Object::Upvalue(upvalue) => write!(f, "Upvalue {:?}", upvalue.borrow()),
            Object::Class(class) => write!(f, "{}", class.name),
            Object::Instance(instance) => write!(f, "{} instance", instance.borrow().class.unwrap_class().name),
        }
    }
}

pub struct ObjectWrapper {
    object: Object,
    pub is_marked: bool,
}

impl ObjectWrapper {
    fn new(object: Object) -> ObjectWrapper {
        ObjectWrapper {
            object,
            is_marked: false,
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct ObjectPtr {
    ptr: *mut ObjectWrapper,
}

impl ObjectPtr {
    pub fn alloc(object: Object) -> ObjectPtr {
        ObjectPtr {
            ptr: Box::into_raw(Box::new(ObjectWrapper::new(object))),
        }
    }

    pub fn dealloc(ptr: ObjectPtr) {
        unsafe {
            Box::from_raw(ptr.ptr);
        }
    }

    pub fn deref_wrapper(&self) -> &ObjectWrapper {
        unsafe {
            &*self.ptr
        }
    }

    pub fn deref_wrapper_mut(&mut self) -> &mut ObjectWrapper {
        unsafe {
            &mut *self.ptr
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
            &(*self.ptr).object
        }
    }
}

impl DerefMut for ObjectPtr {
    fn deref_mut(&mut self) -> &mut Object {
        unsafe {
            &mut (*self.ptr).object
        }
    }
}

#[derive(Debug)]
pub struct ObjectPtrElement {
    pub link: LinkedListLink,
    pub value: ObjectPtr,
}

impl ObjectPtrElement {
    pub fn new(value: ObjectPtr) -> ObjectPtrElement {
        ObjectPtrElement {
            link: LinkedListLink::new(),
            value,
        }
    }
}

intrusive_adapter!(pub ObjectPtrAdapter = Box<ObjectPtrElement>: ObjectPtrElement { link: LinkedListLink });

pub type ObjectPtrLinkedList = LinkedList<ObjectPtrAdapter>;

pub struct Function {
    pub name: Option<String>,
    pub arity: u8,
    pub chunk_index: usize,
    pub upvalue_count: usize,
}

impl Function {
    pub fn new(name: Option<String>, arity: u8, chunk_index: usize) -> Function {
        Function {
            name,
            arity,
            chunk_index,
            upvalue_count: 0,
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

pub struct Closure {
    pub function: ObjectPtr,
    pub upvalues: Vec<ObjectPtr>,
}

impl Closure {
    // `function` must have `ObjectType::Function`.
    pub fn new(function: ObjectPtr, upvalues: Vec<ObjectPtr>) -> Closure {
        Closure {
            function,
            upvalues,
        }
    }
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.function.unwrap_function())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Upvalue {
    Open(usize),
    Closed(Value),
}

pub struct Class {
    pub name: String,
}

impl Class {
    pub fn new(name: String) -> Class {
        Class {
            name,
        }
    }
}

pub struct Instance {
    pub class: ObjectPtr,
    pub fields: HashMap<String, Value>,
}

impl Instance {
    pub fn new(class: ObjectPtr) -> Instance {
        Instance {
            class,
            fields: HashMap::new(),
        }
    }
}
