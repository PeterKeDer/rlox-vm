use crate::object::{Object, ObjectPtr};

pub trait ObjectAllocator {
    fn allocate(&mut self, object: Object) -> ObjectPtr;
    fn deallocate(&mut self, ptr: ObjectPtr);
    fn destroy(self);
}

pub struct DefaultAllocator {
    objects: Vec<ObjectPtr>,
}

impl DefaultAllocator {
    pub fn new() -> DefaultAllocator {
        DefaultAllocator {
            objects: vec![],
        }
    }
}

impl ObjectAllocator for DefaultAllocator {
    fn allocate(&mut self, object: Object) -> ObjectPtr {
        let ptr = ObjectPtr::alloc(object);
        self.objects.push(ptr.clone());
        ptr
    }

    fn deallocate(&mut self, _ptr: ObjectPtr) {
        // TODO: later when implementing gc
        unimplemented!()
    }

    fn destroy(self) {
        for ptr in self.objects.into_iter() {
            ObjectPtr::dealloc(ptr);
        }
    }
}
