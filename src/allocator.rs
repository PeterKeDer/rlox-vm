use crate::object::{Value, Object, ObjectPtr, Upvalue, ObjectPtrLinkedList, ObjectPtrAdapter, ObjectPtrElement};
use crate::chunk::Chunk;

pub trait ObjectAllocator {
    fn object_count(&self) -> usize;
    fn allocate(&mut self, object: Object) -> ObjectPtr;
    fn should_trigger_gc(&self) -> bool;
    fn collect_garbage(&mut self, roots: &[ObjectPtr], chunks: &[Chunk]);
    fn destroy(self);
}

const INITIAL_GC: usize = 64;
const GC_HEAP_GROWTH_FACTOR: usize = 2;

pub struct DefaultAllocator {
    objects: ObjectPtrLinkedList,
    object_count: usize,
    next_gc: usize,
}

impl DefaultAllocator {
    pub fn new() -> DefaultAllocator {
        DefaultAllocator {
            objects: ObjectPtrLinkedList::new(ObjectPtrAdapter::new()),
            object_count: 0,
            next_gc: INITIAL_GC,
        }
    }
}

impl ObjectAllocator for DefaultAllocator {
    fn object_count(&self) -> usize {
        self.object_count
    }

    fn allocate(&mut self, object: Object) -> ObjectPtr {
        self.object_count += 1;
        let ptr = ObjectPtr::alloc(object);
        self.objects.push_back(Box::new(ObjectPtrElement::new(ptr.clone())));
        ptr
    }

    fn should_trigger_gc(&self) -> bool {
        self.object_count >= self.next_gc
    }

    fn collect_garbage(&mut self, roots: &[ObjectPtr], chunks: &[Chunk]) {
        self.object_count = GarbageCollector::collect_garbage(&mut self.objects, roots, chunks);
        self.next_gc = std::cmp::min(self.object_count * GC_HEAP_GROWTH_FACTOR, INITIAL_GC);
    }

    fn destroy(self) {
        for elem in self.objects.into_iter() {
            ObjectPtr::dealloc(elem.value);
        }
    }
}
struct GarbageCollector<'a> {
    objects: &'a mut ObjectPtrLinkedList,
    chunks: &'a [Chunk],
    gray_stack: Vec<ObjectPtr>,
}

impl<'a> GarbageCollector<'a> {
    /// Collect garbage and return the new object count.
    fn collect_garbage(objects: &mut ObjectPtrLinkedList, roots: &[ObjectPtr], chunks: &[Chunk]) -> usize {
        let mut gc = GarbageCollector {
            objects,
            chunks,
            gray_stack: vec![],
        };

        gc.start(roots)
    }

    /// Collect garbage using a basic mark-sweep algorithm. Objects have three states: white, gray, and black.
    /// Initially, all objects are marked as white. Start by marking all roots gray.
    /// Visit all gray objects and mark all their white references "gray", then mark it black.
    /// Repeat until there are no gray objects remaining. All objects should now be either white or black.
    /// Each black object either is a root or referenced by a root, directly or indirectly. Deallocate all white objects.
    fn start(&mut self, roots: &[ObjectPtr]) -> usize {
        // Mark roots
        for root in roots {
            self.mark_object(root.clone());
        }

        // Trace references and add referenced objects to the gray stack until it is empty
        // Objects that are gray means referenced but not traced yet (waiting in the gray stack)
        // After they are traced, they are marked as black
        while let Some(object) = self.gray_stack.pop() {
            match &*object {
                Object::Upvalue(upvalue) => {
                    match &*upvalue.borrow() {
                        // The value is still on the stack, should already be marked
                        Upvalue::Open(_) => (),
                        Upvalue::Closed(value) => self.mark_value(value),
                    }
                },
                Object::Function(function) => {
                    let chunk = &self.chunks[function.chunk_index];

                    for constant in &chunk.constants {
                        self.mark_value(constant);
                    }
                },
                Object::Closure(closure) => {
                    self.mark_object(closure.function.clone());

                    for upvalue in &closure.upvalues {
                        self.mark_object(upvalue.clone());
                    }
                }
                // No references in these types of objects
                _ => (),
            }
        }

        let mut new_object_count = 0;

        // Now, every object is either black or white
        // Deallocate all white objects and remove them from the `objects` vector
        let mut cursor = self.objects.front_mut();

        while let Some(elem) = cursor.get() {
            if elem.value.deref_wrapper().is_marked {
                // Marked - mark as white for next use and advance to next object
                elem.value.clone().deref_wrapper_mut().is_marked = false;
                cursor.move_next();

                new_object_count += 1;

            } else {
                // Not marked - dealloc
                let ptr = cursor.remove().unwrap().value;
                ObjectPtr::dealloc(ptr);
            }
        }

        new_object_count
    }

    fn mark_value(&mut self, value: &Value) {
        if let Value::Object(object) = value {
            self.mark_object(object.clone());
        }
    }

    /// Mark an object as gray if it is not already marked.
    fn mark_object(&mut self, mut ptr: ObjectPtr) {
        let wrapper = ptr.deref_wrapper_mut();

        // Only mark and add to stack if not visited before
        if !wrapper.is_marked {
            wrapper.is_marked = true;
            self.gray_stack.push(ptr);
        }
    }
}
