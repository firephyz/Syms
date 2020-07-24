use std::default::Default;
use std::clone::Clone;
use std::ops::Deref;
use std::cell::Cell;
use std::fmt::{Display, Debug, Formatter};

use super::traits::{Allocable, RefCount};

///////////////////////////////////////////////////////////////////////////////
// Objects to be allocated. Reference counted for garbage collection.
///////////////////////////////////////////////////////////////////////////////
pub(in crate::ast::parse::alloc) struct AllocObject<T> {
    obj: T,
    count: Cell<u32>,
}

impl<T: Allocable> AllocObject<T> {
    pub fn new(data: T::InitData<'_>) -> Self {
        AllocObject {
            obj: T::init(data),
            count: Cell::new(0),
        }
    }
}

impl<T: Display> Debug for AllocObject<T> {
    fn fmt(&self, fmt: &mut Formatter) -> std::fmt::Result {
        fmt.debug_struct("AllocObject")
           .field("obj", &format!("{}", self.obj))
           .field("count", unsafe {&*self.count.as_ptr()})
           .finish()
    }
}

impl<T: Default> Default for AllocObject<T> {
    fn default() -> Self {
        AllocObject {
            obj: T::default(),
            count: Cell::new(0),
        }
    }
}

impl<T> Deref for AllocObject<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.obj
    }
}

// Count how many references exist to the object
impl<T> RefCount for AllocObject<T> {
    fn inc_ref(&self) {
        self.count.set(self.count.get() + 1);
    }

    fn dec_ref(&self) {
        self.count.set(self.count.get() - 1);
    }

    fn ref_count(&self) -> u32 {
        self.count.get().clone()
    }
}

impl<T: Clone> Clone for AllocObject<T> {
    fn clone(&self) -> Self {
        AllocObject {
            obj: self.obj.clone(),
            count: self.count.clone(),
        }
    }
}
