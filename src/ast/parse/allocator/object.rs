use std::default::Default;
use std::clone::Clone;
use std::ops::Deref;
use std::cell::Cell;
use std::fmt::Debug;
use std::marker::Sized;

use super::Allocator;

///////////////////////////////////////////////////////////////////////////////
// Reference counted objects
///////////////////////////////////////////////////////////////////////////////
pub trait RefCount {
    fn inc_ref(&self);
    fn dec_ref(&self);
    fn ref_count(&self) -> u32;
}

pub(super) trait Allocable {
    type Alloc<'a>: Allocator<'a, Self>;
    type InitData<'a>;

    fn init(data: Self::InitData<'_>) -> Self where Self: Sized;
}

///////////////////////////////////////////////////////////////////////////////
// Objects to be allocated. Reference counted for garbage collection.
///////////////////////////////////////////////////////////////////////////////
#[derive(Debug)]
pub(super) struct AllocObject<T> {
    obj: T,
    count: Cell<u32>,
}

impl<T: Allocable> AllocObject<T> {
    fn new(data: T::InitData<'_>) -> Self {
        AllocObject {
            obj: T::init(data),
            count: Cell::new(0),
        }
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
