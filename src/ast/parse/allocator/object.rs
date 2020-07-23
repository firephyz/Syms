use std::vec::Vec;
use std::default::Default;
use std::clone::Clone;
use std::ops::{Drop, Deref};
use std::cell::{Cell, RefCell};
use std::rc::Weak;
use std::fmt::{Debug, Formatter};

extern crate static_assertions;
use static_assertions::const_assert;

use super::Allocator;

///////////////////////////////////////////////////////////////////////////////
// Reference counted objects
///////////////////////////////////////////////////////////////////////////////
pub trait RefCount {
    fn inc_ref(&self);
    fn dec_ref(&self);
    fn ref_count(&self) -> u32;
}

pub trait Allocable {
    type InitData<'a>;

    fn init<'f>(data: Self::InitData<'f>) -> Self;
}

///////////////////////////////////////////////////////////////////////////////
// Objects to be allocated. Reference counted for garbage collection.
///////////////////////////////////////////////////////////////////////////////
#[derive(Debug)]
pub struct AllocObject<T> {
    obj: T,
    count: Cell<u32>,
}

impl<T: Allocable> Allocable for AllocObject<T> {
    type InitData<'a> = <T as Allocable>::InitData<'a>;
    //type Handle<'a> = AllocHandle<'a, A>;

    fn init<'f>(data: Self::InitData<'f>) -> Self {
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
