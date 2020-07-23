use std::fmt::{Debug, Formatter};
use std::ops::{Drop, Deref};
use std::clone::Clone;
use std::marker::PhantomData;

use super::Allocator;
use super::object::Allocable;

///////////////////////////////////////////////////////////////////////////////
// General allocation handle object with reference counting enabled.
// Derefs to T that is owned and allocated by A.
///////////////////////////////////////////////////////////////////////////////
pub(super) struct AllocHandle<'a, T, A> {
    alloc: &'a A,
    index: usize,
    phantom: PhantomData<T>,
}

impl<'a, T, A> AllocHandle<'a, T, A> {
    pub fn new(allocator: &'a A, index: usize) -> Self {
        let result = AllocHandle {
            alloc: allocator,
            index: index,
            phantom: PhantomData,
        };
        //(*result).inc_ref();
        result
    }
}

impl<'a, T, A> Debug for AllocHandle<'a, T, A> {
    fn fmt(&self, fmt: &mut Formatter) -> std::fmt::Result {
        write!(fmt, "AllocHandle {{ alloc: {:?}, index: {} }}", self.alloc as *const A, self.index);
        Ok(())
    }
}

impl<'a, T: Allocable, A: Allocator<'a, T>> Deref for AllocHandle<'a, A, T> {
    type Target = A::Object;

    fn deref(&self) -> &Self::Target {
        const size : usize = std::mem::size_of::<*const u32>();
        //const_assert!(size == 8);
        let bytes : Vec<u8> = vec![0x0 as u8; size];
        unsafe { std::mem::transmute::<*const u8, &Self::Target>(bytes.as_ptr()) }
        //self.base.upgrade().unwrap().borrow().get(self.index).unwrap()
    }
}

impl<'a, T, A> Clone for AllocHandle<'a, T, A> {
    fn clone(&self) -> Self {
        //(*self).inc_ref();
        AllocHandle {
            alloc: self.alloc.clone(),
            index: self.index.clone(),
            phantom: PhantomData,
        }
    }
}

impl<'a, T, A> Drop for AllocHandle<'a, T, A> {
    fn drop(&mut self) {
        // Drop inner value
        // deallocate cell

        // (*self).dec_ref();
        // // Give allocator back the memory cell
        // if (*self).ref_count() == 0 {
        //     //(&ALLOC as &dyn AllocSymbolAllocator).deallocate(self);
        //     std::mem::drop(*self);
        // }
    }
}
