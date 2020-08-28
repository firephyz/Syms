use std::fmt::{Debug, Formatter};
use std::ops::{Drop, Deref};
use std::clone::Clone;
use std::marker::PhantomData;

use super::AllocObject;
use super::traits::{Allocable, PrimAllocator, RefCount};

///////////////////////////////////////////////////////////////////////////////
// General allocation handle object with reference counting enabled.
// Derefs to T that is owned and allocated by A.
///////////////////////////////////////////////////////////////////////////////
pub struct AllocHandle<'a, T, A> {
    alloc: &'a A,
    index: usize,
    phantom: PhantomData<T>, // Needed for Deref
}

impl<'a, T: Allocable, A: PrimAllocator<'a, T>> AllocHandle<'a, T, A> {
    pub fn new(allocator: &'a A, index: usize) -> Self {
        let result = AllocHandle {
            alloc: allocator,
            index: index,
            phantom: PhantomData,
        };
        (*result).inc_ref();
        result
    }
}

impl<'a, T, A> Debug for AllocHandle<'a, T, A> {
    fn fmt(&self, fmt: &mut Formatter) -> std::fmt::Result {
        write!(fmt, "AllocHandle {{ alloc: {:?}, index: {} }}", self.alloc as *const A, self.index);
        Ok(())
    }
}

impl<'a, T: Allocable, A: PrimAllocator<'a, T>> Deref for AllocHandle<'a, T, A> {
    type Target = AllocObject<T>;

    fn deref(&self) -> &Self::Target {
        self.alloc.get(self.index)
    }
}

impl<'a, T: Allocable, A: PrimAllocator<'a, T>> Clone for AllocHandle<'a, T, A> {
    fn clone(&self) -> Self {
        (**self).inc_ref();
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
