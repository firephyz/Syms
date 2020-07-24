use std::fmt::{Display, Debug, Formatter};
use std::mem::MaybeUninit;
use std::clone::Clone;
use std::ops::Drop;
use std::default::Default;

///////////////////////////////////////////////////////////////////////////////
// Symbols
///////////////////////////////////////////////////////////////////////////////
use string_cache::DefaultAtom;

use super::allocator::traits::Allocable;
use super::allocator::SymbolAllocator;

#[derive(Debug)]
pub(super) struct Symbol {
    atom: MaybeUninit<DefaultAtom>,
}

impl Clone for Symbol {
    fn clone(&self) -> Self {
        // Convert self.atom into &[u8] slice
        const size : usize = std::mem::size_of::<MaybeUninit<DefaultAtom>>();
        let bytes = unsafe {
            let ptr = std::mem::transmute::<&MaybeUninit<DefaultAtom>, *const u8>(&self.atom);
            std::ptr::slice_from_raw_parts(ptr, size).as_ref().unwrap()
        };

        // If maybeuninit is all zeroed, keep that. Otherwise, clone internal value.
        let atom = if bytes.iter().eq([0u8; size].iter()) {
            MaybeUninit::zeroed()
        }
        else {
            MaybeUninit::new(unsafeuse string_cache::DefaultAtom; {self.atom.get_ref().clone()})
        };

        Symbol {
            atom: atom,
        }
    }
}

impl Display for Symbol {
    fn fmt(&self, fmt: &mut Formatter) -> std::fmt::Result {
        write!(fmt, "{}", unsafe {self.atom.get_ref()})
    }
}

impl Drop for Symbol {
    fn drop(&mut self) {
        unsafe {
            std::mem::drop(self.atom.get_mut());
        }
    }
}

// Zero out atom as default so clone knows if it's initialized or not
impl Default for Symbol {
    fn default() -> Self {
        Symbol {
            atom: MaybeUninit::zeroed(),
        }
    }
}

impl Allocable for Symbol {
    type Alloc<'a> = SymbolAllocator;
    type InitData<'a> = &'a str;

    fn init(data: Self::InitData<'_>) -> Self {
        Symbol {
            atom: MaybeUninit::new(unsafe { DefaultAtom::from(data) }),
        }
    }
}
