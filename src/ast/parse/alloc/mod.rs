use std::default::Default;
use std::ops::{Deref};
use std::fmt::{Display, Formatter, Debug};

mod allocator;
pub use allocator::AllocError;
use allocator::{AllocatorInstance, SymbolAllocator, AllocHandle, AllocObject};

///////////////////////////////////////////////////////////////////////////////
// Global allocator
///////////////////////////////////////////////////////////////////////////////
static mut ALLOC : Option<AllocatorInstance> = None;

fn get_allocator<'a>() -> &'a mut AllocatorInstance {
    unsafe {
        if ALLOC.is_none() {
            ALLOC = Some(AllocatorInstance::new());
        };

        ALLOC.as_mut().unwrap()
    }
}

///////////////////////////////////////////////////////////////////////////////
// Public allocated symbol
///////////////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone)]
pub(in crate::ast) struct AllocSymbol<'a> {
    handle: AllocHandle<'a, Symbol, SymbolAllocator>,
}

impl<'a> Display for AllocSymbol<'a> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(fmt, "{}", &**self.handle)
    }
}

impl<'a> Deref for AllocSymbol<'a> {
    type Target = AllocObject<Symbol>;
    fn deref(&self) -> &Self::Target {
        self.handle.deref()
    }
}

impl AllocSymbol<'_> {
    pub fn new(string: &str) -> Result<Self, AllocError> {
        Ok(AllocSymbol {
            handle: unsafe { get_allocator().allocate(string)? },
        })
    }
}

///////////////////////////////////////////////////////////////////////////////
// Tests
///////////////////////////////////////////////////////////////////////////////
pub(super) mod tests {
    pub fn test(symbols: impl Iterator<Result<AllocSymbol, AllocError>>) {
        // Gather object refs
        let objects = symbols.map(|sym| {
            sym.as_deref()
        });

        // Print objects
        for obj in objects {
            println!("{:?}", obj);
        }
    }
}
