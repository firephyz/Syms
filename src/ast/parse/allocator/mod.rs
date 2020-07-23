use std::vec::Vec;
use std::default::Default;
use std::mem::MaybeUninit;
use std::ptr::NonNull;
use std::clone::Clone;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use string_cache::DefaultAtom;

//use crate::ast::SourceAST;

mod object;
pub mod handle;
pub use object::{Allocable, AllocObject};
use object::{RefCount};
use handle::AllocHandle;

// Global allocator
pub static mut ALLOC : Option<AllocatorInstance> = None;

///////////////////////////////////////////////////////////////////////////////
// Allocator trait that implements allocating general objects.
// Allocates Object handles that originate from Alloc.
///////////////////////////////////////////////////////////////////////////////
pub trait Allocator<'h, 'a: 'h, Object, Alloc: 'a>
  where Object: Allocable
{
    //type InitData = <Object as Allocable>::InitData;
    //type Handle = AllocHandle<'h, Object, Alloc>;
    type Handle;
    type Error;

    fn allocate<'f>(&mut self, data: <Object as Allocable>::InitData<'f>) -> Result<Self::Handle, Self::Error>;
    fn deallocate(&mut self, item: Self::Handle);
}

///////////////////////////////////////////////////////////////////////////////
// General object allocator
///////////////////////////////////////////////////////////////////////////////
pub struct AllocatorInstance {
    symbols: SymbolAllocator,
//    branches: BranchAllocator,
}

impl AllocatorInstance {
    pub fn new() -> Self {
        AllocatorInstance {
            symbols: SymbolAllocator::new(1000),
//            branches: BranchAllocator::new(1000),
        }
    }

    // pub fn allocate_symbol(&mut self, string: &str) -> Result<AllocHandle<AllocSymbol>, AllocError> {
    //     self.symbols.allocate(string as *const str)
    // }

    // pub fn allocate_branch(&mut self, left: SourceAST, right: SourceAST) -> Result<AllocHandle<AllocBranch>, AllocError> {
    //     let data = (left, right);
    //     self.branches.allocate(data)
    // }
}

impl<'h, 'a: 'h> Allocator<'h, 'a, AllocSymbol, SymbolAllocator> for AllocatorInstance {
    //type InitData = <AllocSymbol as Allocable>::InitData;
    type Handle = AllocHandle<'h, AllocSymbol, SymbolAllocator>;
    type Error = AllocError;

    fn allocate<'f>(&mut self, data: <AllocSymbol as Allocable>::InitData<'f>) -> Result<Self::Handle, Self::Error> {
        self.symbols.allocate(data)
    }

    fn deallocate(&mut self, item: Self::Handle) {
        self.symbols.deallocate(item)
    }
}

//trait AllocSymbolAllocator = Allocator<AllocSymbol>;

// impl Allocator<AllocSymbol> for AllocatorInstance {
//     type InitData = *const str;
//     type Error = AllocError;
//
//     fn allocate(&mut self, data: Self::InitData) -> Result<AllocHandle<AllocSymbol>, Self::Error> {
//         self.symbols.allocate(data)
//     }
//
//     fn deallocate(&mut self, item: &mut AllocSymbol) {
//         self.symbols.deallocate(item)
//     }
// }

// impl Allocator<AllocBranch> for AllocatorInstance {
//     type InitData = (SourceAST, SourceAST);
//     type Error = AllocError;
//
//     fn allocate(&mut self, data: Self::InitData) -> Result<AllocHandle<AllocBranch>, Self::Error> {
//         self.branches.allocate(data)
//     }
//
//     fn deallocate(&mut self, item: &mut AllocBranch) {
//         self.branches.deallocate(item)
//     }
// }

///////////////////////////////////////////////////////////////////////////////
// Allocation errors
///////////////////////////////////////////////////////////////////////////////
#[derive(Debug)]
pub enum AllocError {
    SymbolCap,
    BranchCap,
}

////////////////////////////////////////////////////////////////////////////////
// Allocator for symbols
////////////////////////////////////////////////////////////////////////////////
#[derive(Debug)]
pub struct AllocSymbol {
    atom: MaybeUninit<DefaultAtom>,
}

impl Clone for AllocSymbol {
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
            MaybeUninit::new(unsafe {self.atom.get_ref().clone()})
        };

        AllocSymbol {
            atom: atom,
        }
    }
}

impl Drop for AllocSymbol {
    fn drop(&mut self) {
        unsafe {
            std::mem::drop(self.atom.get_mut());
        }
    }
}

// Zero out atom as default so clone knows if it's initialized or not
impl Default for AllocSymbol {
    fn default() -> Self {
        AllocSymbol {
            atom: MaybeUninit::zeroed(),
        }
    }
}

impl Allocable for AllocSymbol {
    //type InitData = NonNull<str>;
    type InitData<'a> = &'a str;

    fn init<'f>(data: Self::InitData<'f>) -> Self {
        AllocSymbol {
            atom: MaybeUninit::new(unsafe { DefaultAtom::from(data) }),
        }
    }
}

///////////////////////////////////////////////////////////////////////////////
pub struct SymbolAllocator {
    cells: Vec<AllocObject<AllocSymbol>>,
    free: Vec<usize>,
    used: Vec<usize>,
    num_dead: u32,
    atom_hashes: HashMap<u32, usize>, // value is index into cell array
}

impl SymbolAllocator {
    fn new(count: usize) -> Self {
        SymbolAllocator {
            cells: vec![AllocObject::<AllocSymbol>::default(); count],
            free: (0..(count-1)).collect::<Vec<usize>>(),
            used: Vec::new(),
            num_dead: 0,
            atom_hashes: HashMap::with_capacity(count),
        }
    }

    fn full(&mut self) -> bool {
        if self.free.len() == 0 {
            if self.num_dead > 0 {
                self.garbage_collect();
                return self.full();
            }
            else {
                return true;
            }
        }
        else {
            return false;
        }
    }

    fn garbage_collect(&mut self) {
        for index in &self.used {
            //self.cells[index] = Default::default();
            if self.cells[*index].ref_count() == 0 {
                self.free.push(*index);
            }
        }
        self.num_dead = 0;
    }

    // fn get_handle<'a>(&'a self, index: usize) -> AllocHandle<'a, AllocSymbol> {
    //     AllocHandle::new(&self, index)
    // }
}

impl<'h, 'a: 'h> Allocator<'h, 'a, AllocSymbol, Self> for SymbolAllocator {
    type Handle = AllocHandle<'h, AllocSymbol, SymbolAllocator>;
    type Error = AllocError;

    fn allocate<'f>(&mut self, data: <AllocSymbol as Allocable>::InitData<'f>) -> Result<Self::Handle, Self::Error> {
        Err(AllocError::SymbolCap)
        // // Get atom for the symbol string
        // let atom = unsafe { DefaultAtom::from(&*data) };
        //
        // // If atom already exists, use that one
        // if self.atom_hashes.contains_key(&atom.get_hash()) {
        //     return Ok(self.get_handle(self.atom_hashes[&atom.get_hash()]));
        // }
        //
        // // Did we reach capacity?
        // if self.full() {
        //     return Err(AllocError::SymbolCap);
        // }
        //
        // // Reserve space for new atom
        // let new_index = self.free.pop().unwrap();
        // self.used.push(new_index);
        //
        // // Store index at hashed value of atom for quickly checking if atom is already used
        // self.atom_hashes.insert(atom.get_hash(), new_index);
        //
        // // Store atom
        // //self.cells.borrow_mut()[new_index] = AllocObject::new(&atom as *const DefaultAtom);
        //
        // Ok(self.get_handle(new_index))
    }

    // Allocated cell no longer holds needed data, drop the inner data
    fn deallocate(&mut self, item: Self::Handle) {
        // self.atom_hashes.remove(unsafe { &item.atom.get_ref().get_hash() });
        // self.num_dead += 1;

        //std::mem::drop(item)
    }
}

// ////////////////////////////////////////////////////////////////////////////////
// // Allocator for branches
// ////////////////////////////////////////////////////////////////////////////////
// #[derive(Debug)]
// pub struct AllocBranch {
//     left: MaybeUninit<SourceAST>,
//     right: MaybeUninit<SourceAST>,
// }
//
// impl Clone for AllocBranch {
//     fn clone(&self) -> Self {
//         AllocBranch {
//             left: MaybeUninit::new(unsafe {self.left.get_ref().clone()}),
//             right: MaybeUninit::new(unsafe {self.left.get_ref().clone()}),
//         }
//     }
// }
//
// impl Drop for AllocBranch {
//     fn drop(&mut self) {
//         std::mem::drop(self.left.get_ref());
//         std::mem::drop(self.right.get_ref());
//         ALLOC.as_mut().unwrap().deallocate(self);
//     }
// }
//
// impl Default for AllocBranch {
//     fn default() -> Self {
//         AllocBranch {
//             left: MaybeUninit::uninit(),
//             right: MaybeUninit::uninit(),
//         }
//     }
// }
// impl Allocatable for AllocBranch {
//     type InitData = (SourceAST, SourceAST);
//
//     fn init(data: Self::InitData) -> Self {
//         AllocBranch {
//             left: MaybeUninit::new(data.0),
//             right: MaybeUninit::new(data.1),
//         }
//     }
//
//     fn deallocate(&mut self) {
//         ALLOC.as_mut().unwrap().branches.deallocate(self)
//     }
// }
//
// struct BranchAllocator {
//     cells: Rc<RefCell<Vec<AllocObject<AllocBranch>>>>,
//     free: Vec<usize>,
//     used: Vec<usize>,
//     num_dead: u32,
// }
//
// impl BranchAllocator {
//     fn new(count: usize) -> Self {
//         BranchAllocator {
//             cells: Rc::new(RefCell::new(vec![AllocObject::<AllocBranch>::default(); count])),
//             free: (0..(count-1)).collect::<Vec<usize>>(),
//             used: Vec::new(),
//             num_dead: 0,
//         }
//     }
//
//     fn full(&mut self) -> bool {
//         if self.free.len() == 0 {
//             if self.num_dead > 0 {
//                 self.garbage_collect();
//                 return self.full();
//             }
//             else {
//                 return true;
//             }
//         }
//         else {
//             return false;
//         }
//     }
//
//     fn garbage_collect(&mut self) {
//         for index in &self.used {
//             //self.cells[index] = Default::default();
//             if self.cells.borrow_mut()[*index].ref_count() == 0 {
//                 self.free.push(*index);
//             }
//         }
//         self.num_dead = 0;
//     }
//
//     fn get_handle(&self, index: usize) -> AllocHandle<AllocBranch> {
//         AllocHandle::new(Rc::downgrade(&self.cells), index)
//     }
// }
//
// impl<'h> Allocator<AllocBranch> for BranchAllocator<'h> {
//     type InitData = (SourceAST, SourceAST);
//     type Error = AllocError;
//
//     fn allocate(&mut self, data: Self::InitData) -> Result<AllocHandle<AllocBranch, 'h>, Self::Error> {
//         // Did we reach capacity?
//         if self.full() {
//             return Err(AllocError::BranchCap);
//         }
//
//         // Reserve space for new atom
//         let new_index = self.free.pop().unwrap();
//         self.used.push(new_index);
//
//         // Store atom
//         self.cells.borrow_mut()[new_index] = AllocObject::new(data);
//
//         Ok(self.get_handle(new_index))
//     }
//
//     fn deallocate(&mut self, item: &mut AllocBranch) {
//         self.num_dead += 1;
//
//         //std::mem::drop(*item);
//     }
// }
