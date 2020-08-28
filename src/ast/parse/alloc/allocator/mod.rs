use std::vec::Vec;
use std::collections::HashMap;
// use std::fmt::Display;

use string_cache::DefaultAtom;

mod handle;
pub use handle::AllocHandle;

mod object;
pub use object::AllocObject;

mod traits;
use traits::PrimAllocator;
pub use traits::{Allocator, Allocable, RefCount};

use super::primitives::Symbol;

///////////////////////////////////////////////////////////////////////////////
// Allocation errors
///////////////////////////////////////////////////////////////////////////////
#[derive(Debug)]
pub enum AllocError {
    UniqueSymbolCap,
    BranchCap,
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
            symbols: SymbolAllocator::new(100),
//            branches: BranchAllocator::new(1000),
        }
    }
}

impl<'a> Allocator<'a, Symbol> for AllocatorInstance {
    type Object = Symbol;
    type Handle<'h> = AllocHandle<'h, Symbol, <Symbol as Allocable>::Alloc<'h>>;
    type Error = AllocError;

    fn allocate(&'a mut self, data: <Symbol as Allocable>::InitData<'_>) -> Result<Self::Handle<'a>, Self::Error> {
        self.symbols.allocate(data)
    }

    fn deallocate(&mut self, item: Self::Handle<'a>) {
        self.symbols.deallocate(item)
    }
}

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
// Allocator for symbols
///////////////////////////////////////////////////////////////////////////////
pub struct SymbolAllocator {
    cells: Vec<AllocObject<Symbol>>,
    free: Vec<usize>,
    used: Vec<usize>,
    num_dead: u32,
    atom_hashes: HashMap<u32, usize>, // value is index into cell array
}

impl SymbolAllocator {
    fn new(count: usize) -> Self {
        SymbolAllocator {
            cells: vec![AllocObject::<Symbol>::default(); count],
            free: (0..count).collect::<Vec<usize>>(),
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
}

impl<'a> Allocator<'a, Symbol> for SymbolAllocator {
    type Object = Symbol;
    type Handle<'h> = AllocHandle<'h, Symbol, SymbolAllocator>;
    type Error = AllocError;

    fn allocate(&'a mut self, data: <Symbol as Allocable>::InitData<'_>) -> Result<Self::Handle<'a>, Self::Error> {
        // Get atom for the symbol string
        let atom = unsafe { DefaultAtom::from(data) };

        // If atom already exists, use that one
        if self.atom_hashes.contains_key(&atom.get_hash()) {
            return Ok(AllocHandle::new(self, self.atom_hashes[&atom.get_hash()]));
        }

        // Did we reach capacity?
        if self.full() {
            return Err(AllocError::UniqueSymbolCap);
        }

        // Reserve space for new atom
        let new_index = self.free.pop().unwrap();
        self.used.push(new_index);

        // Store index at hashed value of atom for quickly checking if atom is already used
        self.atom_hashes.insert(atom.get_hash(), new_index);

        // Store atom
        //self.cells[new_index] = AllocObject::new(&atom as *const DefaultAtom);
        self.cells[new_index] = AllocObject::new(data);

        Ok(AllocHandle::new(self, new_index))
    }

    // Allocated cell no longer holds needed data, drop the inner data
    fn deallocate(&mut self, item: Self::Handle<'a>) {
        // self.atom_hashes.remove(unsafe { &item.atom.get_ref().get_hash() });
        // self.num_dead += 1;

        //std::mem::drop(item)
    }
}

impl<'a> PrimAllocator<'a, Symbol> for SymbolAllocator {
    fn get(&self, index: usize) -> &AllocObject<Symbol> {
        &self.cells[index]
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
//             free: (0..count).collect::<Vec<usize>>(),
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
