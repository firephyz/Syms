mod allocator;
use allocator::*;

use std::ops::FnMut;

// struct Closure<'a> {
//     reference: &'a mut AllocatorInstance,
// }
//
// impl<'a> FnMut<(u32,)> for Closure<'a> {
//     extern "rust-call" fn call_mut(&mut self, args: (u32,)) -> Self::Output {
//         self.call_once(args)
//     }
// }
//
// impl<'a> FnOnce<(u32,)> for Closure<'a> {
//     //type Output = AllocHandle<'a, AllocSymbol, SymbolAllocator>;
//     type Output = AllocSymbol<'a>;
//     extern "rust-call" fn call_once(self, args: (u32,)) -> Self::Output {
//         self.reference.allocate(format!("Symbol-{}", args.0).as_str()).unwrap()
//     }
// }

pub fn test() {
    // let mut alloc = AllocatorInstance::new();
    // let testing1 = &mut alloc;
    // let clos = Closure { reference: &mut alloc, };
    // let clos_actual = |num: u32| {
    //     testing1.allocate(format!("Symbol-{}", num).as_str()).unwrap()
    // };
    // let symbols = (1..5).map(clos).collect::<Vec<AllocHandle<AllocSymbol>>>();
    let symbols = vec![AllocSymbol::new(format!("Symbol-1").as_str()).unwrap()];

    // symbols.append(&mut (4..5).map(|num| {
    //             alloc.allocate(format!("Symbol-{}", num).as_str()).unwrap()
    //         }).collect::<Vec<AllocHandle<AllocSymbol>>>());

    for symbol in &symbols {
        println!("{:?}", symbol);
    }
}

// mod tokenizer;
// pub mod tree;
//
// // Tokenizer used for parsing SourceASTs
// pub static mut TOKENS : MaybeUninit<TokenIterator> = MaybeUninit::uninit();
//
// ///////////////////////////////////////////////////////////////////////////////
// // Error emitted during parsing SourceASTs
// ///////////////////////////////////////////////////////////////////////////////
// enum SourceASTParseError {
//     NoInput,
//     UnexpectedClosingParens,
//     UnexpectedEOF,
// }
