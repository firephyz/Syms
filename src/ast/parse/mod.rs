mod alloc;
// use std::ops::FnMut;
// use std::ops::Deref;

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

pub mod tests {
    use super::alloc::{AllocSymbol, AllocError};

    pub fn test() -> Result<(), AllocError> {
        // println!("Handles:\n");
        //
        // // Symbols
        // let symbols = (1..=5).chain(3..=7).map(|num| {
        //     AllocSymbol::new(format!("Symbol-{}", num).as_str())
        // }).collect::<Vec<_>>();
        //
        // // Print handles
        // for symbol in symbols.iter() {
        //     println!("{:?}", symbol);
        // }
        //
        // alloc::tests::test(symbols.iter());

        let symbols = (1..=5).chain(3..=7).map(|num| {
            AllocSymbol::new(format!("Symbol-{}", num).as_str())
        }).filter_map(Result::ok).collect::<Vec<_>>();

        for symbol in symbols.iter() {
            println!("{}", symbol);
        }

        Ok(())
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
