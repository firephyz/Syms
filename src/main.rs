use std::io;
use std::string::String;

extern crate syms;

// use syms::ast::SourceAST;
//
// fn main() {
//     let stdin = io::stdin();
//
//     let ast = SourceAST::from("(test (ing syms))");
//
//     // let mut string = String::new();
//     // match stdin.read_line(&mut string) {
//     //     Ok(n) => { println!("Read {} bytes.\nString: {}", n, string); },
//     //     Err(e) => { println!("Error: {}", e); },
//     // };
//     println!("{:?}", &ast);
// }

use syms::ast::parse::tests::test;

fn main() {
    // let mut alloc = AllocatorInstance::new();
    // let testing1 = &mut alloc;
    // // let clos = closure { reference: &mut alloc, };
    // // let clos_actual = |num: u32| {
    // //     testing1.allocate(format!("Symbol-{}", num).as_str()).unwrap()
    // // };
    // // let symbols = (1..5).map(clos).collect::<Vec<AllocHandle<AllocSymbol>>>();
    // let symbols = vec![alloc.allocate(format!("Symbol-1").as_str()).unwrap()];
    //
    // // symbols.append(&mut (4..5).map(|num| {
    // //             alloc.allocate(format!("Symbol-{}", num).as_str()).unwrap()
    // //         }).collect::<Vec<AllocHandle<AllocSymbol>>>());
    //
    // for symbol in &symbols {
    //     println!("{:?}", symbol);
    // }
    test();
}
