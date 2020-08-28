#![feature(associated_type_bounds)]
#![feature(generic_associated_types)]
#![feature(maybe_uninit_ref)]
#![feature(trait_alias)]
#![feature(const_fn)]
#![feature(associated_type_defaults)]
#![feature(inner_deref)]

#![feature(unboxed_closures)]
#![feature(fn_traits)]
#![feature(const_transmute)]

// #[macro_use]
// extern crate enum_display_derive;

pub mod ast;

#[cfg(test)]
mod tests {
    use super::ast;

    #[test]
    fn test() {
        ast::parse::tests::test();
    }
}
