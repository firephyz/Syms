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

pub mod ast;

#[cfg(test)]
mod tests {
    #[test]
    fn test() {
        ast::parse::tests::test();
    }
}
