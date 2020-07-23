#![feature(associated_type_bounds)]
#![feature(generic_associated_types)]
#![feature(maybe_uninit_ref)]
#![feature(trait_alias)]
#![feature(const_fn)]
#![feature(associated_type_defaults)]

pub mod ast;

pub use ast::parse::allocator;