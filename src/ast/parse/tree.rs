use std::fmt::Debug;
use std::clone::Clone;
use std::mem::MaybeUninit;

use super::allocator::{ALLOC, AllocHandle, AllocSymbol, AllocBranch, AllocError};
use super::tokenizer::{TokenIterator, Token};
use super::ALLOC;
use crate::ast::error::SourceASTError;

#[derive(Debug, Clone)]
pub enum SourceAST {
    Nil,
    Symbol(AllocHandle<AllocSymbol>),
    Branch(AllocHandle<AllocBranch>),
}

impl SourceAST {
    pub fn new_symbol(string: &str) -> Result<SourceAST, AllocError> {
        Ok(SourceAST::Symbol(ALLOC.as_mut().unwrap().allocate_symbol(string)?))
    }

    pub fn new_branch(left: SourceAST, right: SourceAST) -> Result<SourceAST, AllocError> {
        Ok(SourceAST::Branch(ALLOC.as_mut().unwrap().allocate_branch(left, right)?))
    }

    fn parse() -> Result<Self, SourceASTError> {
        Ok(match TOKENS.get_mut().next().ok_or(SourceASTParseError::UnexpectedEOF)? {
            Token::LeftParens => Ok(SourceAST::parse_list()?),
            Token::RightParens => Err(SourceASTParseError::UnexpectedClosingParens),
            Token::Symbol(symbol) => Ok(SourceAST::new_symbol(symbol)?),
        }?)
    }

    fn parse_list() -> Result<Self, SourceASTError> {
        // Parse the contents of the list
        let contents = match TOKENS.get_mut().next().ok_or(SourceASTParseError::UnexpectedEOF)? {
            Token::LeftParens => SourceAST::parse_list()?,
            Token::RightParens => SourceAST::Nil,
            Token::Symbol(symbol) => SourceAST::new_symbol(symbol)?,
        };

        // If list ends immediately, contents is the end of the list itself, nil
        if TOKENS.get_mut().last() == Some(Token::RightParens) {
            return Ok(contents);
        }

        // Concatenate contents with the rest of the list
        Ok(SourceAST::new_branch(contents, SourceAST::parse_list()?)?)
    }
}

impl FromStr for SourceAST {
    type Err = SourceASTError;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        ALLOC.init();

        TOKENS = MaybeUninit::new(TokenIterator::new(string).ok_or(SourceASTParseError::NoInput)?);

        SourceAST::parse()
    }
}
