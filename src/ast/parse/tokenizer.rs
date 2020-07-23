use std::iter::Iterator;
use std::cmp::PartialEq;
use std::mem::MaybeUninit;
use std::string::String;
use std::marker::PhantomData;

#[derive(PartialEq)]
pub enum Token<'a> {
    LeftParens,
    RightParens,
    Symbol(&'a str),
}

// impl Eq for Token {
//     fn eq(&self, other) {
//         if self ==
//     }
// }

pub struct TokenIterator<'a> {
    string: String,
    // Avoid iterating through UTF8 chars when extracting a string range from string
    rest: MaybeUninit<&'a str>,
    //token_indicies: (usize, usize),
    token: Option<Token<'a>>, // Next token
    //phantom: PhantomData<&'a str>, // lifetime of string refs in Tokens
}

impl<'t> TokenIterator<'t> {
    pub fn new(string: &str) -> Option<Self> {
        if string.len() == 0 {
            return None;
        }

        let mut result = TokenIterator {
            string: String::from(string),
            rest: MaybeUninit::uninit(),
            //token_indicies: (0, 0),
            //phantom: PhantomData,
            token: None,
        };

        Some(result)
    }

    pub fn last(&self) -> Option<<Self as Iterator>::Item> {
        self.token
    }

    // Return the last parsed token
    pub fn peek(&mut self) -> Option<<Self as Iterator>::Item> {
        // // if indicies coincide, a new token needs to be parsed
        // if self.token_indicies.0 == self.token_indicies.1 {
        //     self.token = Some(self.next());
        //
        //     // Store token for later use
        //     self.
        // }
        // // else reuse last parsed token
        // else {
        //     let character =  self.string.chars.nth(self.token_indicies.0);
        //     let result = match character {
        //         "(" => Token::LeftParens,
        //         ")" => Token::RightParens,
        //         _ => {
        //             let symbol = self.string.get_unchecked(self.token_indicies.0..self.token_indicies.1);
        //             Token::Symbol(symbol)
        //         },
        //     };
        //
        //     Some(result)
        // }
        // if self.token == None {
        //     self.token = Some(self.next());
        // }
        // // else reuse last parsed token
        // else {
        //     // let character =  self.string.chars().nth(self.token_indicies.0);
        //     // let result = match character {
        //     //     "(" => Token::LeftParens,
        //     //     ")" => Token::RightParens,
        //     //     _ => {
        //     //         let symbol = self.rest.chars()[self.token_indicies.0..self.token_indicies.1];
        //     //         Token::Symbol(symbol)
        //     //     },
        //     // };
        //     //
        //     // Some(result)
        // }
        //
        // self.token

        // consume next whitespace
        self.consume_whitespace();

        // if reached end of string, done
        if self.rest.get_ref().len() == 0 {
            None
        }
        else {
            // determine next token length
            let first_char = self.rest.get_ref().chars().nth(0).unwrap();
            let token_length = match first_char {
                '(' => 1,
                ')' => 1,
                _ => self.get_next_symbol_length(),
            };

            let token = match first_char {
                '(' => Token::LeftParens,
                ')' => Token::RightParens,
                _ => Token::Symbol(&self.rest.get_ref()[..token_length]),
            };

            self.rest = MaybeUninit::new(&self.rest.get_ref()[token_length..]);

            Some(token)
        }
    }

    // // Consume the next whitespace in the tokenizer
    // fn consume_whitespace(&mut self) {
    //     let character = self.string.chars().nth(self.token_indicies.1).unwrap();
    //     while TokenIterator::is_whitespace(character) {
    //         self.token_indicies.1 += 1
    //     }
    //     self.token_indicies.0 = self.token_indicies.1;
    // }
    //
    // // Get length of the next symbol
    // fn get_next_symbol_length(&mut self) -> usize {
    //     // move token indicies to end of symbol
    //     let next_char = self.string.chars().nth(self.token_indicies.1).unwrap();
    //     while !(TokenIterator::is_whitespace(next_char) || next_char == '(' || next_char == ')') {
    //         self.token_indicies.1 += 1;
    //         next_char = self.string.chars().nth(self.token_indicies.1).unwrap();
    //     }
    //
    //     self.token_indicies.1 - self.token_indicies.0
    // }
    //
    // // Check if a character is whitespace
    // fn is_whitespace(character: char) -> bool {
    //     match character {
    //         ' ' | '\t' | '\n' => true,
    //         _ => false,
    //     }
    // }
    // Consume the next whitespace in the tokenizer
    fn consume_whitespace(&mut self) {
        // let character = self.rest.chars().nth(self.token_indicies.1).unwrap();
        // while TokenIterator::is_whitespace(character) {
        //     self.token_indicies.1 += 1
        // }
        // self.token_indicies.0 = self.token_indicies.1;
        let rest_index = self.rest.get_ref().chars().position(|c| { !TokenIterator::is_whitespace(c)}).unwrap_or_else(|| { self.rest.get_ref().len() });
        self.rest = MaybeUninit::new(&self.rest.get_ref()[rest_index..]);
    }

    // Get length of the next symbol
    fn get_next_symbol_length(&mut self) -> usize {
        // move token indicies to end of symbol
        // let next_char = self.string.chars().nth(self.token_indicies.1).unwrap();
        // while !(TokenIterator::is_whitespace(next_char) || next_char == '(' || next_char == ')') {
        //     self.token_indicies.1 += 1;
        //     next_char = self.string.chars().nth(self.token_indicies.1).unwrap();
        // }
        //
        // self.token_indicies.1 - self.token_indicies.0
        let predicate = |c| {
            TokenIterator::is_whitespace(c)
              || c == '('
              || c == ')'
        };
        self.rest.get_ref().chars().position(predicate).unwrap()
    }

    // Check if a character is whitespace
    fn is_whitespace(character: char) -> bool {
        match character {
            ' ' | '\t' | '\n' => true,
            _ => false,
        }
    }
}

// Iterate over tokens in the string
impl<'t> Iterator for TokenIterator<'t> {
    type Item = Token<'t>;

    // fn next(&mut self) -> Option<Self::Item> {
    //     // consume next whitespace
    //     self.consume_whitespace();
    //
    //     // if reached end of string, done
    //     if self.token_indicies.1 == self.string.len() {
    //         None
    //     }
    //     else {
    //         // move token_indicies start to the start of the next token
    //         self.token_indicies.0 = self.token_indicies.1;
    //
    //         // determine next token length
    //         let first_char = self.string.chars().nth(self.token_indicies.0).unwrap();
    //         let token_length = match first_char {
    //             '(' => 1,
    //             ')' => 1,
    //             _ => self.get_next_symbol_length(),
    //         };
    //
    //         // advance end of token indicies
    //         self.token_indicies.1 = self.token_indicies.1 + token_length;
    //
    //         // return parsed token
    //         self.get()
    //     }
    // }

    fn next(&mut self) -> Option<Self::Item> {
        // // consume next whitespace
        // self.consume_whitespace();
        //
        // // if reached end of string, done
        // if self.rest.len() == 0 {
        //     None
        // }
        // else {
        //     // determine next token length
        //     let first_char = self.rest.chars().nth(0).unwrap();
        //     // let token_length = match first_char {
        //     //     '(' => 1,
        //     //     ')' => 1,
        //     //     _ => self.get_next_symbol_length(),
        //     // };
        //
        //     // store token
        //     self.token = match first_char {
        //         '(' => Token::LeftParens,
        //         ')' => Token::RightParens,
        //         _ => Token::Symbol(self.rest[..self.get_next_symbol_length()]),
        //     };
        // }
        if self.token == None {
            self.token = self.peek();
        }

        self.token
    }
}
