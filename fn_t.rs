use std::fmt;

use crate::fntype::types::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Print,
    Bang,
    BangEq,
    Eq,
    EqEq,
    Greater,
    GreaterEq,
    Less,
    LessEq,
    And,
    Class,
    Else,
    False,
    True,
    Fun,
    For,
    In,
    If,
    Null,
    Or,
    Println,
    Return,
    Super,
    SelF,
    Let,
    While,
    Xor,
    Break,
    Continue,
    Using,
    As,
    From,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semi,
    Slash,
    Percent,
    Star,
    Bra,
    Ket,
    Str,
    Num,

    Ident,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub t: TokenType,
    pub line: u64,
    pub lexeme: String,
    pub literal: Type,
}

impl Token {
    pub fn new(lexeme: String, literal: Type, t: TokenType, line: u64) -> Token {
        Token {
            lexeme,
            literal,
            t,
            line,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {:?} {:?}", self.lexeme, self.literal, self.t)
    }
}
