#[derive(Debug, PartialEq)]
pub(crate) enum Token {
    Illegal,
    EndOfFile,
    Identifier(String),
    Int(u32),
    Assign,
    Plus,
    Comma,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Function,
    Let,
}
