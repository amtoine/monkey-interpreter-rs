#[derive(Debug, PartialEq, Clone, Eq, Hash)]
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
    Bang,
    Minus,
    Slash,
    Asterisk,
    LessThan,
    GreaterThan,
    If,
    Else,
    Return,
    True,
    False,
    EqualTo,
    NotEqualTo,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let repr = match self {
            Token::Illegal => "??",
            Token::EndOfFile => "EOF",
            Token::Identifier(id) => id,
            Token::Int(val) => {
                // NOTE: otherwise, a "temporary value" is created...
                write!(f, "{}", val)?;
                return Ok(());
            }
            Token::Assign => "=",
            Token::Plus => "+",
            Token::Comma => ",",
            Token::Semicolon => ";",
            Token::LeftParen => "(",
            Token::RightParen => ")",
            Token::LeftBrace => "{",
            Token::RightBrace => "}",
            Token::Function => "fn",
            Token::Let => "let",
            Token::Bang => "!",
            Token::Minus => "-",
            Token::Slash => "/",
            Token::Asterisk => "*",
            Token::LessThan => "<",
            Token::GreaterThan => ">",
            Token::If => "if",
            Token::Else => "else",
            Token::Return => "return",
            Token::True => "true",
            Token::False => "false",
            Token::EqualTo => "==",
            Token::NotEqualTo => "!=",
        };

        write!(f, "{}", repr)
    }
}
