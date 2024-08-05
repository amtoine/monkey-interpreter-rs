use crate::token::Token;

#[derive(Debug)]
pub(crate) struct Lexer {
    input: String,
    position: usize,
    char: char,
}

impl Lexer {
    pub(crate) fn new(input: String) -> Lexer {
        let mut lexer = Lexer {
            input,
            position: 0,
            char: '\0',
        };
        lexer.char = lexer.peek_char();

        lexer
    }

    fn peek_char(&self) -> char {
        self.input.chars().nth(self.position + 1).unwrap_or('\0')
    }

    fn read_char(&mut self) {
        self.char = self.peek_char();
        self.position += 1;
    }

    fn skip_whitespaces(&mut self) {
        while self.char.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        let identifier_start_position = self.position;
        while self.char.is_ascii_alphabetic() {
            self.read_char();
        }

        self.input[identifier_start_position..self.position].to_string()
    }

    fn read_integer(&mut self) -> u32 {
        let value_start_position = self.position;
        while self.char.is_ascii_digit() {
            self.read_char();
        }

        self.input[value_start_position..self.position]
            .to_string()
            .parse()
            .unwrap()
    }

    pub(crate) fn next_token(&mut self) -> Token {
        self.skip_whitespaces();

        let token = match self.char {
            '=' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    Token::EqualTo
                }
                _ => Token::Assign,
            },
            '+' => Token::Plus,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '\0' => Token::EndOfFile,
            '!' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    Token::NotEqualTo
                }
                _ => Token::Bang,
            },
            '-' => Token::Minus,
            '/' => Token::Slash,
            '*' => Token::Asterisk,
            '<' => Token::LessThan,
            '>' => Token::GreaterThan,
            _ => {
                if self.char.is_ascii_alphabetic() {
                    return match self.read_identifier().as_ref() {
                        "let" => Token::Let,
                        "fn" => Token::Function,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "return" => Token::Return,
                        "true" => Token::True,
                        "false" => Token::False,
                        id => Token::Identifier(id.to_string()),
                    };
                } else if self.char.is_ascii_digit() {
                    return Token::Int(self.read_integer());
                } else {
                    Token::Illegal
                }
            }
        };

        self.read_char();

        token
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::Lexer;
    use crate::token::Token;

    #[test]
    fn next_token() {
        let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);

!-/*1;
2 < 3 > 4;

if (6 < 7) {
    return true;
} else {
    return false;
}

8 == 8;
9 != 11;
";

        let expected_tokens = [
            Token::Let,
            Token::Identifier("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LeftParen,
            Token::Identifier("x".to_string()),
            Token::Comma,
            Token::Identifier("y".to_string()),
            Token::RightParen,
            Token::LeftBrace,
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Identifier("y".to_string()),
            Token::Semicolon,
            Token::RightBrace,
            Token::Semicolon,
            Token::Let,
            Token::Identifier("result".to_string()),
            Token::Assign,
            Token::Identifier("add".to_string()),
            Token::LeftParen,
            Token::Identifier("five".to_string()),
            Token::Comma,
            Token::Identifier("ten".to_string()),
            Token::RightParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(1),
            Token::Semicolon,
            Token::Int(2),
            Token::LessThan,
            Token::Int(3),
            Token::GreaterThan,
            Token::Int(4),
            Token::Semicolon,
            Token::If,
            Token::LeftParen,
            Token::Int(6),
            Token::LessThan,
            Token::Int(7),
            Token::RightParen,
            Token::LeftBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RightBrace,
            Token::Else,
            Token::LeftBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RightBrace,
            Token::Int(8),
            Token::EqualTo,
            Token::Int(8),
            Token::Semicolon,
            Token::Int(9),
            Token::NotEqualTo,
            Token::Int(11),
            Token::Semicolon,
            Token::EndOfFile,
        ];

        let mut lexer = Lexer::new(input.to_string());

        for (i, expected_token) in expected_tokens.iter().enumerate() {
            assert_eq!(&lexer.next_token(), expected_token, "token {}", i);
        }

        let input = "= 5";
        let expected_tokens = [Token::Assign, Token::Int(5)];
        let mut lexer = Lexer::new(input.to_string());
        for (i, expected_token) in expected_tokens.iter().enumerate() {
            assert_eq!(
                &lexer.next_token(),
                expected_token,
                "input: {:?}, token: {}",
                input,
                i
            );
        }
    }
}
