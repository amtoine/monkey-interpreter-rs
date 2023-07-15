use crate::token::Token;

#[derive(Debug)]
struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    char: char,
}

impl Lexer {
    fn new(input: String) -> Lexer {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            char: '\0',
        };
        lexer.read_char();

        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.char = '\0';
        } else {
            self.char = self.input.chars().nth(self.read_position).unwrap();
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn next_token(&mut self) -> Token {
        let token = match self.char {
            '=' => Token::Assign,
            '+' => Token::Plus,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '\0' => Token::EndOfFile,
            _ => Token::Illegal,
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
            Token::Identifier("x".to_string()),
            Token::Comma,
            Token::Identifier("y".to_string()),
            Token::RightParen,
            Token::Semicolon,
            Token::EndOfFile,
        ];

        let mut lexer = Lexer::new(input.to_string());

        for (i, expected_token) in expected_tokens.iter().enumerate() {
            assert_eq!(&lexer.next_token(), expected_token, "token {}", i);
        }
    }
}
