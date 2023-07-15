use crate::token::Token;

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
    use super::Lexer;
    use crate::token::Token;

    #[test]
    fn next_token() {
        let input = "=+(){},;";

        let expected_tokens = [
            Token::Assign,
            Token::Plus,
            Token::LeftParen,
            Token::RightParen,
            Token::LeftBrace,
            Token::RightBrace,
            Token::Comma,
            Token::Semicolon,
            Token::EndOfFile,
        ];

        let mut lexer = Lexer::new(input.to_string());

        for (i, expected_token) in expected_tokens.iter().enumerate() {
            let token = lexer.next_token();

            assert_eq!(
                &token, expected_token,
                "test {} - wrong token: expected {:?} got {:?}",
                i, expected_token, token
            );
        }
    }
}
