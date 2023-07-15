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
                token, expected_token,
                "test {} - wrong token: expected {} got {}",
                i, expected_token, token
            );
        }
    }
}
