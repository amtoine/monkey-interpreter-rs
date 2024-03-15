use crate::{ast::Program, lexer::Lexer, token::Token};

#[derive(Debug)]
struct Parser {
    lexer: Lexer,
    curr: Token,
    peek: Token,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut p = Parser {
            lexer,
            curr: Token::Illegal,
            peek: Token::Illegal,
        };

        p.next_token();
        p.next_token();

        p
    }

    fn next_token(&mut self) {
        self.curr = self.peek.clone();
        self.peek = self.lexer.next_token();
    }

    fn parse(self) -> Program {
        Program { statements: vec![] }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::{ast::Statement, lexer::Lexer};

    use super::Parser;

    #[test]
    fn parsing() {
        let input = "let x = 5;
let y = 10;
let foobar = 838383;
";
        let lexer = Lexer::new(input.into());
        let parser = Parser::new(lexer);
        let program = parser.parse();

        assert_eq!(
            program.statements.len(),
            3,
            "program does not contain 3 statements, got {}",
            program.statements.len()
        );

        let expected_identifiers = vec!["x", "y", "foobar"];
        for (s, e) in program.statements.iter().zip(expected_identifiers) {
            matches!(s, Statement::Let(..));
            let Statement::Let(id, _) = s;

            assert_eq!(id.value, e);
        }
    }
}
