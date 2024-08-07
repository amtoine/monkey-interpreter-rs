use crate::{ast::Program, lexer::Lexer, token::Token};

struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut p = Self {
            lexer,
            curr_token: Token::Illegal,
            peek_token: Token::Illegal,
        };
        p.next_token();
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse(self) -> Program {
        Program::default()
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::Statement, lexer::Lexer};

    use super::Parser;

    #[test]
    fn let_statements() {
        let input = "let x = 5;
let y = 10;
let foobar = 838383;";
        let expected_identifiers = vec!["x", "y", "foobar"];

        let parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse();

        assert_eq!(
            program.statements.len(),
            3,
            "AST for input {:?} should contain 3 statements, found {}",
            input,
            program.statements.len()
        );

        for (i, (stmt, id)) in program
            .statements
            .iter()
            .zip(expected_identifiers.iter())
            .enumerate()
        {
            assert!(
                matches!(stmt, Statement::Let(..)),
                "{}-th statement should be a 'let', found {:?}",
                i,
                stmt,
            );

            let Statement::Let(name, _) = stmt;
            assert_eq!(
                name.0,
                id.to_string(),
                "{}-th statement should have identifier {:?}, found {:?}",
                i,
                id,
                name.0,
            );
        }
    }
}
