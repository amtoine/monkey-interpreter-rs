use crate::{
    ast::{Expression, Identifier, Program, Statement},
    lexer::Lexer,
    token::Token,
};

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

    fn parse(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while self.curr != Token::EndOfFile {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr {
            Token::Let => self.parse_let_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        if !matches!(self.peek, Token::Identifier(_)) {
            return None;
        }
        self.next_token();

        let id = Identifier {
            value: format!("{:?}", self.curr),
        };

        if !matches!(self.peek, Token::Assign) {
            return None;
        }
        self.next_token();

        // TODO: skipping all other expressions until semicolon for now
        while !matches!(self.curr, Token::Semicolon) {
            self.next_token();
        }

        return Some(Statement::Let(id, Expression::Dummy));
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
        let mut parser = Parser::new(lexer);
        let program = parser.parse();

        assert_eq!(
            program.statements.len(),
            3,
            "program does not contain 3 statements, got {}",
            program.statements.len()
        );

        let expected_identifiers = vec![
            r#"Identifier("x")"#,
            r#"Identifier("y")"#,
            r#"Identifier("foobar")"#,
        ];
        for (s, e) in program.statements.iter().zip(expected_identifiers) {
            matches!(s, Statement::Let(..));
            let Statement::Let(id, _) = s;

            assert_eq!(id.value, e);
        }
    }
}
