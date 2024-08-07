use crate::{
    ast::{Expression, Identifier, Program, Statement},
    lexer::Lexer,
    token::Token,
};

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

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let name = if let Token::Identifier(name) = self.peek_token.clone() {
            self.next_token();
            Identifier(name.to_string())
        } else {
            return None;
        };
        if !matches!(self.peek_token, Token::Assign) {
            return None;
        }
        self.next_token();

        // FIXME: skipping expression until semicolon for now
        while !matches!(self.curr_token, Token::Semicolon) {
            self.next_token();
        }

        let dummy = Expression::Identifier(Identifier("dummy".into()));
        Some(Statement::Let(name, dummy))
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr_token {
            Token::Let => self.parse_let_statement(),
            _ => None,
        }
    }

    fn parse(&mut self) -> Program {
        let mut program = Program::default();

        while !matches!(self.curr_token, Token::EndOfFile) {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }
            self.next_token();
        }

        program
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

        let mut parser = Parser::new(Lexer::new(input.into()));
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
