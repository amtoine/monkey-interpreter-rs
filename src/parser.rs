use crate::{
    ast::{Expression, Identifier, Program, Statement},
    lexer::Lexer,
    token::Token,
};

struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut p = Self {
            lexer,
            curr_token: Token::Illegal,
            peek_token: Token::Illegal,
            errors: vec![],
        };
        p.next_token();
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn add_token_error(&mut self, expected: &str) {
        self.errors.push(format!(
            "expected {} token, found {:?}",
            expected, self.peek_token
        ));
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let name = if let Token::Identifier(name) = self.peek_token.clone() {
            self.next_token();
            Identifier(name.to_string())
        } else {
            self.add_token_error("identifier");
            return None;
        };
        if !matches!(self.peek_token, Token::Assign) {
            self.add_token_error("assignment");
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

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        // FIXME: skipping expression until semicolon for now
        while !matches!(self.curr_token, Token::Semicolon) {
            self.next_token();
        }

        let dummy = Expression::Identifier(Identifier("dummy".into()));
        Some(Statement::Return(dummy))
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
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
    use crate::{
        ast::{Program, Statement},
        lexer::Lexer,
    };

    use super::Parser;

    fn check_parser_errors(parser: &Parser, input: &str) {
        assert!(
            parser.errors.is_empty(),
            "parsing input {:?} generated {} errors: {:?}",
            input,
            parser.errors.len(),
            parser.errors,
        );
    }

    fn check_program(program: &Program, input: &str, nb_statements: usize) {
        assert_eq!(
            program.statements.len(),
            nb_statements,
            "AST for input {:?} should contain {} statements, found {}",
            input,
            nb_statements,
            program.statements.len()
        );
    }

    #[test]
    fn let_statements() {
        let input = "let x = 5;
let y = 10;
let foobar = 838383;";
        let expected_identifiers = vec!["x", "y", "foobar"];

        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse();

        check_parser_errors(&parser, input);
        check_program(&program, input, 3);

        for (i, (stmt, id)) in program
            .statements
            .iter()
            .zip(expected_identifiers.iter())
            .enumerate()
        {
            if let Statement::Let(name, _) = stmt {
                assert_eq!(
                    name.0,
                    id.to_string(),
                    "{}-th statement should have identifier {:?}, found {:?}",
                    i,
                    id,
                    name.0,
                );
            } else {
                panic!("{}-th statement should be a 'let', found {:?}", i, stmt,);
            }
        }
    }

    #[test]
    fn return_statements() {
        let input = "return 5;
return 15 * 25;
return add(1, 2);";

        let mut parser = Parser::new(Lexer::new(input.into()));
        let program = parser.parse();

        check_parser_errors(&parser, input);
        check_program(&program, input, 3);

        for (i, stmt) in program.statements.iter().enumerate() {
            assert!(
                matches!(stmt, Statement::Return(_)),
                "{}-th statement should be a 'return', found {:?}",
                i,
                stmt
            );
        }
    }
}
