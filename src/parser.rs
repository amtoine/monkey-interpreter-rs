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
    errors: Vec<String>,
}

impl Parser {
    fn new(lexer: Lexer) -> Self {
        let mut p = Parser {
            lexer,
            curr: Token::Illegal,
            peek: Token::Illegal,
            errors: vec![],
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
            match self.parse_statement() {
                Ok(statement) => program.statements.push(statement),
                Err(err) => self.errors.push(err),
            }

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.curr {
            Token::Let => self.parse_let_statement(),
            _ => Err("not implemented".into()),
        }
    }

    fn error(&mut self, expected: String) -> String {
        format!(
            "parser error: expected next token to be {}, got {:?} instead",
            expected, self.peek,
        )
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        if !matches!(self.peek, Token::Identifier(_)) {
            return Err(self.error("Identifier".into()));
        }
        self.next_token();

        let id = Identifier {
            value: format!("{:?}", self.curr),
        };

        if !matches!(self.peek, Token::Assign) {
            return Err(self.error("Assign".into()));
        }
        self.next_token();

        // TODO: skipping all other expressions until semicolon for now
        while !matches!(self.curr, Token::Semicolon) {
            self.next_token();
        }

        return Ok(Statement::Let(id, Expression::Dummy));
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::{
        ast::{Expression, Identifier, Statement},
        lexer::Lexer,
    };

    use super::Parser;

    #[test]
    fn parsing() {
        let input = "let x = 5;
let y = 10;
let foobar = 838383;
";
        let expected_program = vec![
            Statement::Let(Identifier { value: "x".into() }, Expression::Dummy),
            Statement::Let(Identifier { value: "y".into() }, Expression::Dummy),
            Statement::Let(
                Identifier {
                    value: "foobar".into(),
                },
                Expression::Dummy,
            ),
        ];

        let lexer = Lexer::new(input.into());
        let mut parser = Parser::new(lexer);
        let program = parser.parse();

        assert_eq!(
            parser.errors.len(),
            0,
            "parser should not have encountered errors, found {}: \n{}",
            parser.errors.len(),
            parser.errors.join("\n"),
        );

        assert_eq!(
            program.statements.len(),
            expected_program.len(),
            "program does not contain {} statements, got {}",
            expected_program.len(),
            program.statements.len()
        );

        for (s, e) in program.statements.iter().zip(expected_program) {
            assert_eq!(s, &e);
        }
    }
}
