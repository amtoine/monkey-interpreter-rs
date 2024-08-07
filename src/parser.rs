use crate::{
    ast::{Expression, Identifier, Program, Statement},
    lexer::Lexer,
    token::Token,
};

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
}

impl From<Token> for Precedence {
    fn from(token: Token) -> Self {
        match token {
            Token::EqualTo | Token::NotEqualTo => Self::EQUALS,
            Token::LessThan | Token::GreaterThan => Self::LESSGREATER,
            Token::Plus | Token::Minus => Self::SUM,
            Token::Slash | Token::Asterisk => Self::PRODUCT,
            _ => Self::LOWEST,
        }
    }
}

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

        Some(Statement::Let(name, Expression::default()))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        // FIXME: skipping expression until semicolon for now
        while !matches!(self.curr_token, Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Return(Expression::default()))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let mut expr = match &self.curr_token {
            Token::Identifier(id) => Expression::Identifier(Identifier(id.into())),
            Token::Int(int) => Expression::IntegerLitteral(int.parse().unwrap()),
            Token::Bang | Token::Minus => {
                let op = self.curr_token.clone();
                self.next_token();
                if let Some(rhs) = self.parse_expression(Precedence::PREFIX) {
                    Expression::Prefix(op, Box::new(rhs))
                } else {
                    self.errors
                        .push("could not parse prefix expression".to_string());
                    return None;
                }
            }
            _ => return None,
        };

        while !matches!(self.peek_token, Token::Semicolon)
            && precedence < self.peek_token.clone().into()
        {
            expr = match &self.peek_token {
                Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Asterisk
                | Token::EqualTo
                | Token::NotEqualTo
                | Token::LessThan
                | Token::GreaterThan => {
                    self.next_token();

                    let op = self.curr_token.clone();
                    let precedence = self.curr_token.clone().into();

                    self.next_token();

                    if let Some(rhs) = self.parse_expression(precedence) {
                        Expression::Infix(Box::new(expr.clone()), op, Box::new(rhs))
                    } else {
                        self.errors
                            .push("could not parse infix expression".to_string());
                        return Some(expr);
                    }
                }
                _ => return Some(expr),
            };
        }

        Some(expr)
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expression = self.parse_expression(Precedence::LOWEST);

        match expression {
            Some(expr) => Some(Statement::Expression(expr)),
            None => None,
        }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
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
    use pretty_assertions::assert_eq;

    use crate::{
        ast::{Expression, Identifier, Program, Statement},
        lexer::Lexer,
        token::Token,
    };

    use super::Parser;

    fn parse(input: &str, expected: Program) {
        let mut parser = Parser::new(Lexer::new(input.to_string()));
        let parsed = parser.parse();

        assert!(
            parser.errors.is_empty(),
            "\n===\n{}\n===\nparsing generated {} errors: {:?}",
            input,
            parser.errors.len(),
            parser.errors,
        );

        assert_eq!(parsed, expected, "\n===\n{}\n===", input);
    }

    #[test]
    fn let_statements() {
        parse(
            "let x = 5;
let y = 10;
let foobar = 838383;",
            Program {
                statements: vec![
                    Statement::Let(Identifier("x".to_string()), Expression::default()),
                    Statement::Let(Identifier("y".to_string()), Expression::default()),
                    Statement::Let(Identifier("foobar".to_string()), Expression::default()),
                ],
            },
        );
    }

    #[test]
    fn return_statements() {
        parse(
            "return 5;
return 15 * 25;
return add(1, 2);",
            Program {
                statements: vec![
                    Statement::Return(Expression::default()),
                    Statement::Return(Expression::default()),
                    Statement::Return(Expression::default()),
                ],
            },
        );
    }

    #[test]
    fn identifier_expression() {
        parse(
            "foobar;",
            Program {
                statements: vec![Statement::Expression(Expression::Identifier(Identifier(
                    "foobar".to_string(),
                )))],
            },
        );
    }

    #[test]
    fn integer_litteral_expression() {
        parse(
            "5;",
            Program {
                statements: vec![Statement::Expression(Expression::IntegerLitteral(5))],
            },
        );
    }

    #[test]
    fn prefix_expression() {
        for (input, op, value) in [("!5;", Token::Bang, 5), ("-15", Token::Minus, 15)] {
            parse(
                input,
                Program {
                    statements: vec![Statement::Expression(Expression::Prefix(
                        op,
                        Box::new(Expression::IntegerLitteral(value)),
                    ))],
                },
            );
        }
    }

    #[test]
    fn infix_expression() {
        for (input, lhs, op, rhs) in [
            ("5 + 5;", 5, Token::Plus, 5),
            ("5 - 5;", 5, Token::Minus, 5),
            ("5 * 5;", 5, Token::Asterisk, 5),
            ("5 / 5;", 5, Token::Slash, 5),
            ("5 > 5;", 5, Token::GreaterThan, 5),
            ("5 < 5;", 5, Token::LessThan, 5),
            ("5 == 5;", 5, Token::EqualTo, 5),
            ("5 != 5;", 5, Token::NotEqualTo, 5),
        ] {
            parse(
                input,
                Program {
                    statements: vec![Statement::Expression(Expression::Infix(
                        Box::new(Expression::IntegerLitteral(lhs)),
                        op,
                        Box::new(Expression::IntegerLitteral(rhs)),
                    ))],
                },
            );
        }
    }

    #[test]
    fn operator_precedence() {
        for (input, statements) in [
            (
                "-a * b",
                vec![Statement::Expression(Expression::Infix(
                    Box::new(Expression::Prefix(
                        Token::Minus,
                        Box::new(Expression::Identifier(Identifier("a".to_string()))),
                    )),
                    Token::Asterisk,
                    Box::new(Expression::Identifier(Identifier("b".to_string()))),
                ))],
            ),
            (
                "!-a",
                vec![Statement::Expression(Expression::Prefix(
                    Token::Bang,
                    Box::new(Expression::Prefix(
                        Token::Minus,
                        Box::new(Expression::Identifier(Identifier("a".to_string()))),
                    )),
                ))],
            ),
            (
                "a + b + c",
                vec![Statement::Expression(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Identifier(Identifier("a".to_string()))),
                        Token::Plus,
                        Box::new(Expression::Identifier(Identifier("b".to_string()))),
                    )),
                    Token::Plus,
                    Box::new(Expression::Identifier(Identifier("c".to_string()))),
                ))],
            ),
            (
                "a + b - c",
                vec![Statement::Expression(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Identifier(Identifier("a".to_string()))),
                        Token::Plus,
                        Box::new(Expression::Identifier(Identifier("b".to_string()))),
                    )),
                    Token::Minus,
                    Box::new(Expression::Identifier(Identifier("c".to_string()))),
                ))],
            ),
            (
                "a * b * c",
                vec![Statement::Expression(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Identifier(Identifier("a".to_string()))),
                        Token::Asterisk,
                        Box::new(Expression::Identifier(Identifier("b".to_string()))),
                    )),
                    Token::Asterisk,
                    Box::new(Expression::Identifier(Identifier("c".to_string()))),
                ))],
            ),
            (
                "a * b / c",
                vec![Statement::Expression(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Identifier(Identifier("a".to_string()))),
                        Token::Asterisk,
                        Box::new(Expression::Identifier(Identifier("b".to_string()))),
                    )),
                    Token::Slash,
                    Box::new(Expression::Identifier(Identifier("c".to_string()))),
                ))],
            ),
            (
                "a + b / c",
                vec![Statement::Expression(Expression::Infix(
                    Box::new(Expression::Identifier(Identifier("a".to_string()))),
                    Token::Plus,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Identifier(Identifier("b".to_string()))),
                        Token::Slash,
                        Box::new(Expression::Identifier(Identifier("c".to_string()))),
                    )),
                ))],
            ),
            (
                "a + b * c + d / e - f",
                vec![Statement::Expression(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Infix(
                            Box::new(Expression::Identifier(Identifier("a".to_string()))),
                            Token::Plus,
                            Box::new(Expression::Infix(
                                Box::new(Expression::Identifier(Identifier("b".to_string()))),
                                Token::Asterisk,
                                Box::new(Expression::Identifier(Identifier("c".to_string()))),
                            )),
                        )),
                        Token::Plus,
                        Box::new(Expression::Infix(
                            Box::new(Expression::Identifier(Identifier("d".to_string()))),
                            Token::Slash,
                            Box::new(Expression::Identifier(Identifier("e".to_string()))),
                        )),
                    )),
                    Token::Minus,
                    Box::new(Expression::Identifier(Identifier("f".to_string()))),
                ))],
            ),
            (
                "3 + 4; -5 * 5",
                vec![
                    Statement::Expression(Expression::Infix(
                        Box::new(Expression::IntegerLitteral(3)),
                        Token::Plus,
                        Box::new(Expression::IntegerLitteral(4)),
                    )),
                    Statement::Expression(Expression::Infix(
                        Box::new(Expression::Prefix(
                            Token::Minus,
                            Box::new(Expression::IntegerLitteral(5)),
                        )),
                        Token::Asterisk,
                        Box::new(Expression::IntegerLitteral(5)),
                    )),
                ],
            ),
            (
                "5 > 4 == 3 < 4",
                vec![Statement::Expression(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::IntegerLitteral(5)),
                        Token::GreaterThan,
                        Box::new(Expression::IntegerLitteral(4)),
                    )),
                    Token::EqualTo,
                    Box::new(Expression::Infix(
                        Box::new(Expression::IntegerLitteral(3)),
                        Token::LessThan,
                        Box::new(Expression::IntegerLitteral(4)),
                    )),
                ))],
            ),
            (
                "5 < 4 != 3 > 4",
                vec![Statement::Expression(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::IntegerLitteral(5)),
                        Token::LessThan,
                        Box::new(Expression::IntegerLitteral(4)),
                    )),
                    Token::NotEqualTo,
                    Box::new(Expression::Infix(
                        Box::new(Expression::IntegerLitteral(3)),
                        Token::GreaterThan,
                        Box::new(Expression::IntegerLitteral(4)),
                    )),
                ))],
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                vec![Statement::Expression(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::IntegerLitteral(3)),
                        Token::Plus,
                        Box::new(Expression::Infix(
                            Box::new(Expression::IntegerLitteral(4)),
                            Token::Asterisk,
                            Box::new(Expression::IntegerLitteral(5)),
                        )),
                    )),
                    Token::EqualTo,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Infix(
                            Box::new(Expression::IntegerLitteral(3)),
                            Token::Asterisk,
                            Box::new(Expression::IntegerLitteral(1)),
                        )),
                        Token::Plus,
                        Box::new(Expression::Infix(
                            Box::new(Expression::IntegerLitteral(4)),
                            Token::Asterisk,
                            Box::new(Expression::IntegerLitteral(5)),
                        )),
                    )),
                ))],
            ),
        ] {
            parse(input, Program { statements });
        }
    }
}
