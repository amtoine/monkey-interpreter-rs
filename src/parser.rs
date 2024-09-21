use crate::{
    ast::{Expression, Program, Statement},
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

pub(crate) struct Parser {
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
}

impl Parser {
    pub(crate) fn new(lexer: Lexer) -> Self {
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
        self.next_token();
        let name = if let Token::Identifier(name) = self.curr_token.clone() {
            name.to_string()
        } else {
            self.add_token_error("identifier");
            return None;
        };

        self.next_token();
        if !matches!(self.curr_token, Token::Assign) {
            self.add_token_error("assignment");
            return None;
        }

        self.next_token();

        if let Some(expr) = self.parse_expression(Precedence::LOWEST) {
            Some(Statement::Let(name, expr))
        } else {
            self.errors
                .push("could not parse RHS of let statement".to_string());
            None
        }
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        if let Some(expr) = self.parse_expression(Precedence::LOWEST) {
            Some(Statement::Return(expr))
        } else {
            self.errors
                .push("could not parse RHS of return statement".to_string());
            None
        }
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let expr = if let Some(expr) = self.parse_expression(Precedence::LOWEST) {
            expr
        } else {
            self.errors
                .push("could not parse expression in grouped expression".to_string());
            return None;
        };

        self.next_token();
        if !matches!(self.curr_token, Token::RightParen) {
            self.errors.push(format!(
                "missing right parentheses in grouped expression, found {:?}",
                self.curr_token,
            ));
            return None;
        }

        Some(expr)
    }

    fn parse_block_statement(&mut self) -> Vec<Statement> {
        let mut statements = vec![];

        self.next_token();

        while !matches!(self.curr_token, Token::RightBrace)
            && !matches!(self.curr_token, Token::EndOfFile)
        {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt)
            }
            self.next_token();
        }

        statements
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        self.next_token();
        if !matches!(self.curr_token, Token::LeftParen) {
            self.errors.push(format!(
                "missing left parenthese in if expression, found {:?}",
                self.curr_token,
            ));
            return None;
        }

        let cond = self.parse_expression(Precedence::LOWEST)?;

        if !matches!(self.curr_token, Token::RightParen) {
            self.errors.push(format!(
                "missing right parenthese in if expression, found {:?}",
                self.curr_token,
            ));
            return None;
        }

        self.next_token();
        if !matches!(self.curr_token, Token::LeftBrace) {
            self.errors.push(format!(
                "missing left brace in if expression, found {:?}",
                self.curr_token,
            ));
            return None;
        }

        let then = self.parse_block_statement();

        let alt = if matches!(self.peek_token, Token::Else) {
            self.next_token();
            self.next_token();
            if !matches!(self.curr_token, Token::LeftBrace) {
                self.errors.push(format!(
                    "missing left brace in if expression, found {:?}",
                    self.curr_token,
                ));
                return None;
            }
            self.parse_block_statement()
        } else {
            vec![]
        };

        Some(Expression::If(Box::new(cond), then, alt))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let mut expr = match &self.curr_token {
            Token::Identifier(id) => Expression::Identifier(id.to_string()),
            Token::Int(int) => Expression::IntegerLitteral(int.parse().unwrap()),
            Token::True => Expression::Boolean(true),
            Token::False => Expression::Boolean(false),
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
            Token::LeftParen => {
                if let Some(grouped_expr) = self.parse_grouped_expression() {
                    grouped_expr
                } else {
                    self.errors
                        .push("could not parse grouped expression".to_string());
                    return None;
                }
            }
            Token::If => {
                if let Some(if_expr) = self.parse_if_expression() {
                    if_expr
                } else {
                    self.errors
                        .push("could not parse if expression".to_string());
                    return None;
                }
            }
            Token::Semicolon => return None,
            t => {
                self.errors.push(format!("no rule for parsing {:?}", t));
                return None;
            }
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
        match self.parse_expression(Precedence::LOWEST) {
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

    pub(crate) fn parse(&mut self) -> Program {
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
        ast::{Expression, Program, Statement},
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

    const TRUE: Expression = Expression::Boolean(true);
    const FALSE: Expression = Expression::Boolean(false);
    /// builds an [`Expression::IntegerLitteral`]
    macro_rules! int {
        ($val:expr) => {
            Expression::IntegerLitteral($val)
        };
    }
    /// builds an [`Expression::Identifier`] and takes care of the "string" conversion
    macro_rules! id {
        ($id:expr) => {
            Expression::Identifier($id.to_string())
        };
    }
    /// build an [`Expression::Prefix`] and takes care of the "boxing" of the right expression
    ///
    /// > supports trailing comma in case the macro is used on multiple lines
    macro_rules! prefix {
        ($op:expr, $expr:expr $(,)*) => {
            Expression::Prefix($op, Box::new($expr))
        };
    }
    /// build an [`Expression::Infix`] and takes care of the "boxing" of the expressions
    ///
    /// > supports trailing comma in case the macro is used on multiple lines
    macro_rules! infix {
        ($lhs:expr, $op:expr, $rhs:expr $(,)*) => {
            Expression::Infix(Box::new($lhs), $op, Box::new($rhs))
        };
    }
    /// build an [`Expression::If`] and takes care of the "boxing" of the expressions
    macro_rules! ifthenelse {
        ($cond:expr => $($then:expr),* $(,)*) => {
            Expression::If(
                Box::new($cond),
                vec![$(Statement::Expression($then)),*],
                vec![],
            )
        };
        ($cond:expr => $($then:expr),* => $($else:expr),* $(,)*) => {
            Expression::If(
                Box::new($cond),
                vec![$(Statement::Expression($then)),*],
                vec![$(Statement::Expression($else)),*],
            )
        };
    }

    #[test]
    fn let_statements() {
        parse(
            "let x = 5;
let y = 10;
let foobar = 838383;",
            Program {
                statements: vec![
                    Statement::Let("x".to_string(), int!(5)),
                    Statement::Let("y".to_string(), int!(10)),
                    Statement::Let("foobar".to_string(), int!(838383)),
                ],
            },
        );
    }

    #[test]
    fn return_statements() {
        parse(
            "return 5;
return 15 * 25;",
            Program {
                statements: vec![
                    Statement::Return(int!(5)),
                    Statement::Return(infix!(int!(15), Token::Asterisk, int!(25),)),
                ],
            },
        );
    }

    #[test]
    fn identifier_expression() {
        parse(
            "foobar;",
            Program {
                statements: vec![Statement::Expression(id!("foobar"))],
            },
        );
    }

    #[test]
    fn integer_litteral_expression() {
        parse(
            "5;",
            Program {
                statements: vec![Statement::Expression(int!(5))],
            },
        );
    }

    #[test]
    fn prefix_expression() {
        for (input, op, value) in [
            ("!5;", Token::Bang, int!(5)),
            ("-15", Token::Minus, int!(15)),
            ("!true", Token::Bang, TRUE),
            ("!false", Token::Bang, FALSE),
        ] {
            parse(
                input,
                Program {
                    statements: vec![Statement::Expression(prefix!(op, value))],
                },
            );
        }
    }

    #[test]
    fn infix_expression() {
        for (input, lhs, op, rhs) in [
            ("5 + 5;", int!(5), Token::Plus, int!(5)),
            ("5 - 5;", int!(5), Token::Minus, int!(5)),
            ("5 * 5;", int!(5), Token::Asterisk, int!(5)),
            ("5 / 5;", int!(5), Token::Slash, int!(5)),
            ("5 > 5;", int!(5), Token::GreaterThan, int!(5)),
            ("5 < 5;", int!(5), Token::LessThan, int!(5)),
            ("5 == 5;", int!(5), Token::EqualTo, int!(5)),
            ("5 != 5;", int!(5), Token::NotEqualTo, int!(5)),
            ("true == true", TRUE, Token::EqualTo, TRUE),
            ("true != false", TRUE, Token::NotEqualTo, FALSE),
            ("false == false", FALSE, Token::EqualTo, FALSE),
        ] {
            parse(
                input,
                Program {
                    statements: vec![Statement::Expression(infix!(lhs, op, rhs))],
                },
            );
        }
    }

    #[test]
    fn operator_precedence() {
        for (input, statements) in [
            (
                "-a * b",
                vec![Statement::Expression(infix!(
                    prefix!(Token::Minus, id!("a")),
                    Token::Asterisk,
                    id!("b"),
                ))],
            ),
            (
                "!-a",
                vec![Statement::Expression(prefix!(
                    Token::Bang,
                    prefix!(Token::Minus, id!("a"))
                ))],
            ),
            (
                "a + b + c",
                vec![Statement::Expression(infix!(
                    infix!(id!("a"), Token::Plus, id!("b")),
                    Token::Plus,
                    id!("c"),
                ))],
            ),
            (
                "a + b - c",
                vec![Statement::Expression(infix!(
                    infix!(id!("a"), Token::Plus, id!("b")),
                    Token::Minus,
                    id!("c"),
                ))],
            ),
            (
                "a * b * c",
                vec![Statement::Expression(infix!(
                    infix!(id!("a"), Token::Asterisk, id!("b")),
                    Token::Asterisk,
                    id!("c"),
                ))],
            ),
            (
                "a * b / c",
                vec![Statement::Expression(infix!(
                    infix!(id!("a"), Token::Asterisk, id!("b")),
                    Token::Slash,
                    id!("c"),
                ))],
            ),
            (
                "a + b / c",
                vec![Statement::Expression(infix!(
                    id!("a"),
                    Token::Plus,
                    infix!(id!("b"), Token::Slash, id!("c"))
                ))],
            ),
            (
                "a + b * c + d / e - f",
                vec![Statement::Expression(infix!(
                    infix!(
                        infix!(
                            id!("a"),
                            Token::Plus,
                            infix!(id!("b"), Token::Asterisk, id!("c")),
                        ),
                        Token::Plus,
                        infix!(id!("d"), Token::Slash, id!("e")),
                    ),
                    Token::Minus,
                    id!("f"),
                ))],
            ),
            (
                "3 + 4; -5 * 5",
                vec![
                    Statement::Expression(infix!(int!(3), Token::Plus, int!(4))),
                    Statement::Expression(infix!(
                        prefix!(Token::Minus, int!(5)),
                        Token::Asterisk,
                        int!(5),
                    )),
                ],
            ),
            (
                "5 > 4 == 3 < 4",
                vec![Statement::Expression(infix!(
                    infix!(int!(5), Token::GreaterThan, int!(4)),
                    Token::EqualTo,
                    infix!(int!(3), Token::LessThan, int!(4)),
                ))],
            ),
            (
                "5 < 4 != 3 > 4",
                vec![Statement::Expression(infix!(
                    infix!(int!(5), Token::LessThan, int!(4)),
                    Token::NotEqualTo,
                    infix!(int!(3), Token::GreaterThan, int!(4)),
                ))],
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                vec![Statement::Expression(infix!(
                    infix!(
                        int!(3),
                        Token::Plus,
                        infix!(int!(4), Token::Asterisk, int!(5)),
                    ),
                    Token::EqualTo,
                    infix!(
                        infix!(int!(3), Token::Asterisk, int!(1)),
                        Token::Plus,
                        infix!(int!(4), Token::Asterisk, int!(5)),
                    ),
                ))],
            ),
            (
                "3 > 5 == false",
                vec![Statement::Expression(infix!(
                    infix!(int!(3), Token::GreaterThan, int!(5)),
                    Token::EqualTo,
                    FALSE,
                ))],
            ),
            (
                "3 < 5 == false",
                vec![Statement::Expression(infix!(
                    infix!(int!(3), Token::LessThan, int!(5)),
                    Token::EqualTo,
                    FALSE,
                ))],
            ),
            (
                "1 + (2 + 3) + 4",
                vec![Statement::Expression(infix!(
                    infix!(int!(1), Token::Plus, infix!(int!(2), Token::Plus, int!(3))),
                    Token::Plus,
                    int!(4)
                ))],
            ),
            (
                "(5 + 5) * 2",
                vec![Statement::Expression(infix!(
                    infix!(int!(5), Token::Plus, int!(5)),
                    Token::Asterisk,
                    int!(2)
                ))],
            ),
            (
                "2 / (5 + 5)",
                vec![Statement::Expression(infix!(
                    int!(2),
                    Token::Slash,
                    infix!(int!(5), Token::Plus, int!(5))
                ))],
            ),
            (
                "-(5 + 5)",
                vec![Statement::Expression(prefix!(
                    Token::Minus,
                    infix!(int!(5), Token::Plus, int!(5))
                ))],
            ),
            (
                "!(true == true)",
                vec![Statement::Expression(prefix!(
                    Token::Bang,
                    infix!(TRUE, Token::EqualTo, TRUE)
                ))],
            ),
        ] {
            parse(input, Program { statements });
        }
    }

    #[test]
    fn boolean_expression() {
        for (input, statement) in [
            ("true;", Statement::Expression(TRUE)),
            ("false;", Statement::Expression(FALSE)),
            (
                "let foobar = true;",
                Statement::Let("foobar".to_string(), TRUE),
            ),
            (
                "let barfoo = false;",
                Statement::Let("barfoo".to_string(), FALSE),
            ),
        ] {
            parse(
                input,
                Program {
                    statements: vec![statement],
                },
            );
        }
    }

    #[test]
    fn if_expression() {
        for (input, statement) in [
            (
                "if (x < y) { x }",
                Statement::Expression(ifthenelse!(
                    infix!(id!("x"), Token::LessThan, id!("y")) => id!("x")
                )),
            ),
            (
                "if (x < y) { x } else { y }",
                Statement::Expression(ifthenelse!(
                    infix!(id!("x"), Token::LessThan, id!("y")) => id!("x") => id!("y")
                )),
            ),
            (
                "if (x < y) { 1 + 2; x } else { y }",
                Statement::Expression(ifthenelse!(
                    infix!(id!("x"), Token::LessThan, id!("y"))
                        => infix!(int!(1), Token::Plus, int!(2)), id!("x")
                        => id!("y")
                )),
            ),
        ] {
            parse(
                input,
                Program {
                    statements: vec![statement],
                },
            );
        }
    }
}
