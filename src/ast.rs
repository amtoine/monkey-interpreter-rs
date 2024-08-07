use crate::token::Token;

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Identifier(pub String);

#[derive(Clone, Debug, PartialEq, Default)]
pub(crate) enum Expression {
    Identifier(Identifier),
    IntegerLitteral(u32),
    Prefix(Token, Box<Expression>),
    #[default]
    Dummy,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
    Expression(Expression),
}

#[derive(Default, Debug, PartialEq)]
pub(crate) struct Program {
    pub statements: Vec<Statement>,
}
