use crate::token::Token;

#[derive(Clone, Debug, PartialEq, Default)]
pub(crate) enum Expression {
    Identifier(String),
    IntegerLitteral(u32),
    Prefix(Token, Box<Self>),
    Infix(Box<Self>, Token, Box<Self>),
    Boolean(bool),
    If(Box<Self>, Vec<Statement>, Vec<Statement>),
    Function(Vec<String>, Vec<Statement>),
    #[default]
    Dummy,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Statement {
    Let(String, Expression),
    Return(Expression),
    Expression(Expression),
}

#[derive(Default, Debug, PartialEq)]
pub(crate) struct Program {
    pub statements: Vec<Statement>,
}
