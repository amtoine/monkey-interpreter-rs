#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Identifier(pub String);

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Expression {
    Identifier(Identifier),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
}

#[derive(Default)]
pub(crate) struct Program {
    pub statements: Vec<Statement>,
}
