#[derive(Debug, PartialEq)]
pub(crate) enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
}

#[derive(Debug, PartialEq)]
pub(crate) enum Expression {
    Dummy,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Identifier {
    pub value: String,
}

pub(crate) struct Program {
    pub statements: Vec<Statement>,
}
