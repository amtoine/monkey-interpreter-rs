#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Identifier(pub String);

#[derive(Clone, Debug, PartialEq, Default)]
pub(crate) enum Expression {
    Identifier(Identifier),
    IntegerLitteral(u32),
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
