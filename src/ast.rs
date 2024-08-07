#[derive(Clone, Debug, PartialEq)]
struct Identifier(String);

#[derive(Clone, Debug, PartialEq)]
enum Expression {
    Identifier(Identifier)
}

#[derive(Clone, Debug, PartialEq)]
enum Statement {
    Let(Identifier, Expression),
}

#[derive(Default)]
pub(crate) struct Program {
    statements: Vec<Statement>,
}
