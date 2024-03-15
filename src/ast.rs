pub(crate) enum Statement {
    Let(Identifier, Expression),
}

pub(crate) enum Expression {}


#[derive(Debug)]
pub(crate) struct Identifier {
    pub value: String,
}

pub(crate) struct Program {
    pub statements: Vec<Statement>,
}
