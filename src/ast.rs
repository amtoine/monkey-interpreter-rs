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

struct Program {
    statements: Vec<Statement>,
}

impl Program {
    fn token_literal(self) -> Option<String> {
        if self.statements.len() > 0 {
            Some(format!("{:?}", self.statements[0]))
        } else {
            None
        }
    }
}
