use std::io::Write;

mod ast;
mod lexer;
mod parser;
mod token;

fn main() {
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();

        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();

        let mut parser = parser::Parser::new(lexer::Lexer::new(input.to_string()));
        let ast = parser.parse();
        println!("{:#?}", ast);

        if parser.errors.len() > 0 {
            eprintln!("ERRORS: {:#?}", parser.errors);
        }
    }
}
