use std::io::Write;

mod ast;
mod lexer;
mod token;

fn main() {
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();

        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();

        let mut lexer = lexer::Lexer::new(input);
        loop {
            match lexer.next_token() {
                token::Token::EndOfFile => break,
                token => println!("{:?}", token),
            }
        }
    }
}
