mod ast;
mod error_utils;
mod evaluator;
mod lexer;
mod parser;
mod prattutil;
mod token;
fn main() {
    // arg
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <input>", args[0]);
        std::process::exit(1);
    }

    let input = &args[1];
    let mut lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(lexer);
    let program = parser.parse_program();
    let mut evaluator = evaluator::Evaluator::new();
    let result = evaluator.eval(&program);
    match result {
        Ok(value) => println!("Result: {:?}", value),
        Err(err) => eprintln!("Error: {}", err),
    }
}
