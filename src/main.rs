use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

mod funcs;
mod parser;
mod scope;
mod types;

fn compute(scope: &Rc<RefCell<scope::Scope>>, parser: &mut parser::Parser) {
    loop {
        match parser.pop() {
            Some(expr) => match expr.compute(Rc::clone(scope)) {
                Ok(result) if result.is_nil() => println!(""),
                Ok(result) => println!("> {}\n", result),
                Err(err) => {
                    println!("! {}\n", err);
                    std::process::exit(1);
                }
            },
            None => break,
        }
    }
}

fn main() {
    let scope = scope::Scope::new(None);
    if let Err(err) = funcs::register_to(&mut (*scope).borrow_mut()) {
        println!("failed to load embedded functions: {}", err);
        std::process::exit(-1);
    }

    let mut parser = parser::Parser::new();

    let stdin = std::io::stdin();
    let mut input = String::new();
    loop {
        if parser.is_completed() {
            print!("< ");
        } else {
            print!("| ");
        }
        let _ = std::io::stdout().flush();

        input.clear();
        match stdin.read_line(&mut input) {
            Ok(0) => break,
            Ok(_) => {
                if let Err(err) = parser.feed(input.as_str()) {
                    println!("! {}", err);
                    std::process::exit(1);
                }

                compute(&scope, &mut parser);
            }
            Err(err) => {
                println!("! {}", err);
                std::process::exit(-1);
            }
        }
    }

    if let Err(err) = parser.flush() {
        println!("! {}", err);
        std::process::exit(1);
    }

    compute(&scope, &mut parser);
}
