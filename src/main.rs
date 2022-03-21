use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

mod funcs;
mod parser;
mod scope;
mod types;

fn compute(scope: &Rc<RefCell<scope::Scope>>, parser: &mut parser::Parser, show_prompt: bool) {
    loop {
        match parser.pop() {
            Some(expr) => match expr.compute(Rc::clone(scope)) {
                Ok(result) if !result.is_nil() && show_prompt => println!("< {}", result),
                Ok(_) => {}
                Err(err) if show_prompt => {
                    println!("! {}", err);
                }
                Err(err) => {
                    println!("> {}", expr);
                    println!("! {}", err);
                    std::process::exit(1);
                }
            },
            None => {
                if show_prompt && parser.is_completed() {
                    println!();
                }
                break;
            }
        }
    }
}

fn main() {
    let scope = scope::Scope::new(None);
    if let Err(err) = funcs::register_to(&mut (*scope).borrow_mut()) {
        println!("failed to load embedded functions: {}", err);
        std::process::exit(-1);
    }

    let show_prompt = atty::is(atty::Stream::Stdin);

    let mut parser = parser::Parser::new();

    let stdin = std::io::stdin();
    let mut input = String::new();
    loop {
        if show_prompt {
            if parser.is_completed() {
                print!("> ");
            } else {
                print!("| ");
            }
        }
        let _ = std::io::stdout().flush();

        input.clear();
        match stdin.read_line(&mut input) {
            Ok(0) => {
                if show_prompt {
                    println!("");
                }
                break;
            }
            Ok(_) => {
                if let Err(err) = parser.feed(input.as_str()) {
                    println!("! {}", err);
                    if show_prompt {
                        parser = parser::Parser::new();
                    } else {
                        std::process::exit(1);
                    }
                }

                compute(&scope, &mut parser, show_prompt);
            }
            Err(err) => {
                println!("! {}", err);
                std::process::exit(-1);
            }
        }
    }

    if let Err(err) = parser.close() {
        println!("! {}", err);
        std::process::exit(1);
    }

    compute(&scope, &mut parser, show_prompt);
}
