use std::io::Write;
use std::rc::Rc;

mod env;
mod funcs;
mod parser;
mod scope;
mod types;

#[cfg(test)]
mod test;

fn compute(
    env: &mut env::Env,
    scope: &Rc<scope::Scope>,
    parser: &mut parser::Parser,
    show_prompt: bool,
) {
    loop {
        env.reset_trace();

        match parser.pop() {
            Some(expr) => match expr.compute(env, scope) {
                Ok(result) if !result.is_nil() && show_prompt => println!("< {}", result),
                Ok(_) => {}
                Err(err) if show_prompt => {
                    env.trace.print(err);
                }
                Err(err) => {
                    println!("> {}", expr);
                    env.trace.print(err);
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
    let mut env = env::Env::new();
    let scope = Rc::new(scope::Scope::new());
    if let Err(err) = funcs::register_to(&scope) {
        println!("failed to load embedded functions: {}", err);
        std::process::exit(-1);
    }

    let show_prompt = atty::is(atty::Stream::Stdin);

    let mut parser = parser::Parser::new("<stdin>".into());

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
                        parser.reset();
                    } else {
                        std::process::exit(1);
                    }
                }

                compute(&mut env, &scope, &mut parser, show_prompt);
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

    compute(&mut env, &scope, &mut parser, show_prompt);
}
