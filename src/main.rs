use std::env::args;
use std::io::Write;

mod env;
mod funcs;
mod parser;
mod scope;
mod types;

#[cfg(test)]
mod test;

fn compute(
    env: &mut env::Env,
    scope: &scope::Scope,
    parser: &mut parser::Parser,
    show_prompt: bool,
) {
    loop {
        match parser.pop() {
            Some(expr) => match expr.compute(env, scope) {
                Ok(result) => {
                    if !result.is_nil() && show_prompt {
                        println!("< {}", result)
                    }
                }
                Err(err) => {
                    env.trace.print(err);
                    if !show_prompt {
                        std::process::exit(1);
                    }
                    env.trace.clear();
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

fn interpreter(mut env: env::Env, scope: scope::Scope) {
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
                    eprintln!("! {}", err);
                    if show_prompt {
                        parser.reset();
                    } else {
                        std::process::exit(1);
                    }
                }

                compute(&mut env, &scope, &mut parser, show_prompt);
            }
            Err(err) => {
                eprintln!("! {}", err);
                std::process::exit(-1);
            }
        }
    }

    if let Err(err) = parser.close() {
        eprintln!("! {}", err);
        std::process::exit(1);
    }

    compute(&mut env, &scope, &mut parser, show_prompt);
}

fn main() {
    let mut env = env::Env::new();
    let scope = scope::Scope::new();
    if let Err(err) = funcs::register_to(&scope) {
        eprintln!("failed to load embedded functions: {}", err);
        std::process::exit(-1);
    }

    let args: Vec<String> = args().collect();
    match args.len() {
        1 => interpreter(env, scope.child()),
        2 => {
            if let Err(err) = env.load(&scope, args[1].clone()) {
                eprintln!("! {}", err);
                std::process::exit(1);
            }
        }
        _ => {
            eprintln!("$ risp [file]");
        }
    }
}
