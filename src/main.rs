use std::rc::Rc;

mod funcs;
mod parser;
mod scope;
mod types;

fn main() {
    let scope = scope::Scope::new(None);
    if let Err(err) = funcs::register_to(&mut (*scope).borrow_mut()) {
        println!("failed to load embedded functions: {}", err);
        std::process::exit(-1);
    }

    let input = r"
        (def f (func (x)
          ((def loop (func (n)
            (set x (* x n))
            (if (= n 1)
              x
              (loop (- n 1)))))
            (- x 1))))

        (println (f 5))
    ";

    match parser::parse(input) {
        Ok(xs) => {
            for expr in xs.iter() {
                println!("< {}", expr);
                match expr.compute(Rc::clone(&scope)) {
                    Ok(result) if result.is_nil() => println!(""),
                    Ok(result) => println!("> {}\n", result),
                    Err(err) => println!("! {}\n", err),
                }
            }
        }
        Err(err) => {
            println!("{}", err);
            std::process::exit(-1);
        }
    }
}
