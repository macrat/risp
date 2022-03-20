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
        (def double (func (x) (* x 2)))

        (def doubled (double 4))
        (println doubled)

        (set doubled (double doubled))
        (println doubled)

        (def countdown (func (x)
          (println x)
          (if x (countdown (- x 1)))))
        (countdown 5)
    ";

    match parser::parse(input) {
        Ok(xs) => {
            for expr in xs.iter() {
                println!("< {}", expr);
                match expr.compute(Rc::clone(&scope)) {
                    Ok(types::RType::List(list)) if list.len() == 0 => println!(""),
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
