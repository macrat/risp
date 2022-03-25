use std::fs;

use crate::context::scope::Scope;
use crate::context::Context;
use crate::funcs::register_to;

fn execute_test(source: String) {
    let mut ctx = Context::new(Scope::new());
    assert_eq!(Ok(()), register_to(&mut ctx.scope().borrow_mut()));

    match ctx.load(source.into()) {
        Ok(_) => {}
        Err(err) => {
            ctx.trace().borrow().print(err);
            panic!("assertion unsatisfied");
        }
    }
}

#[test]
fn fail_test() {
    let mut ctx = Context::new(Scope::new());
    assert_eq!(Ok(()), register_to(&mut ctx.scope().borrow_mut()));

    match ctx.load("./tests/fail-test.risp".into()) {
        Ok(x) => panic!("expected error but got `{}`.", x),
        Err(err) => {
            assert_eq!(
                r#"UserError: ("assertion unsatisfied" (= 1 2))"#,
                err.to_string()
            );
        }
    }
}

#[test]
fn test() {
    let dir = match fs::read_dir("./tests/") {
        Ok(dir) => dir,
        Err(err) => panic!("{}", err),
    };

    for entry in dir {
        if let Ok(entry) = entry {
            let path = entry.path().into_os_string().into_string().unwrap();
            if path != "./tests/fail-test.risp" && path.ends_with(".risp") {
                println!("test {}", path);
                execute_test(path);
            }
        }
    }
}
