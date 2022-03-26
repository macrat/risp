use std::fs;
use std::rc::Rc;

use crate::env::Env;
use crate::funcs::register_to;
use crate::scope::Scope;

fn execute_test(source: String) {
    let mut env = Env::new();
    let scope = Rc::new(Scope::new());
    assert_eq!(Ok(()), register_to(&scope));

    match env.load(&scope, source.into()) {
        Ok(_) => {}
        Err(err) => {
            env.trace.print(err);
            panic!("assertion unsatisfied");
        }
    }
}

#[test]
fn fail_test() {
    let mut env = Env::new();
    let scope = Rc::new(Scope::new());
    assert_eq!(Ok(()), register_to(&scope));

    match env.load(&scope, "./tests/fail-test.risp".into()) {
        Ok(x) => panic!("expected error but got `{}`.", x),
        Err(err) => {
            assert_eq!(
                r#"UserError: ("assertion unsatisfied" (= 1 2))"#,
                err.to_string(),
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
