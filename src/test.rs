use std::rc::Rc;

use crate::env::Env;
use crate::funcs::register_to;
use crate::scope::Scope;

#[test]
fn fail_test() {
    let mut env = Env::new();
    let scope = Rc::new(Scope::new());
    assert_eq!(Ok(()), register_to(&scope));

    match env.load(&scope, "./tests/fail-test.risp".into()) {
        Ok(x) => panic!("expected error but got `{}`.", x),
        Err(err) => {
            assert_eq!(r#"("assertion unsatisfied" (= 1 2))"#, err.to_string(),);
        }
    }
}

#[test]
fn self_test() {
    let mut env = Env::new();
    let scope = Rc::new(Scope::new());
    assert_eq!(Ok(()), register_to(&scope));

    match env.load(&scope, "./tests/run-tests.risp".into()) {
        Ok(_) => {}
        Err(err) => {
            env.trace.print(err);
            panic!("uncaught exception");
        }
    }
}
