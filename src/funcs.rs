use std::rc::Rc;

use crate::scope::Scope;
use crate::types::*;

mod flow;
mod func;
mod io;
mod list;
mod operator;
mod types;
mod variable;

macro_rules! binary_func {
    ($func:expr) => {
        RValue::Func(Rc::new(RFunc::Binary(Box::new($func))))
    };
}

macro_rules! register {
    ($scope:expr, $name:expr, $value:expr) => {
        $scope.define(String::from($name), $value)?;
    };
}

pub fn register_to(scope: &Scope) -> Result<(), RError> {
    // variable
    register!(scope, "def", binary_func!(variable::Def));
    register!(scope, "set", binary_func!(variable::Set));

    // flow
    register!(scope, "if", binary_func!(flow::If));
    register!(scope, "while", binary_func!(flow::While));
    register!(scope, "do", binary_func!(flow::Do));
    register!(scope, "panic!", binary_func!(flow::Panic));

    // function
    register!(scope, "func", binary_func!(func::Func));

    // list
    register!(scope, "list", binary_func!(list::List));
    register!(scope, "car", binary_func!(list::Car));
    register!(scope, "cdr", binary_func!(list::Cdr));
    register!(scope, "seq", binary_func!(list::Seq));
    register!(scope, "map", binary_func!(list::Map));
    register!(scope, "fold", binary_func!(list::Fold));

    // io
    register!(scope, "import", binary_func!(io::Import));
    register!(scope, "print", binary_func!(io::PRINT));
    register!(scope, "println", binary_func!(io::PRINTLN));

    // type
    register!(scope, "is-number", binary_func!(types::IS_NUMBER));
    register!(scope, "is-string", binary_func!(types::IS_STRING));
    register!(scope, "is-list", binary_func!(types::IS_LIST));
    register!(scope, "is-func", binary_func!(types::IS_FUNC));

    // operator
    register!(scope, "+", binary_func!(operator::ADD));
    register!(scope, "-", binary_func!(operator::SUB));
    register!(scope, "*", binary_func!(operator::MULTIPLY));
    register!(scope, "/", binary_func!(operator::DIVIDE));

    register!(scope, "=", binary_func!(operator::EQ));
    register!(scope, "!=", binary_func!(operator::NE));
    register!(scope, "<", binary_func!(operator::LT));
    register!(scope, ">", binary_func!(operator::GT));
    register!(scope, "<=", binary_func!(operator::LE));
    register!(scope, ">=", binary_func!(operator::GE));

    register!(scope, "not", binary_func!(operator::Not));

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::env::Env;
    use crate::parser::test::parse;

    fn execute(code: &str) -> Result<RValue, RError> {
        let mut env = Env::new();
        let scope = Scope::new();
        assert_eq!(Ok(()), register_to(&scope));

        return match parse(code) {
            Ok(x) => x.compute_last(&mut env, &scope),
            Err(err) => Err(err),
        };
    }

    fn assert_atom(expect: RAtom, code: &str) {
        match execute(code) {
            Ok(RValue::Atom(x)) => assert_eq!(expect, x),
            Ok(x) => panic!("expected {} but got {}", expect, x),
            Err(err) => panic!("{}", err),
        }
    }

    fn assert_err(expect: RError, code: &str) {
        match execute(code) {
            Err(err) => assert_eq!(expect, err),
            Ok(x) => panic!("expected error but got {}", x),
        }
    }

    #[cfg(test)]
    mod calc {
        use super::*;

        #[test]
        fn plus() {
            assert_err(
                RError::Argument(String::from("`+` needs at least 1 value.")),
                "(+)",
            );

            assert_atom(RAtom::Number(1.0), "(+ 1)");
            assert_atom(RAtom::Number(3.0), "(+ 1 2)");
            assert_atom(RAtom::Number(15.0), "(+ 1 2 3 4 5)");
            assert_atom(RAtom::Number(15.0), "(+ 1 (+ 2 3) 4 5)");

            assert_atom(RAtom::String(String::from("hello")), r#"(+ "hello")"#);
            assert_atom(
                RAtom::String(String::from("helloworld")),
                r#"(+ "hello" "world")"#,
            );
            assert_atom(
                RAtom::String(String::from("hello world")),
                r#"(+ "hello" " " "world")"#,
            );

            assert_err(
                RError::Type(String::from("`+` can not apply to 123")),
                r#"(+ "hello" 123)"#,
            );
            assert_err(
                RError::Type(String::from("`+` can not apply to \"hello\"")),
                r#"(+ 123 "hello")"#,
            );
        }

        #[test]
        fn minus() {
            assert_err(
                RError::Argument(String::from("`-` needs at least 1 value.")),
                "(-)",
            );
            assert_atom(RAtom::Number(-1.0), "(- 1)");
            assert_atom(RAtom::Number(-1.0), "(- 1 2)");
            assert_atom(RAtom::Number(-13.0), "(- 1 2 3 4 5)");
            assert_atom(RAtom::Number(-7.0), "(- 1 (- 2 3) 4 5)");
        }

        #[test]
        fn multiply() {
            assert_err(
                RError::Argument(String::from("`*` needs at least 2 values.")),
                "(*)",
            );
            assert_err(
                RError::Argument(String::from("`*` needs at least 2 values.")),
                "(* 1)",
            );
            assert_atom(RAtom::Number(2.0), "(* 1 2)");
            assert_atom(RAtom::Number(120.0), "(* 1 2 3 4 5)");
        }

        #[test]
        fn divide() {
            assert_err(
                RError::Argument(String::from("`/` needs at least 2 values.")),
                "(/)",
            );
            assert_err(
                RError::Argument(String::from("`/` needs at least 2 values.")),
                "(/ 1)",
            );
            assert_atom(RAtom::Number(2.5), "(/ 5 2)");
            assert_atom(RAtom::Number(5.0), "(/ 20 2 2)");
        }
    }

    #[test]
    fn def_and_set() {
        assert_atom(RAtom::Number(42.0), "(def x 42) x");
        assert_atom(RAtom::Number(3.0), "(def x (+ 1 2)) x");

        assert_err(
            RError::AlreadyExist(String::from("x")),
            "(def x 1) (def x 2)",
        );
        assert_atom(RAtom::Number(2.0), "(def x 1) (set x 2) x");
        assert_err(RError::NotExist(String::from("x")), "(set x 3) x");
    }

    #[test]
    fn panic() {
        assert_err(
            RError::User(RValue::Atom(RAtom::String(String::from("hello world")))),
            r#"(panic! "hello world")"#,
        );

        assert_err(
            RError::User(RValue::Atom(RAtom::Number(123.0))),
            r#"(panic! 123)"#,
        );

        assert_err(RError::User(RValue::nil()), "(panic!)");
    }
}
