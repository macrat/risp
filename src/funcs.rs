use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::scope::Scope;
use crate::types::*;

#[derive(Debug)]
struct Func;

impl Callable for Func {
    fn name(&self) -> &str {
        "func"
    }

    fn call(&self, args: RList, _: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        Ok(RType::Func(Rc::new(RFunc::Pure {
            args: match &args[0] {
                RType::List(list) => {
                    let mut symbols: Vec<String> = Vec::new();
                    for x in list.iter() {
                        if let RType::Atom(RAtom::Symbol(name)) = x {
                            symbols.push(String::from(name))
                        } else {
                            return Err(RError::Type(String::from(
                                "the first argument of `func` should be a list of symbol.",
                            )));
                        }
                    }
                    symbols
                }
                _ => {
                    return Err(RError::Type(String::from(
                        "first argument of `func` should be a list of symbol.",
                    )))
                }
            },
            body: RList::from(&args[1..]),
        })))
    }
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "func")
    }
}

#[derive(Debug)]
struct Def;

impl Callable for Def {
    fn name(&self) -> &str {
        "def"
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        let value = match args[1].compute(Rc::clone(&scope)) {
            Ok(x) => x,
            Err(err) => return Err(err),
        };

        if let RType::Atom(RAtom::Symbol(name)) = &args[0] {
            if let Err(err) = (*scope)
                .borrow_mut()
                .define(String::from(name), Rc::new(value.clone()))
            {
                Err(err)
            } else {
                Ok(value)
            }
        } else {
            Err(RError::Type(format!(
                "first argument of `def` should be a symbol."
            )))
        }
    }
}

#[derive(Debug)]
struct Set;

impl Callable for Set {
    fn name(&self) -> &str {
        "set"
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        let value = match args[1].compute(Rc::clone(&scope)) {
            Ok(x) => x,
            Err(err) => return Err(err),
        };

        if let RType::Atom(RAtom::Symbol(name)) = &args[0] {
            if let Err(err) = (*scope)
                .borrow_mut()
                .set(String::from(name), Rc::new(value.clone()))
            {
                Err(err)
            } else {
                Ok(value)
            }
        } else {
            Err(RError::Type(format!(
                "first argument of `set` should be a symbol."
            )))
        }
    }
}

#[derive(Debug)]
struct Println;

impl Callable for Println {
    fn name(&self) -> &str {
        "println"
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        match args.compute_each(scope) {
            Ok(x) => {
                println!("{}", x.to_bare_string());
                Ok(RType::nil())
            }
            Err(err) => Err(err),
        }
    }
}

#[derive(Debug)]
struct If;

impl Callable for If {
    fn name(&self) -> &str {
        "if"
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        if args.len() != 2 && args.len() != 3 {
            return Err(RError::Argument(format!(
                "`if` requires 2 or 3 arguments but got {} arguments.",
                args.len()
            )));
        }

        let cond = match args[0].compute(Rc::clone(&scope)) {
            Ok(val) => val.into(),
            Err(err) => return Err(err),
        };

        if cond {
            args[1].compute(Scope::new(Some(scope)))
        } else if args.len() == 3 {
            args[2].compute(Scope::new(Some(scope)))
        } else {
            Ok(RType::nil())
        }
    }
}

#[derive(Debug)]
struct List;

impl Callable for List {
    fn name(&self) -> &str {
        "list"
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        match args.compute_each(scope) {
            Ok(list) => Ok(RType::List(list)),
            Err(err) => Err(err),
        }
    }
}

#[derive(Debug)]
struct Car;

impl Callable for Car {
    fn name(&self) -> &str {
        "car"
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        if args.len() != 1 {
            Err(RError::Argument(format!(
                "`car` needs exact 1 argument but got {} arguments",
                args.len()
            )))
        } else {
            match args[0].compute(scope) {
                Ok(RType::List(list)) => Ok(list[0].clone()),
                Ok(x) => Err(RError::Type(format!("`car` needs list but got {}", x))),
                Err(err) => Err(err),
            }
        }
    }
}

#[derive(Debug)]
struct Cdr;

impl Callable for Cdr {
    fn name(&self) -> &str {
        "cdr"
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        if args.len() != 1 {
            Err(RError::Argument(format!(
                "`cdr` needs exact 1 argument but got {} arguments",
                args.len()
            )))
        } else {
            match args[0].compute(scope) {
                Ok(RType::List(list)) => Ok(RType::List(RList::from(&list[1..]))),
                Ok(x) => Err(RError::Type(format!("`cdr` needs list but got {}", x))),
                Err(err) => Err(err),
            }
        }
    }
}

#[derive(Debug)]
struct CalculateOperator<'a>(&'a str, fn(Vec<RType>) -> Result<RType, RError>);

impl Callable for CalculateOperator<'_> {
    fn name(&self) -> &str {
        self.0
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        let mut xs: Vec<RType> = Vec::new();
        for x in args.iter() {
            match x.compute(Rc::clone(&scope)) {
                Ok(x) => xs.push(x),
                Err(err) => return Err(err),
            }
        }
        self.1(xs)
    }
}

struct CompareOperator<'a>(&'a str, fn(&RType, &RType) -> bool);

impl Callable for CompareOperator<'_> {
    fn name(&self) -> &str {
        self.0
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        if args.len() < 2 {
            return Err(RError::Argument(format!(
                "`{}` needs at least 2 values.",
                self.0
            )));
        }

        let mut x = match args[0].compute(Rc::clone(&scope)) {
            Ok(x) => x,
            Err(err) => return Err(err),
        };

        for y in &args[1..] {
            match y.compute(Rc::clone(&scope)) {
                Ok(y) => {
                    if !self.1(&x, &y) {
                        return Ok(RType::Atom(RAtom::Int(0)));
                    }
                    x = y;
                }
                Err(err) => return Err(err),
            }
        }

        Ok(RType::Atom(RAtom::Int(1)))
    }
}

macro_rules! binary_func {
    ($func:expr) => {
        Rc::new(RType::Func(Rc::new(RFunc::Binary(Box::new($func)))))
    };
}

macro_rules! register {
    ($scope:expr, $name:expr, $value:expr) => {
        if let Err(err) = $scope.define(String::from($name), $value) {
            return Err(err);
        }
    };
}

pub fn register_to(scope: &mut Scope) -> Result<(), RError> {
    register!(scope, "true", Rc::new(RType::Atom(RAtom::Int(1))));
    register!(scope, "false", Rc::new(RType::Atom(RAtom::Int(0))));
    register!(scope, "nil", Rc::new(RType::nil()));

    register!(scope, "def", binary_func!(Def));
    register!(scope, "set", binary_func!(Set));

    register!(scope, "func", binary_func!(Func));
    register!(scope, "println", binary_func!(Println));

    register!(scope, "if", binary_func!(If));

    register!(scope, "list", binary_func!(List));
    register!(scope, "car", binary_func!(Car));
    register!(scope, "cdr", binary_func!(Cdr));

    register!(
        scope,
        "=",
        binary_func!(CompareOperator("=", |x, y| *x == *y))
    );
    register!(
        scope,
        "!=",
        binary_func!(CompareOperator("!=", |x, y| *x != *y))
    );

    register!(
        scope,
        "+",
        binary_func!(CalculateOperator("+", |xs| {
            if xs.len() == 0 {
                return Err(RError::Argument(String::from(
                    "`+` needs at least 1 value.",
                )));
            }

            if let RType::Atom(RAtom::String(_)) = xs[0] {
                let mut result = String::new();
                for x in xs {
                    if let RType::Atom(RAtom::String(x)) = x {
                        result += &x;
                    } else {
                        return Err(RError::Type(format!("`+` can not apply to `{}`", x)));
                    }
                }
                Ok(RType::Atom(RAtom::String(result)))
            } else {
                let mut result = 0;
                for x in xs {
                    if let RType::Atom(RAtom::Int(x)) = x {
                        result += x;
                    } else {
                        return Err(RError::Type(format!("`+` can not apply to `{}`", x)));
                    }
                }
                Ok(RType::Atom(RAtom::Int(result)))
            }
        }))
    );

    register!(
        scope,
        "-",
        binary_func!(CalculateOperator("-", |xs| {
            match xs.len() {
                0 => Err(RError::Argument(String::from(
                    "`-` needs at least 1 value.",
                ))),
                1 => {
                    if let RType::Atom(RAtom::Int(x)) = xs[0] {
                        Ok(RType::Atom(RAtom::Int(-x)))
                    } else {
                        Err(RError::Type(format!("`-` can not apply to `{}`", xs[0])))
                    }
                }
                _ => {
                    let mut result = if let RType::Atom(RAtom::Int(x)) = xs[0] {
                        x
                    } else {
                        return Err(RError::Type(format!("`-` can not apply to `{}`", xs[0])));
                    };

                    for x in &xs[1..] {
                        if let RType::Atom(RAtom::Int(x)) = x {
                            result -= *x;
                        } else {
                            return Err(RError::Type(format!("`-` can not apply to `{}`", x)));
                        }
                    }

                    Ok(RType::Atom(RAtom::Int(result)))
                }
            }
        }))
    );

    register!(
        scope,
        "*",
        binary_func!(CalculateOperator("*", |xs| {
            if xs.len() < 2 {
                return Err(RError::Argument(String::from(
                    "`*` needs at least 2 values.",
                )));
            }

            let mut result = 1;
            for x in xs {
                if let RType::Atom(RAtom::Int(x)) = x {
                    result *= x;
                } else {
                    return Err(RError::Type(format!("`*` can not apply to `{}`", x)));
                }
            }
            Ok(RType::Atom(RAtom::Int(result)))
        }))
    );

    register!(
        scope,
        "/",
        binary_func!(CalculateOperator("/", |xs| {
            if xs.len() < 2 {
                return Err(RError::Argument(String::from(
                    "`/` needs at least 2 values.",
                )));
            }

            let mut result = if let RType::Atom(RAtom::Int(x)) = xs[0] {
                x
            } else {
                return Err(RError::Type(format!("`/` can not apply to `{}`", xs[0])));
            };

            for x in &xs[1..] {
                if let RType::Atom(RAtom::Int(x)) = x {
                    result /= *x;
                } else {
                    return Err(RError::Type(format!("`/` can not apply to `{}`", x)));
                }
            }

            Ok(RType::Atom(RAtom::Int(result)))
        }))
    );

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::parse;

    fn execute(code: &str) -> Result<RType, RError> {
        let scope = Scope::new(None);
        assert_eq!(Ok(()), register_to(&mut (*scope).borrow_mut()));

        return match parse(code) {
            Ok(x) => x.compute_last(scope),
            Err(err) => Err(err),
        };
    }

    fn assert_atom(expect: RAtom, code: &str) {
        match execute(code) {
            Ok(RType::Atom(x)) => assert_eq!(expect, x),
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

    #[test]
    fn plus() {
        assert_err(
            RError::Argument(String::from("`+` needs at least 1 value.")),
            "(+)",
        );

        assert_atom(RAtom::Int(1), "(+ 1)");
        assert_atom(RAtom::Int(3), "(+ 1 2)");
        assert_atom(RAtom::Int(15), "(+ 1 2 3 4 5)");
        assert_atom(RAtom::Int(15), "(+ 1 (+ 2 3) 4 5)");

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
            RError::Type(String::from("`+` can not apply to `123`")),
            r#"(+ "hello" 123)"#,
        );
        assert_err(
            RError::Type(String::from("`+` can not apply to `hello`")),
            r#"(+ 123 "hello")"#,
        );
    }

    #[test]
    fn minus() {
        assert_err(
            RError::Argument(String::from("`-` needs at least 1 value.")),
            "(-)",
        );
        assert_atom(RAtom::Int(-1), "(- 1)");
        assert_atom(RAtom::Int(-1), "(- 1 2)");
        assert_atom(RAtom::Int(-13), "(- 1 2 3 4 5)");
        assert_atom(RAtom::Int(-7), "(- 1 (- 2 3) 4 5)");
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
        assert_atom(RAtom::Int(2), "(* 1 2)");
        assert_atom(RAtom::Int(120), "(* 1 2 3 4 5)");
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
        assert_atom(RAtom::Int(2), "(/ 4 2)");
        assert_atom(RAtom::Int(5), "(/ 20 2 2)");
    }

    #[test]
    fn def_and_set() {
        assert_atom(RAtom::Int(42), "(def x 42) x");
        assert_atom(RAtom::Int(3), "(def x (+ 1 2)) x");

        assert_err(
            RError::AlreadyExist(String::from("x")),
            "(def x 1) (def x 2)",
        );
        assert_atom(RAtom::Int(2), "(def x 1) (set x 2) x");
        assert_err(RError::NotExist(String::from("x")), "(set x 3) x");
    }

    #[test]
    fn func() {
        assert_atom(
            RAtom::Int(120),
            r"
                (def f (func (x)
                  (def loop (func (n)
                    (if (- n 1) ((func ()
                      (set x (* x n))
                      (loop (- n 1))))
                      x)))
                  (loop (- x 1))))
                (f 5)
            ",
        )
    }

    #[test]
    fn list_car_cdr() {
        assert_atom(RAtom::Int(123), "(car (list 123 456))");
        assert_atom(RAtom::Int(456), "(car (cdr (list 123 456)))");

        assert_atom(
            RAtom::Int(12),
            r"
                (def xs (list 12 34))
                (def ys xs)
                (car ys)
            ",
        );
    }

    #[test]
    fn if_() {
        assert_atom(
            RAtom::Int(1),
            r"
                (def result 0)
                (if true
                  (set result 1)
                  (set result 2))
                result
            ",
        );

        assert_atom(
            RAtom::Int(2),
            r"
                (def result 0)
                (if false
                  (set result 1)
                  (set result 2))
                result
            ",
        );

        assert_atom(
            RAtom::Int(2),
            r"
                (def result 0)
                (if nil
                  (set result 1)
                  (set result 2))
                result
            ",
        );
    }

    #[test]
    fn compare() {
        assert_atom(RAtom::Int(1), "(= 1 1)");
        assert_atom(RAtom::Int(0), "(= 1 2)");
        assert_atom(RAtom::Int(1), "(= 2 2)");

        assert_atom(RAtom::Int(1), "(= (list 1 2 3) (list 1 2 3))");
        assert_atom(RAtom::Int(0), "(= (list 1 2 4) (list 1 2 3))");
        assert_atom(RAtom::Int(0), "(= (list 1 2 3 4) (list 1 2 3))");
        assert_atom(RAtom::Int(1), "(= (list) (list))");

        assert_atom(RAtom::Int(1), "(= println println)");
        assert_atom(RAtom::Int(0), "(= if println)");

        assert_atom(RAtom::Int(1), "(def f (func ())) (= f f)");
        assert_atom(RAtom::Int(0), "(def f (func ())) (= f (func ()))");

        assert_atom(RAtom::Int(0), "(!= 1 1)");
        assert_atom(RAtom::Int(1), "(!= 1 2)");
        assert_atom(RAtom::Int(0), "(!= if if)");
        assert_atom(RAtom::Int(1), "(!= if println)");
    }
}
