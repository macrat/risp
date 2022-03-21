use std::cell::RefCell;
use std::cmp::Ordering;
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

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
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
            capture: Rc::clone(&scope),
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
struct Print<'a>(&'a str, &'a str);

impl Callable for Print<'_> {
    fn name(&self) -> &str {
        self.0
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        match args.compute_each(scope) {
            Ok(x) => {
                print!("{}{}", x.to_bare_string(), self.1);
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
            Ok(val) => val.as_bool(),
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
struct Do;

impl Callable for Do {
    fn name(&self) -> &str {
        "do"
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        args.compute_last(Scope::new(Some(scope)))
    }
}

#[derive(Debug)]
struct While;

impl Callable for While {
    fn name(&self) -> &str {
        "while"
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        if args.len() == 0 {
            return Err(RError::Argument(format!(
                "`while` needs 1 or more arguments but got {}.",
                args
            )));
        }

        let scope = Scope::new(Some(scope));
        let mut result = RType::nil();

        loop {
            let cond = match args[0].compute(Rc::clone(&scope)) {
                Ok(val) => val,
                Err(err) => return Err(err),
            };
            if !cond.as_bool() {
                break;
            }

            result = match RList::from(&args[1..]).compute_last(Rc::clone(&scope)) {
                Ok(val) => val,
                Err(err) => return Err(err),
            }
        }

        Ok(result)
    }
}

#[derive(Debug)]
struct Map;

impl Callable for Map {
    fn name(&self) -> &str {
        "map"
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        if args.len() != 2 {
            return Err(RError::Argument(format!(
                "`map` needs exact 2 arguments but got {}.",
                args
            )));
        }

        let list = match args[0].compute(Rc::clone(&scope)) {
            Ok(RType::List(xs)) => xs,
            Ok(x) => {
                return Err(RError::Type(format!(
                    "the first argument of `map` must be a list, but got `{}`.",
                    x
                )))
            }
            Err(err) => return Err(err),
        };

        let func = match args[1].compute(Rc::clone(&scope)) {
            Ok(RType::Func(f)) => f,
            Ok(x) => {
                return Err(RError::Type(format!(
                    "the second argument of `map` must be a function, but got `{}`.",
                    x
                )))
            }
            Err(err) => return Err(err),
        };

        let mut result = RList::empty();

        for x in list.iter() {
            match func.call(RList::new(vec![x.clone()]), Rc::clone(&scope)) {
                Ok(x) => result.push(x),
                Err(err) => return Err(err),
            }
        }

        Ok(RType::List(result))
    }
}

#[derive(Debug)]
struct Fold;

impl Callable for Fold {
    fn name(&self) -> &str {
        "fold"
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        if args.len() != 2 {
            return Err(RError::Argument(format!(
                "`fold` needs exact 2 arguments but got {}.",
                args
            )));
        }

        let list = match args[0].compute(Rc::clone(&scope)) {
            Ok(RType::List(xs)) if xs.len() >= 2 => xs,
            Ok(RType::List(xs)) => {
                return Err(RError::Type(format!(
                    "the first argument of `fold` must be longer than 2, but got `{}`.",
                    xs
                )))
            }
            Ok(x) => {
                return Err(RError::Type(format!(
                    "the first argument of `fold` must be a list, but got `{}`.",
                    x
                )))
            }
            Err(err) => return Err(err),
        };

        let func = match args[1].compute(Rc::clone(&scope)) {
            Ok(RType::Func(f)) => f,
            Ok(x) => {
                return Err(RError::Type(format!(
                    "the second argument of `fold` must be a function, but got `{}`.",
                    x
                )))
            }
            Err(err) => return Err(err),
        };

        let mut result = list[0].clone();

        for x in &list[1..] {
            match func.call(RList::new(vec![result, x.clone()]), Rc::clone(&scope)) {
                Ok(x) => result = x,
                Err(err) => return Err(err),
            }
        }

        Ok(result)
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
struct Seq;

impl Callable for Seq {
    fn name(&self) -> &str {
        "seq"
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        if args.len() != 1 && args.len() != 2 {
            return Err(RError::Argument(format!(
                "`seq` needs 1 or 2 int argument but got {}",
                args
            )));
        }

        let args = match args.compute_each(scope) {
            Ok(xs) => xs,
            Err(err) => return Err(err),
        };

        let to = match &args[args.len() - 1] {
            RType::Atom(RAtom::Int(n)) => *n,
            x => {
                return Err(RError::Argument(format!(
                    "`seq` needs 1 or 2 int argument but got `{}`",
                    x
                )))
            }
        };
        if args.len() == 1 && to == 0 {
            return Ok(RType::nil());
        }

        let from = if args.len() == 2 {
            match &args[0] {
                RType::Atom(RAtom::Int(n)) => *n,
                x => {
                    return Err(RError::Argument(format!(
                        "`seq` needs 1 or 2 int argument but got `{}`",
                        x
                    )))
                }
            }
        } else if to > 0 {
            1
        } else {
            -1
        };

        let step = if from < to { 1 } else { -1 };

        let mut result = RList::empty();
        let mut i = from;
        while i != to {
            result.push(RType::Atom(RAtom::Int(i)));
            i += step;
        }
        result.push(RType::Atom(RAtom::Int(i)));
        Ok(RType::List(result))
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
                Ok(RType::Atom(RAtom::String(x))) => match x.chars().next() {
                    Some(c) => Ok(RType::Atom(RAtom::String(c.into()))),
                    None => Ok(RType::Atom(RAtom::String(String::new()))),
                },
                Ok(x) => Err(RError::Type(format!(
                    "`car` needs list or string but got {}",
                    x
                ))),
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
                Ok(RType::Atom(RAtom::String(x))) => {
                    let mut chars = x.chars();
                    chars.next();
                    Ok(RType::Atom(RAtom::String(chars.as_str().into())))
                }
                Ok(x) => Err(RError::Type(format!(
                    "`cdr` needs list or string but got {}",
                    x
                ))),
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

struct CompareOperator<'a>(&'a str, fn(&RType, &RType) -> Result<bool, RError>);

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
                    match self.1(&x, &y) {
                        Ok(false) => return Ok(RType::Atom(RAtom::Int(0))),
                        Err(err) => return Err(err),
                        _ => {}
                    }
                    x = y;
                }
                Err(err) => return Err(err),
            }
        }

        Ok(RType::Atom(RAtom::Int(1)))
    }
}

macro_rules! ordering_rule {
    ($order:expr, true) => {
        |x, y| match x.cmp(y) {
            Ok(order) => Ok(order == $order),
            Err(err) => Err(err),
        }
    };
    ($order:expr, false) => {
        |x, y| match x.cmp(y) {
            Ok(order) => Ok(order != $order),
            Err(err) => Err(err),
        }
    };
}

struct TypeCheckOperator<'a>(&'a str, fn(&RType) -> bool);

impl Callable for TypeCheckOperator<'_> {
    fn name(&self) -> &str {
        self.0
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        for x in args.iter() {
            match x.compute(Rc::clone(&scope)) {
                Ok(x) if self.1(&x) => {
                    return Ok(RType::Atom(RAtom::Int(1)));
                }
                Err(err) => return Err(err),
                _ => {}
            }
        }
        Ok(RType::Atom(RAtom::Int(0)))
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
    register!(scope, "def", binary_func!(Def));
    register!(scope, "set", binary_func!(Set));

    register!(scope, "func", binary_func!(Func));

    register!(scope, "list", binary_func!(List));
    register!(scope, "car", binary_func!(Car));
    register!(scope, "cdr", binary_func!(Cdr));
    register!(scope, "seq", binary_func!(Seq));

    register!(scope, "print", binary_func!(Print("println", "")));
    register!(scope, "println", binary_func!(Print("println", "\n")));

    register!(scope, "if", binary_func!(If));
    register!(scope, "do", binary_func!(Do));
    register!(scope, "while", binary_func!(While));
    register!(scope, "map", binary_func!(Map));
    register!(scope, "fold", binary_func!(Fold));

    register!(
        scope,
        "is-int",
        binary_func!(TypeCheckOperator("is-int", |x| match x {
            RType::Atom(RAtom::Int(_)) => true,
            _ => false,
        }))
    );
    register!(
        scope,
        "is-string",
        binary_func!(TypeCheckOperator("is-string", |x| match x {
            RType::Atom(RAtom::String(_)) => true,
            _ => false,
        }))
    );
    register!(
        scope,
        "is-list",
        binary_func!(TypeCheckOperator("is-list", |x| match x {
            RType::List(_) => true,
            _ => false,
        }))
    );
    register!(
        scope,
        "is-func",
        binary_func!(TypeCheckOperator("is-func", |x| match x {
            RType::Func(_) => true,
            _ => false,
        }))
    );

    register!(
        scope,
        "=",
        binary_func!(CompareOperator("=", |x, y| Ok(*x == *y)))
    );
    register!(
        scope,
        "!=",
        binary_func!(CompareOperator("!=", |x, y| Ok(*x != *y)))
    );
    register!(
        scope,
        "<",
        binary_func!(CompareOperator("<", ordering_rule!(Ordering::Less, true)))
    );
    register!(
        scope,
        ">",
        binary_func!(CompareOperator(
            ">",
            ordering_rule!(Ordering::Greater, true)
        ))
    );
    register!(
        scope,
        "<=",
        binary_func!(CompareOperator(
            "<=",
            ordering_rule!(Ordering::Greater, false)
        ))
    );
    register!(
        scope,
        ">=",
        binary_func!(CompareOperator(">=", ordering_rule!(Ordering::Less, false)))
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
    use crate::parser::test::parse;

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

    #[cfg(test)]
    mod calc {
        use super::*;

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
                  (def l (func (n)
                    (if (> n 1) (do
                      (set x (* x n))
                      (l (- n 1))))
                      x))
                  (l (- x 1))))
                (f 5)
            ",
        );

        assert_atom(
            RAtom::Int(120),
            r"
                (def f (func (x)
                  (def n x)
                  (while (> n 1)
                    (set n (- n 1))
                    (set x (* x n)))))

                (f 5)
            ",
        );

        assert_atom(
            RAtom::Int(120),
            r"
                (def f (func (x)
                  (map (seq (- x 1))
                    (func (n) (set x (* x n))))
                  x))

                (f 5)
            ",
        );

        assert_atom(
            RAtom::Int(120),
            r"
                (def f (func (x)
                  (fold (seq x) *)))

                (f 5)
            ",
        );

        assert_atom(
            RAtom::Int(1),
            r#"
                (def counter (do
                  (def x 0)
                  (func ()
                    (set x (+ x 1))
                    x)))

                (def xs (list
                  (counter)
                  (counter)
                  (counter)))

                (println "xs =" xs)
                (= xs (list 1 2 3))
            "#,
        );
    }

    #[test]
    fn seq() {
        assert_atom(
            RAtom::Int(1),
            "(println (seq 5)) (= (seq 5) (list 1 2 3 4 5))",
        );
        assert_atom(
            RAtom::Int(1),
            "(println (seq (- 3))) (= (seq (- 3)) (list (- 1) (- 2) (- 3)))",
        );
        assert_atom(RAtom::Int(1), "(println (seq 0)) (= (seq 0) ())");

        assert_atom(
            RAtom::Int(1),
            "(println (seq 5 8)) (= (seq 5 8) (list 5 6 7 8))",
        );
        assert_atom(
            RAtom::Int(1),
            "(println (seq 8 5)) (= (seq 8 5) (list 8 7 6 5))",
        );
        assert_atom(RAtom::Int(1), "(println (seq 5 5)) (= (seq 5 5) (list 5))");
        assert_atom(RAtom::Int(1), "(println (seq 0 0)) (= (seq 0 0) (list 0))");
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
    fn string_car_cdr() {
        assert_atom(RAtom::String("h".into()), r#"(car "hello")"#);
        assert_atom(RAtom::String("e".into()), r#"(car (cdr "hello"))"#);
        assert_atom(RAtom::String("llo".into()), r#"(cdr (cdr "hello"))"#);
    }

    #[test]
    fn if_() {
        assert_atom(
            RAtom::Int(1),
            r"
                (def result 0)
                (if 1
                  (set result 1)
                  (set result 2))
                result
            ",
        );

        assert_atom(
            RAtom::Int(2),
            r"
                (def result 0)
                (if 0
                  (set result 1)
                  (set result 2))
                result
            ",
        );

        assert_atom(
            RAtom::Int(2),
            r"
                (def result 0)
                (if ()
                  (set result 1)
                  (set result 2))
                result
            ",
        );
    }

    #[test]
    fn map() {
        assert_atom(
            RAtom::Int(1),
            r"
                (def f (func (x) (* x 2)))

                (def xs (map (seq 5) f))

                (println xs)
                (= xs (list 2 4 6 8 10))
            ",
        );
    }

    #[test]
    fn fold() {
        assert_atom(RAtom::Int(15), "(fold (seq 5) +)");
        assert_atom(RAtom::Int(120), "(fold (seq 5) *)");
        assert_atom(
            RAtom::String("hello".into()),
            r#"(fold (list "he" "l" "lo") +)"#,
        );
    }

    #[cfg(test)]
    mod type_check {
        use super::*;

        #[test]
        fn is_int() {
            assert_atom(RAtom::Int(1), "(is-int 2)");
            assert_atom(RAtom::Int(0), "(is-int \"hello\")");
            assert_atom(RAtom::Int(0), "(is-int (list 12 34))");
            assert_atom(RAtom::Int(0), "(is-int if)");
        }

        #[test]
        fn is_string() {
            assert_atom(RAtom::Int(0), "(is-string 2)");
            assert_atom(RAtom::Int(1), "(is-string \"hello\")");
            assert_atom(RAtom::Int(0), "(is-string (list 12 34))");
            assert_atom(RAtom::Int(0), "(is-string if)");
        }

        #[test]
        fn is_list() {
            assert_atom(RAtom::Int(0), "(is-list 2)");
            assert_atom(RAtom::Int(0), "(is-list \"hello\")");
            assert_atom(RAtom::Int(1), "(is-list (list 12 34))");
            assert_atom(RAtom::Int(0), "(is-list if)");
        }

        #[test]
        fn is_func() {
            assert_atom(RAtom::Int(0), "(is-func 2)");
            assert_atom(RAtom::Int(0), "(is-func \"hello\")");
            assert_atom(RAtom::Int(0), "(is-func (list 12 34))");
            assert_atom(RAtom::Int(1), "(is-func if)");
        }
    }

    #[cfg(test)]
    mod compare {
        use super::*;

        #[test]
        fn eq_ne() {
            assert_atom(RAtom::Int(1), "(= 1 1)");
            assert_atom(RAtom::Int(0), "(= 1 2)");
            assert_atom(RAtom::Int(1), "(= 2 2)");
            assert_atom(RAtom::Int(1), "(= (list 1 2 3) (list 1 2 3))");
            assert_atom(RAtom::Int(0), "(= (list 1 2 4) (list 1 2 3))");
            assert_atom(RAtom::Int(0), "(= (list 1 2 3 4) (list 1 2 3))");
            assert_atom(RAtom::Int(1), "(= () (list))");

            assert_atom(RAtom::Int(1), "(= println println)");
            assert_atom(RAtom::Int(0), "(= if println)");

            assert_atom(RAtom::Int(1), "(def f (func ())) (= f f)");
            assert_atom(RAtom::Int(0), "(def f (func ())) (= f (func ()))");

            assert_atom(RAtom::Int(0), "(!= 1 1)");
            assert_atom(RAtom::Int(1), "(!= 1 2)");
            assert_atom(RAtom::Int(0), "(!= if if)");
            assert_atom(RAtom::Int(1), "(!= if println)");
        }

        #[test]
        fn lt() {
            assert_atom(RAtom::Int(1), "(< 1 2)");
            assert_atom(RAtom::Int(0), "(< 2 2)");
            assert_atom(RAtom::Int(0), "(< 3 2)");

            assert_atom(RAtom::Int(1), "(< 1 2 3)");
            assert_atom(RAtom::Int(0), "(< 1 2 2)");
            assert_atom(RAtom::Int(0), "(< 1 2 1)");

            assert_atom(RAtom::Int(1), "(< (list 1 2 3) (list 2 2 3))");
            assert_atom(RAtom::Int(0), "(< (list 1 2 3) (list 1 2 3))");
            assert_atom(RAtom::Int(0), "(< (list 1 2 3) (list 0 2 3))");

            assert_atom(RAtom::Int(1), "(< (list 1 2 3) (list 1 2 3 4))");
            assert_atom(RAtom::Int(0), "(< (list 1 2 3) (list 1 2 3))");
            assert_atom(RAtom::Int(0), "(< (list 1 2 3 4) (list 1 2 3))");
        }

        #[test]
        fn le() {
            assert_atom(RAtom::Int(1), "(<= 1 2)");
            assert_atom(RAtom::Int(1), "(<= 2 2)");
            assert_atom(RAtom::Int(0), "(<= 3 2)");

            assert_atom(RAtom::Int(1), "(<= 1 2 3)");
            assert_atom(RAtom::Int(1), "(<= 1 2 2)");
            assert_atom(RAtom::Int(0), "(<= 1 2 1)");

            assert_atom(RAtom::Int(1), "(<= (list 1 2 3) (list 2 2 3))");
            assert_atom(RAtom::Int(1), "(<= (list 1 2 3) (list 1 2 3))");
            assert_atom(RAtom::Int(0), "(<= (list 1 2 3) (list 0 2 3))");

            assert_atom(RAtom::Int(1), "(<= (list 1 2 3) (list 1 2 3 4))");
            assert_atom(RAtom::Int(1), "(<= (list 1 2 3) (list 1 2 3))");
            assert_atom(RAtom::Int(0), "(<= (list 1 2 3 4) (list 1 2 3))");
        }

        #[test]
        fn gt() {
            assert_atom(RAtom::Int(0), "(> 1 2)");
            assert_atom(RAtom::Int(0), "(> 2 2)");
            assert_atom(RAtom::Int(1), "(> 3 2)");

            assert_atom(RAtom::Int(0), "(> 3 2 3)");
            assert_atom(RAtom::Int(0), "(> 3 2 2)");
            assert_atom(RAtom::Int(1), "(> 3 2 1)");

            assert_atom(RAtom::Int(0), "(> (list 1 2 3) (list 2 2 3))");
            assert_atom(RAtom::Int(0), "(> (list 1 2 3) (list 1 2 3))");
            assert_atom(RAtom::Int(1), "(> (list 1 2 3) (list 0 2 3))");

            assert_atom(RAtom::Int(0), "(> (list 1 2 3) (list 1 2 3 4))");
            assert_atom(RAtom::Int(0), "(> (list 1 2 3) (list 1 2 3))");
            assert_atom(RAtom::Int(1), "(> (list 1 2 3 4) (list 1 2 3))");
        }

        #[test]
        fn ge() {
            assert_atom(RAtom::Int(0), "(>= 1 2)");
            assert_atom(RAtom::Int(1), "(>= 2 2)");
            assert_atom(RAtom::Int(1), "(>= 3 2)");

            assert_atom(RAtom::Int(0), "(>= 3 2 3)");
            assert_atom(RAtom::Int(1), "(>= 3 2 2)");
            assert_atom(RAtom::Int(1), "(>= 3 2 1)");

            assert_atom(RAtom::Int(0), "(>= (list 1 2 3) (list 2 2 3))");
            assert_atom(RAtom::Int(1), "(>= (list 1 2 3) (list 1 2 3))");
            assert_atom(RAtom::Int(1), "(>= (list 1 2 3) (list 0 2 3))");

            assert_atom(RAtom::Int(0), "(>= (list 1 2 3) (list 1 2 3 4))");
            assert_atom(RAtom::Int(1), "(>= (list 1 2 3) (list 1 2 3))");
            assert_atom(RAtom::Int(1), "(>= (list 1 2 3 4) (list 1 2 3))");
        }
    }
}
