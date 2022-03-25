use std::cmp::Ordering;
use std::fmt;
use std::rc::Rc;

use crate::context::{self, Context};
use crate::types::*;

#[derive(Debug)]
struct Func;

impl Callable for Func {
    fn name(&self) -> &str {
        "func"
    }

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError> {
        if args.len() == 0 {
            return Err(RError::Argument(
                "`func` needs at least 1 argument but got no argument.".into(),
            ));
        }

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
            body: RList::from(&args[1..], None),
            capture: ctx.scope(),
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

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError> {
        if args.len() != 2 {
            return Err(RError::Argument(format!(
                "`def` needs exact 2 arguments but got `{}`.",
                args
            )));
        }

        let value = match args[1].compute(ctx) {
            Ok(x) => x,
            Err(err) => return Err(err),
        };

        if let RType::Atom(RAtom::Symbol(name)) = &args[0] {
            if let Err(err) = ctx.define_value(String::from(name), Rc::new(value.clone())) {
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

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError> {
        let value = match args[1].compute(ctx) {
            Ok(x) => x,
            Err(err) => return Err(err),
        };

        if let RType::Atom(RAtom::Symbol(name)) = &args[0] {
            if let Err(err) = ctx.set_value(String::from(name), Rc::new(value.clone())) {
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
struct Import;

impl Callable for Import {
    fn name(&self) -> &str {
        "import"
    }

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError> {
        if args.len() != 1 {
            return Err(RError::Argument(format!(
                "`import` needs exact 1 argument but got `{}`.",
                args
            )));
        }

        let name = match args[0].compute(ctx) {
            Ok(RType::Atom(RAtom::String(x))) => x,
            Ok(x) => {
                return Err(RError::Type(format!(
                    "`import` needs a string as argument but got `{}`.",
                    x
                )))
            }
            Err(err) => return Err(err),
        };

        ctx.load(name)
    }
}

#[derive(Debug)]
struct Print<'a>(&'a str, &'a str);

impl Callable for Print<'_> {
    fn name(&self) -> &str {
        self.0
    }

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError> {
        match args.compute_each(ctx) {
            Ok(x) => {
                print!("{}{}", x.to_bare_printable(), self.1);
                Ok(RType::nil())
            }
            Err(err) => Err(err),
        }
    }
}

#[derive(Debug)]
struct Panic;

impl Callable for Panic {
    fn name(&self) -> &str {
        "panic!"
    }

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError> {
        match args.len() {
            0 => Err(RError::User(RType::nil())),
            1 => match args[0].compute(ctx) {
                Ok(x) => Err(RError::User(x)),
                Err(err) => Err(err),
            },
            _ => Err(RError::Argument(format!(
                "`panic!` needs 0 or 1 argument but got {}",
                args
            ))),
        }
    }
}

#[derive(Debug)]
struct If;

impl Callable for If {
    fn name(&self) -> &str {
        "if"
    }

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError> {
        if args.len() != 2 && args.len() != 3 {
            return Err(RError::Argument(format!(
                "`if` requires 2 or 3 arguments but got {} arguments.",
                args.len()
            )));
        }

        let cond = match args[0].compute(ctx) {
            Ok(val) => val.as_bool(),
            Err(err) => return Err(err),
        };

        if cond {
            args[1].compute(ctx)
        } else if args.len() == 3 {
            args[2].compute(ctx)
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

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError> {
        args.compute_last(&mut ctx.child())
    }
}

#[derive(Debug)]
struct While;

impl Callable for While {
    fn name(&self) -> &str {
        "while"
    }

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError> {
        if args.len() == 0 {
            return Err(RError::Argument(format!(
                "`while` needs 1 or more arguments but got {}.",
                args
            )));
        }

        let mut local = ctx.child();
        let mut result = RType::nil();

        loop {
            let cond = match args[0].compute(ctx) {
                Ok(val) => val,
                Err(err) => return Err(err),
            };
            if !cond.as_bool() {
                break;
            }

            result = match RList::from(&args[1..], None).compute_last(&mut local) {
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

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError> {
        if args.len() != 2 {
            return Err(RError::Argument(format!(
                "`map` needs exact 2 arguments but got {}.",
                args
            )));
        }

        let list = match args[0].compute(ctx) {
            Ok(RType::List(xs)) => xs,
            Ok(x) => {
                return Err(RError::Type(format!(
                    "the first argument of `map` must be a list, but got `{}`.",
                    x
                )))
            }
            Err(err) => return Err(err),
        };

        let func = match args[1].compute(ctx) {
            Ok(RType::Func(f)) => f,
            Ok(x) => {
                return Err(RError::Type(format!(
                    "the second argument of `map` must be a function, but got `{}`.",
                    x
                )))
            }
            Err(err) => return Err(err),
        };

        let mut result = RList::empty(None);

        for x in list.iter() {
            match func.call(ctx, RList::new(vec![x.clone()], None)) {
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

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError> {
        if args.len() != 2 {
            return Err(RError::Argument(format!(
                "`fold` needs exact 2 arguments but got {}.",
                args
            )));
        }

        let list = match args[0].compute(ctx) {
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

        let func = match args[1].compute(ctx) {
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
            match func.call(ctx, RList::new(vec![result, x.clone()], None)) {
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

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError> {
        match args.compute_each(ctx) {
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

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError> {
        if args.len() != 1 && args.len() != 2 {
            return Err(RError::Argument(format!(
                "`seq` needs 1 or 2 number argument but got {}",
                args
            )));
        }

        let args = match args.compute_each(ctx) {
            Ok(xs) => xs,
            Err(err) => return Err(err),
        };

        let to = match &args[args.len() - 1] {
            RType::Atom(RAtom::Number(n)) => *n,
            x => {
                return Err(RError::Argument(format!(
                    "`seq` needs 1 or 2 number argument but got `{}`",
                    x
                )))
            }
        };
        if args.len() == 1 && to == 0.0 {
            return Ok(RType::nil());
        }

        let from = if args.len() == 2 {
            match &args[0] {
                RType::Atom(RAtom::Number(n)) => *n,
                x => {
                    return Err(RError::Argument(format!(
                        "`seq` needs 1 or 2 number argument but got `{}`",
                        x
                    )))
                }
            }
        } else if to > 0.0 {
            1.0
        } else {
            -1.0
        };

        let step = if from < to { 1.0 } else { -1.0 };

        let mut result = RList::empty(None);
        let mut i = from;
        while i != to {
            result.push(RType::Atom(RAtom::Number(i)));
            i += step;
        }
        result.push(RType::Atom(RAtom::Number(i)));
        Ok(RType::List(result))
    }
}

#[derive(Debug)]
struct Car;

impl Callable for Car {
    fn name(&self) -> &str {
        "car"
    }

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError> {
        if args.len() != 1 {
            Err(RError::Argument(format!(
                "`car` needs exact 1 argument but got {} arguments",
                args.len()
            )))
        } else {
            match args[0].compute(ctx) {
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

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError> {
        if args.len() != 1 {
            Err(RError::Argument(format!(
                "`cdr` needs exact 1 argument but got {} arguments",
                args.len()
            )))
        } else {
            match args[0].compute(ctx) {
                Ok(RType::List(list)) => Ok(RType::List(RList::from(&list[1..], None))),
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

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError> {
        let mut xs: Vec<RType> = Vec::new();
        for x in args.iter() {
            match x.compute(ctx) {
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

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError> {
        if args.len() < 2 {
            return Err(RError::Argument(format!(
                "`{}` needs at least 2 values.",
                self.0
            )));
        }

        let mut x = match args[0].compute(ctx) {
            Ok(x) => x,
            Err(err) => return Err(err),
        };

        for y in &args[1..] {
            match y.compute(ctx) {
                Ok(y) => {
                    match self.1(&x, &y) {
                        Ok(false) => return Ok(RType::Atom(RAtom::Number(0.0))),
                        Err(err) => return Err(err),
                        _ => {}
                    }
                    x = y;
                }
                Err(err) => return Err(err),
            }
        }

        Ok(RType::Atom(RAtom::Number(1.0)))
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

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError> {
        for x in args.iter() {
            match x.compute(ctx) {
                Ok(x) if self.1(&x) => {
                    return Ok(RType::Atom(RAtom::Number(1.0)));
                }
                Err(err) => return Err(err),
                _ => {}
            }
        }
        Ok(RType::Atom(RAtom::Number(0.0)))
    }
}

#[derive(Debug)]
struct NotOperator;

impl Callable for NotOperator {
    fn name(&self) -> &str {
        "not"
    }

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError> {
        if args.len() != 1 {
            return Err(RError::Argument(format!(
                "`not` needs exact 1 argument but got `{}`.",
                args
            )));
        }

        match args[0].compute(ctx) {
            Ok(x) => Ok(RType::Atom(RAtom::Number(if x.as_bool() {
                0.0
            } else {
                1.0
            }))),
            Err(err) => Err(err),
        }
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

pub fn register_to(scope: &mut context::scope::Scope) -> Result<(), RError> {
    register!(scope, "def", binary_func!(Def));
    register!(scope, "set", binary_func!(Set));
    register!(scope, "import", binary_func!(Import));
    register!(scope, "panic!", binary_func!(Panic));

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
        "is-number",
        binary_func!(TypeCheckOperator("is-number", |x| match x {
            RType::Atom(RAtom::Number(_)) => true,
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

    register!(scope, "not", binary_func!(NotOperator));

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
                        return Err(RError::Type(format!("`+` can not apply to {}", x)));
                    }
                }
                Ok(RType::Atom(RAtom::String(result)))
            } else {
                let mut result = 0.0;
                for x in xs {
                    if let RType::Atom(RAtom::Number(x)) = x {
                        result += x;
                    } else {
                        return Err(RError::Type(format!("`+` can not apply to {}", x)));
                    }
                }
                Ok(RType::Atom(RAtom::Number(result)))
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
                    if let RType::Atom(RAtom::Number(x)) = xs[0] {
                        Ok(RType::Atom(RAtom::Number(-x)))
                    } else {
                        Err(RError::Type(format!("`-` can not apply to {}", xs[0])))
                    }
                }
                _ => {
                    let mut result = if let RType::Atom(RAtom::Number(x)) = xs[0] {
                        x
                    } else {
                        return Err(RError::Type(format!("`-` can not apply to {}", xs[0])));
                    };

                    for x in &xs[1..] {
                        if let RType::Atom(RAtom::Number(x)) = x {
                            result -= *x;
                        } else {
                            return Err(RError::Type(format!("`-` can not apply to {}", x)));
                        }
                    }

                    Ok(RType::Atom(RAtom::Number(result)))
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

            let mut result = 1.0;
            for x in xs {
                if let RType::Atom(RAtom::Number(x)) = x {
                    result *= x;
                } else {
                    return Err(RError::Type(format!("`*` can not apply to {}", x)));
                }
            }
            Ok(RType::Atom(RAtom::Number(result)))
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

            let mut result = if let RType::Atom(RAtom::Number(x)) = xs[0] {
                x
            } else {
                return Err(RError::Type(format!("`/` can not apply to {}", xs[0])));
            };

            for x in &xs[1..] {
                if let RType::Atom(RAtom::Number(x)) = x {
                    result /= *x;
                } else {
                    return Err(RError::Type(format!("`/` can not apply to {}", x)));
                }
            }

            Ok(RType::Atom(RAtom::Number(result)))
        }))
    );

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::test::parse;

    fn execute(code: &str) -> Result<RType, RError> {
        let mut ctx = Context::new(context::scope::Scope::new());
        assert_eq!(Ok(()), register_to(&mut ctx.scope().borrow_mut()));

        return match parse(code) {
            Ok(x) => x.compute_last(&mut ctx),
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
            RError::User(RType::Atom(RAtom::String(String::from("hello world")))),
            r#"(panic! "hello world")"#,
        );

        assert_err(
            RError::User(RType::Atom(RAtom::Number(123.0))),
            r#"(panic! 123)"#,
        );

        assert_err(RError::User(RType::nil()), "(panic!)");
    }
}
