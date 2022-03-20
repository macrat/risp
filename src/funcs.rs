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
                                "elements in the first argument of `func` should be a symbol.",
                            )));
                        }
                    }
                    symbols
                }
                _ => {
                    return Err(RError::Type(String::from(
                        "first argument of `func` should be a list.",
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
struct Operator<'a>(&'a str, fn(Vec<RType>) -> Result<RType, RError>);

impl Callable for Operator<'_> {
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

macro_rules! binary_func {
    ($func:expr) => {
        Rc::new(RType::Func(Rc::new(RFunc::Binary(Box::new($func)))))
    };
}

macro_rules! register {
    ($scope:expr, $name:expr, $func:expr) => {
        if let Err(err) = $scope.define(String::from($name), $func) {
            return Err(err);
        }
    };
}

pub fn register_to(scope: &mut Scope) -> Result<(), RError> {
    register!(scope, "func", binary_func!(Func));
    register!(scope, "def", binary_func!(Def));
    register!(scope, "set", binary_func!(Set));
    register!(scope, "println", binary_func!(Println));
    register!(scope, "if", binary_func!(If));

    register!(
        scope,
        "+",
        binary_func!(Operator("+", |xs| {
            let mut result = 0;
            for x in xs {
                if let RType::Atom(RAtom::Int(x)) = x {
                    result += x;
                } else {
                    return Err(RError::Type(format!("`+` can not apply to `{}`", x)));
                }
            }
            Ok(RType::Atom(RAtom::Int(result)))
        }))
    );

    register!(
        scope,
        "-",
        binary_func!(Operator("-", |xs| {
            match xs.len() {
                0 => Ok(RType::Atom(RAtom::Int(0))),
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
        binary_func!(Operator("*", |xs| {
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

    Ok(())
}
