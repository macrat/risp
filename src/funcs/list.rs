use crate::env::Env;
use crate::scope::Scope;
use crate::types::*;

#[derive(Debug)]
pub struct List;

impl Callable for List {
    fn name(&self) -> &str {
        "list"
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::Any
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        Ok(RValue::List(args.compute_each(env, scope)?))
    }
}

#[derive(Debug)]
pub struct Car;

impl Callable for Car {
    fn name(&self) -> &str {
        "car"
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::Exact(1)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        match args[0].compute(env, scope)? {
            RValue::List(list) => Ok(list[0].clone()),
            RValue::Atom(RAtom::String(x)) => match x.chars().next() {
                Some(c) => Ok(c.to_string().into()),
                None => Ok("".into()),
            },
            x => Err(RError::type_(format!(
                "`car` needs list or string but got {}",
                x,
            ))),
        }
    }
}

#[derive(Debug)]
pub struct Cdr;

impl Callable for Cdr {
    fn name(&self) -> &str {
        "cdr"
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::Exact(1)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        match args[0].compute(env, scope)? {
            RValue::List(list) => Ok(RValue::List(RList::from(&list[1..], None))),
            RValue::Atom(RAtom::String(x)) => {
                let mut chars = x.chars();
                chars.next();
                Ok(chars.as_str().into())
            }
            x => Err(RError::type_(format!(
                "`cdr` needs list or string but got {}",
                x,
            ))),
        }
    }
}

#[derive(Debug)]
pub struct Seq;

impl Callable for Seq {
    fn name(&self) -> &str {
        "seq"
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::Or(1, 2)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        let args = args.compute_each(env, scope)?;

        let to = match &args[args.len() - 1] {
            RValue::Atom(RAtom::Number(n)) => *n,
            x => {
                return Err(RError::argument(format!(
                    "`seq` needs 1 or 2 number argument but got `{}`",
                    x
                )))
            }
        };
        if args.len() == 1 && to == 0.0 {
            return Ok(RValue::nil());
        }

        let from = if args.len() == 2 {
            match &args[0] {
                RValue::Atom(RAtom::Number(n)) => *n,
                x => {
                    return Err(RError::argument(format!(
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
            result.push(i.into());
            i += step;
        }
        result.push(i.into());
        Ok(RValue::List(result))
    }
}

#[derive(Debug)]
pub struct Map;

impl Callable for Map {
    fn name(&self) -> &str {
        "map"
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::Exact(2)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        let list = match args[0].compute(env, scope)? {
            RValue::List(xs) => xs,
            x => {
                return Err(RError::type_(format!(
                    "the first argument of `map` must be a list, but got `{}`.",
                    x
                )))
            }
        };

        let func = match args[1].compute(env, scope)? {
            RValue::Func(f) => f,
            x => {
                return Err(RError::type_(format!(
                    "the second argument of `map` must be a function, but got `{}`.",
                    x
                )))
            }
        };

        let mut result = RList::empty(None);

        for x in list.iter() {
            result.push(func.call(env, scope, RList::new(vec![x.clone()], None))?);
        }

        Ok(RValue::List(result))
    }
}

#[derive(Debug)]
pub struct Fold;

impl Callable for Fold {
    fn name(&self) -> &str {
        "fold"
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::Exact(2)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        let list = match args[0].compute(env, scope)? {
            RValue::List(xs) if xs.len() >= 2 => xs,
            RValue::List(xs) => {
                return Err(RError::type_(format!(
                    "the first argument of `fold` must be longer than 2, but got `{}`.",
                    xs
                )))
            }
            x => {
                return Err(RError::type_(format!(
                    "the first argument of `fold` must be a list, but got `{}`.",
                    x
                )))
            }
        };

        let func = match args[1].compute(env, scope)? {
            RValue::Func(f) => f,
            x => {
                return Err(RError::type_(format!(
                    "the second argument of `fold` must be a function, but got `{}`.",
                    x
                )))
            }
        };

        let mut result = list[0].clone();

        for x in &list[1..] {
            result = func.call(env, scope, RList::new(vec![result, x.clone()], None))?;
        }

        Ok(result)
    }
}
