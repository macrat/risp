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
        args.compute(env, scope)
    }
}

#[derive(Debug)]
pub struct Length;

impl Callable for Length {
    fn name(&self) -> &str {
        "length"
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::Exact(1)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        let len = match args[0].compute(env, scope)? {
            RValue::Atom(RAtom::String(s)) => s.chars().count(),
            RValue::List(list) => list.len(),
            x => {
                return Err(RError::type_(format!(
                    "the argument of `length` must be a list or a string, but got {}.",
                    x.type_str(),
                )))
            }
        };

        Ok((len as f64).into())
    }
}

fn calc_index(idx: f64, len: usize) -> Result<usize, RError> {
    if 0.0 <= idx {
        if len <= idx as usize {
            Err(RError::out_of_bounds(idx, len))
        } else {
            Ok(idx as usize)
        }
    } else {
        let rev = (len as f64) + idx;
        if rev < 0.0 || len < rev as usize {
            Err(RError::out_of_bounds(idx, len))
        } else {
            Ok(rev as usize)
        }
    }
}

#[test]
fn test_calc_index() {
    assert_eq!(calc_index(0.0, 10), Ok(0));

    assert_eq!(calc_index(3.0, 10), Ok(3));
    assert_eq!(calc_index(9.0, 10), Ok(9));
    assert_eq!(calc_index(10.0, 10), Err(RError::out_of_bounds(10.0, 10)));

    assert_eq!(calc_index(-2.0, 10), Ok(8));
    assert_eq!(calc_index(-10.0, 10), Ok(0));
    assert_eq!(calc_index(-11.0, 10), Err(RError::out_of_bounds(-11.0, 10)));
}

#[derive(Debug)]
pub struct Get;

impl Callable for Get {
    fn name(&self) -> &str {
        "get"
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::Exact(2)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        let idx = match args[0].compute(env, scope)? {
            RValue::Atom(RAtom::Number(n)) => n,
            x => {
                return Err(RError::type_(format!(
                    "the first argument of `get` must be a number, but got {}.",
                    x.type_str(),
                )))
            }
        };

        match args[1].compute(env, scope)? {
            RValue::Atom(RAtom::String(s)) => {
                if let Some(c) = s.chars().nth(calc_index(idx, s.chars().count())?) {
                    Ok(c.to_string().into())
                } else {
                    Err(RError::out_of_bounds(idx, s.chars().count()))
                }
            }
            RValue::List(list) => Ok(list[calc_index(idx, list.len())?].clone()),
            x => Err(RError::type_(format!(
                "the second argument of `head` must be a list or a string, but got {}.",
                x.type_str(),
            ))),
        }
    }
}

#[derive(Debug)]
pub struct Head;

impl Callable for Head {
    fn name(&self) -> &str {
        "head"
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::Exact(2)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        let idx = match args[0].compute(env, scope)? {
            RValue::Atom(RAtom::Number(n)) => n,
            x => {
                return Err(RError::type_(format!(
                    "the first argument of `head` must be a number, but got {}.",
                    x.type_str(),
                )))
            }
        };

        match args[1].compute(env, scope)? {
            RValue::Atom(RAtom::String(s)) => {
                Ok(String::from_iter(s.chars().take(calc_index(idx, s.chars().count())?)).into())
            }
            RValue::List(list) => Ok(RValue::List(RList::new(
                list[..calc_index(idx, list.len())?].into(),
                None,
            ))),
            x => Err(RError::type_(format!(
                "the second argument of `head` must be a list or a string, but got {}.",
                x.type_str(),
            ))),
        }
    }
}

#[derive(Debug)]
pub struct Tail;

impl Callable for Tail {
    fn name(&self) -> &str {
        "tail"
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::Exact(2)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        let idx = match args[0].compute(env, scope)? {
            RValue::Atom(RAtom::Number(n)) => n,
            x => {
                return Err(RError::type_(format!(
                    "the first argument of `tail` must be a number, but got {}.",
                    x.type_str(),
                )))
            }
        };

        match args[1].compute(env, scope)? {
            RValue::Atom(RAtom::String(s)) => {
                if idx == 0.0 {
                    Ok("".into())
                } else {
                    let len = s.chars().count();
                    let idx = calc_index(idx, len)?;
                    Ok(String::from_iter(s.chars().skip(len - idx)).into())
                }
            }
            RValue::List(list) => Ok(RValue::List(RList::new(
                list[(list.len() - calc_index(idx, list.len())?)..].into(),
                None,
            ))),
            x => Err(RError::type_(format!(
                "the second argument of `tail` must be a list or a string, but got {}.",
                x.type_str(),
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
        let func = match args[0].compute(env, scope)? {
            RValue::Func(f) => f,
            x => {
                return Err(RError::type_(format!(
                    "the first argument of `map` must be a function, but got `{}`.",
                    x
                )))
            }
        };

        func.arg_rule().check(&"map function".to_string(), 1)?;

        match args[1].compute(env, scope)? {
            RValue::List(list) => {
                let mut result = RList::empty(None);

                for x in list.iter() {
                    result.push(func.call(env, scope, RList::new(vec![x.clone()], None))?);
                }

                Ok(RValue::List(result))
            }
            RValue::Atom(RAtom::String(s)) => {
                let mut result = RList::empty(None);

                for c in s.chars() {
                    result.push(func.call(
                        env,
                        scope,
                        RList::new(vec![c.to_string().into()], None),
                    )?);
                }

                Ok(RValue::List(result))
            }
            x => Err(RError::type_(format!(
                "the second argument of `map` must be a list or a string, but got `{}`.",
                x
            ))),
        }
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
        let func = match args[0].compute(env, scope)? {
            RValue::Func(f) => f,
            x => {
                return Err(RError::type_(format!(
                    "the first argument of `fold` must be a function, but got `{}`.",
                    x
                )))
            }
        };

        func.arg_rule().check(&"fold function".to_string(), 2)?;

        match args[1].compute(env, scope)? {
            RValue::List(list) if list.len() >= 2 => {
                let mut result = list[0].clone();

                for x in &list[1..] {
                    result = func.call(env, scope, RList::new(vec![result, x.clone()], None))?;
                }

                Ok(result)
            }
            RValue::List(xs) if xs.len() == 1 => Ok(xs[0].clone()),
            RValue::List(xs) => Ok(RValue::List(xs.clone())),
            RValue::Atom(RAtom::String(s)) if s.len() <= 1 => Ok(s.into()),
            RValue::Atom(RAtom::String(s)) => {
                let mut chars = s.chars();
                let mut result: RValue = if let Some(c) = chars.next() {
                    c.to_string().into()
                } else {
                    return Ok("".into());
                };

                for c in chars {
                    result = func.call(
                        env,
                        scope,
                        RList::new(vec![result, c.to_string().into()], None),
                    )?;
                }

                Ok(result)
            }
            x => Err(RError::type_(format!(
                "the second argument of `fold` must be a list or a string, but got `{}`.",
                x,
            ))),
        }
    }
}
