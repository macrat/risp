use std::cmp::Ordering;

use crate::env::Env;
use crate::scope::Scope;
use crate::types::*;

#[derive(Debug)]
pub struct CalculateOperator<'a>(&'a str, usize, fn(Vec<RValue>) -> Result<RValue, RError>);

impl Callable for CalculateOperator<'_> {
    fn name(&self) -> &str {
        self.0
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::AtLeast(self.1)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        let mut xs: Vec<RValue> = Vec::new();
        for x in args.iter() {
            xs.push(x.compute(env, scope)?);
        }
        self.2(xs)
    }
}

pub const ADD: CalculateOperator = CalculateOperator("+", 1, |xs| match &xs[0] {
    RValue::Atom(RAtom::String(_)) => {
        let mut result = String::new();
        for x in xs {
            if let RValue::Atom(RAtom::String(x)) = x {
                result += &x;
            } else {
                return Err(RError::type_(format!(
                    "`+` can not apply to different types such as string and {}.",
                    x.type_str(),
                )));
            }
        }
        Ok(result.into())
    }
    RValue::Atom(RAtom::Number(_)) => {
        let mut result = 0.0;
        for x in xs {
            if let RValue::Atom(RAtom::Number(x)) = x {
                result += x;
            } else {
                return Err(RError::type_(format!(
                    "`+` can not apply to different types such as number and {}.",
                    x.type_str(),
                )));
            }
        }
        Ok(result.into())
    }
    RValue::List(list) => {
        let mut list = list.clone();
        for xs in &xs[1..] {
            match xs {
                RValue::List(xs) => list.extend(xs.clone()),
                _ => {
                    return Err(RError::type_(format!(
                        "`+` can not apply to different types such as list and {}.",
                        xs.type_str(),
                    )))
                }
            }
        }
        Ok(RValue::List(list))
    }
    x => Err(RError::type_(format!(
        "`+` can not apply to {}.",
        x.type_str()
    ))),
});

pub const SUB: CalculateOperator = CalculateOperator("-", 1, |xs| {
    if xs.len() == 1 {
        match &xs[0] {
            RValue::Atom(RAtom::Number(x)) => Ok((-x).into()),
            x => Err(RError::type_(format!(
                "`-` can not apply to {}.",
                x.type_str(),
            ))),
        }
    } else {
        let mut result = if let RValue::Atom(RAtom::Number(x)) = xs[0] {
            x
        } else {
            return Err(RError::type_(format!(
                "`-` can not apply to {}.",
                xs[0].type_str(),
            )));
        };

        for x in &xs[1..] {
            if let RValue::Atom(RAtom::Number(x)) = x {
                result -= *x;
            } else {
                return Err(RError::type_(format!(
                    "`-` can not apply to {}.",
                    x.type_str(),
                )));
            }
        }

        Ok(result.into())
    }
});

pub const MULTIPLY: CalculateOperator = CalculateOperator("*", 2, |xs| {
    let mut result = 1.0;
    for x in xs {
        if let RValue::Atom(RAtom::Number(x)) = x {
            result *= x;
        } else {
            return Err(RError::type_(format!(
                "`*` can not apply to {}.",
                x.type_str()
            )));
        }
    }
    Ok(result.into())
});

pub const DIVIDE: CalculateOperator = CalculateOperator("/", 2, |xs| {
    let mut result = if let RValue::Atom(RAtom::Number(x)) = xs[0] {
        x
    } else {
        return Err(RError::type_(format!(
            "`/` can not apply to {}.",
            xs[0].type_str(),
        )));
    };

    for x in &xs[1..] {
        if let RValue::Atom(RAtom::Number(x)) = x {
            result /= *x;
        } else {
            return Err(RError::type_(format!(
                "`/` can not apply to {}.",
                x.type_str(),
            )));
        }
    }

    Ok(result.into())
});

pub struct CompareOperator<'a>(&'a str, fn(&RValue, &RValue) -> Result<bool, RError>);

impl Callable for CompareOperator<'_> {
    fn name(&self) -> &str {
        self.0
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::AtLeast(2)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        let mut x = args[0].compute(env, scope)?;

        for y in &args[1..] {
            let y = y.compute(env, scope)?;
            if !self.1(&x, &y)? {
                return Ok(0.0.into());
            }
            x = y;
        }

        Ok(1.0.into())
    }
}

pub const EQ: CompareOperator = CompareOperator("=", |x, y| Ok(*x == *y));
pub const NE: CompareOperator = CompareOperator("!=", |x, y| Ok(*x != *y));

pub const LT: CompareOperator = CompareOperator("<", |x, y| Ok(x.cmp(y)? == Ordering::Less));
pub const GT: CompareOperator = CompareOperator(">", |x, y| Ok(x.cmp(y)? == Ordering::Greater));

pub const LE: CompareOperator = CompareOperator("<=", |x, y| Ok(x.cmp(y)? != Ordering::Greater));
pub const GE: CompareOperator = CompareOperator(">=", |x, y| Ok(x.cmp(y)? != Ordering::Less));

#[derive(Debug)]
pub struct And;

impl Callable for And {
    fn name(&self) -> &str {
        "and"
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::AtLeast(1)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        Ok(
            (if args.compute_each(env, scope)?.iter().all(|x| x.as_bool()) {
                1.0
            } else {
                0.0
            })
            .into(),
        )
    }
}

#[derive(Debug)]
pub struct Or;

impl Callable for Or {
    fn name(&self) -> &str {
        "or"
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::AtLeast(1)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        Ok(
            (if args.compute_each(env, scope)?.iter().any(|x| x.as_bool()) {
                1.0
            } else {
                0.0
            })
            .into(),
        )
    }
}

#[derive(Debug)]
pub struct Not;

impl Callable for Not {
    fn name(&self) -> &str {
        "not"
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::Exact(1)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        let result = if args[0].compute(env, scope)?.as_bool() {
            0.0
        } else {
            1.0
        };
        Ok(result.into())
    }
}
