use std::cmp::Ordering;

use crate::env::Env;
use crate::scope::Scope;
use crate::types::*;

#[derive(Debug)]
pub struct CalculateOperator<'a>(&'a str, fn(Vec<RValue>) -> Result<RValue, RError>);

impl Callable for CalculateOperator<'_> {
    fn name(&self) -> &str {
        self.0
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        let mut xs: Vec<RValue> = Vec::new();
        for x in args.iter() {
            xs.push(x.compute(env, scope)?);
        }
        self.1(xs)
    }
}

pub const ADD: CalculateOperator = CalculateOperator("+", |xs| {
    if xs.len() == 0 {
        return Err(RError::argument("`+` needs at least 1 value.".into()));
    }

    if let RValue::Atom(RAtom::String(_)) = xs[0] {
        let mut result = String::new();
        for x in xs {
            if let RValue::Atom(RAtom::String(x)) = x {
                result += &x;
            } else {
                return Err(RError::type_(format!("`+` can not apply to {}", x)));
            }
        }
        Ok(result.into())
    } else {
        let mut result = 0.0;
        for x in xs {
            if let RValue::Atom(RAtom::Number(x)) = x {
                result += x;
            } else {
                return Err(RError::type_(format!("`+` can not apply to {}", x)));
            }
        }
        Ok(result.into())
    }
});

pub const SUB: CalculateOperator = CalculateOperator("-", |xs| match xs.len() {
    0 => Err(RError::argument("`-` needs at least 1 value.".into())),
    1 => {
        if let RValue::Atom(RAtom::Number(x)) = xs[0] {
            Ok((-x).into())
        } else {
            Err(RError::type_(format!("`-` can not apply to {}", xs[0])))
        }
    }
    _ => {
        let mut result = if let RValue::Atom(RAtom::Number(x)) = xs[0] {
            x
        } else {
            return Err(RError::type_(format!("`-` can not apply to {}", xs[0])));
        };

        for x in &xs[1..] {
            if let RValue::Atom(RAtom::Number(x)) = x {
                result -= *x;
            } else {
                return Err(RError::type_(format!("`-` can not apply to {}", x)));
            }
        }

        Ok(result.into())
    }
});

pub const MULTIPLY: CalculateOperator = CalculateOperator("*", |xs| {
    if xs.len() < 2 {
        return Err(RError::argument("`*` needs at least 2 values.".into()));
    }

    let mut result = 1.0;
    for x in xs {
        if let RValue::Atom(RAtom::Number(x)) = x {
            result *= x;
        } else {
            return Err(RError::type_(format!("`*` can not apply to {}", x)));
        }
    }
    Ok(result.into())
});

pub const DIVIDE: CalculateOperator = CalculateOperator("/", |xs| {
    if xs.len() < 2 {
        return Err(RError::argument("`/` needs at least 2 values.".into()));
    }

    let mut result = if let RValue::Atom(RAtom::Number(x)) = xs[0] {
        x
    } else {
        return Err(RError::type_(format!("`/` can not apply to {}", xs[0])));
    };

    for x in &xs[1..] {
        if let RValue::Atom(RAtom::Number(x)) = x {
            result /= *x;
        } else {
            return Err(RError::type_(format!("`/` can not apply to {}", x)));
        }
    }

    Ok(result.into())
});

pub struct CompareOperator<'a>(&'a str, fn(&RValue, &RValue) -> Result<bool, RError>);

impl Callable for CompareOperator<'_> {
    fn name(&self) -> &str {
        self.0
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        if args.len() < 2 {
            return Err(RError::argument(format!(
                "`{}` needs at least 2 values.",
                self.0,
            )));
        }

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
pub struct Not;

impl Callable for Not {
    fn name(&self) -> &str {
        "not"
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        if args.len() != 1 {
            return Err(RError::argument(format!(
                "`not` needs exact 1 argument but got `{}`.",
                args,
            )));
        }

        let result = if args[0].compute(env, scope)?.as_bool() {
            0.0
        } else {
            1.0
        };
        Ok(result.into())
    }
}
