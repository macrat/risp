use crate::env::Env;
use crate::scope::Scope;
use crate::types::*;

#[derive(Debug)]
pub struct If;

impl Callable for If {
    fn name(&self) -> &str {
        "if"
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        if args.len() != 2 && args.len() != 3 {
            return Err(RError::Argument(format!(
                "`if` requires 2 or 3 arguments but got {} arguments.",
                args.len(),
            )));
        }

        let cond = args[0].compute(env, scope)?.as_bool();

        if cond {
            args[1].compute(env, scope)
        } else if args.len() == 3 {
            args[2].compute(env, scope)
        } else {
            Ok(RValue::nil())
        }
    }
}

#[derive(Debug)]
pub struct While;

impl Callable for While {
    fn name(&self) -> &str {
        "while"
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        if args.len() == 0 {
            return Err(RError::Argument(format!(
                "`while` needs 1 or more arguments but got {}.",
                args,
            )));
        }

        let local = scope.child();
        let mut result = RValue::nil();

        loop {
            if !args[0].compute(env, scope)?.as_bool() {
                break;
            }

            result = RList::from(&args[1..], None).compute_last(env, &local)?;
        }

        Ok(result)
    }
}

#[derive(Debug)]
pub struct Do;

impl Callable for Do {
    fn name(&self) -> &str {
        "do"
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        args.compute_last(env, &scope.child())
    }
}

#[derive(Debug)]
pub struct Panic;

impl Callable for Panic {
    fn name(&self) -> &str {
        "panic!"
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        match args.len() {
            0 => Err(RError::User(RValue::nil())),
            1 => Err(RError::User(args[0].compute(env, scope)?)),
            _ => Err(RError::Argument(format!(
                "`panic!` needs 0 or 1 argument but got {}",
                args,
            ))),
        }
    }
}
