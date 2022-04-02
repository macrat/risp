use crate::env::Env;
use crate::scope::Scope;
use crate::types::*;

#[derive(Debug)]
pub struct If;

impl Callable for If {
    fn name(&self) -> &str {
        "if"
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::Or(2, 3)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
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

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::AtLeast(1)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        let mut result = RValue::nil();

        loop {
            if !args[0].compute(env, scope)?.as_bool() {
                break;
            }

            let local = scope.child();
            result = RList::new(args[1..].into(), None).compute_last(env, &local)?;
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

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::Any
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        args.compute_last(env, &scope.child())
    }
}

#[derive(Debug)]
pub struct Throw;

impl Callable for Throw {
    fn name(&self) -> &str {
        "throw"
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::Or(0, 1)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        Err(if args.len() == 0 {
            RValue::nil().into()
        } else {
            args[0].compute(env, scope)?.into()
        })
    }
}

#[derive(Debug)]
pub struct TryCatch;

impl Callable for TryCatch {
    fn name(&self) -> &str {
        "try-catch"
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::AtLeast(2)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        let func = match args[0].compute(env, scope)? {
            RValue::Func(func) => func,
            x => {
                return Err(RError::argument(format!(
                    "the first argument of `try-catch` should be a function but got {}",
                    x,
                )))
            }
        };

        let saved_trace = env.trace.save();

        match RList::new(args[1..].into(), None).compute_last(env, scope) {
            Ok(val) => Ok(val),
            Err(err) => {
                let result = func.call(
                    env,
                    &scope,
                    RList::new([err.into(), (&env.trace).into()].into(), None),
                )?;

                env.trace.restore(saved_trace);

                Ok(result)
            }
        }
    }
}
