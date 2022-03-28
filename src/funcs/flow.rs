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
            return Err(RError::argument(format!(
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
            return Err(RError::argument(format!(
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
pub struct Throw;

impl Callable for Throw {
    fn name(&self) -> &str {
        "throw"
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        match args.len() {
            0 => Err(RValue::nil().into()),
            1 => Err(args[0].compute(env, scope)?.into()),
            _ => Err(RError::argument(format!(
                "`throw` needs 0 or 1 argument but got {}",
                args,
            ))),
        }
    }
}

#[derive(Debug)]
pub struct TryCatch;

impl Callable for TryCatch {
    fn name(&self) -> &str {
        "try-catch"
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        if args.len() < 2 {
            return Err(RError::argument(format!(
                "`try-catch` needs at least 2 arguments but got {}",
                args,
            )));
        }

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

        match RList::from(&args[1..], None).compute_last(env, scope) {
            Ok(val) => Ok(val),
            Err(err) => {
                // XXX: This variables are not necessary, if interpreter can decide if a RList means calling function or just a list.
                //      But in current implementation, this is necessary sadly.
                let scope = scope.child();
                scope.define("__error__".to_string(), err.into())?;
                scope.define("__trace__".to_string(), (&env.trace).into())?;

                let result = func.call(
                    env,
                    &scope,
                    RList::from(
                        &[
                            RValue::Atom(RAtom::Symbol("__error__".to_string())),
                            RValue::Atom(RAtom::Symbol("__trace__".to_string())),
                        ],
                        None,
                    ),
                )?;

                env.trace.restore(saved_trace);

                Ok(result)
            }
        }
    }
}
