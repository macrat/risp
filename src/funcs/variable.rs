use crate::env::Env;
use crate::scope::Scope;
use crate::types::*;

#[derive(Debug)]
pub struct Def;

impl Callable for Def {
    fn name(&self) -> &str {
        "def"
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        if args.len() != 2 {
            return Err(RError::argument(format!(
                "`def` needs exact 2 arguments but got `{}`.",
                args,
            )));
        }

        let value = args[1].compute(env, scope)?;

        if let RValue::Atom(RAtom::Symbol(name)) = &args[0] {
            scope.define(name.into(), value.clone())?;
            Ok(value)
        } else {
            Err(RError::type_(format!(
                "first argument of `def` should be a symbol."
            )))
        }
    }
}

#[derive(Debug)]
pub struct Set;

impl Callable for Set {
    fn name(&self) -> &str {
        "set"
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        let value = args[1].compute(env, scope)?;

        if let RValue::Atom(RAtom::Symbol(name)) = &args[0] {
            scope.set(name.into(), value.clone())?;
            Ok(value)
        } else {
            Err(RError::type_(format!(
                "first argument of `set` should be a symbol."
            )))
        }
    }
}
