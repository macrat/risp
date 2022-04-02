use std::cell::RefCell;
use std::io;

use crate::env::Env;
use crate::scope::Scope;
use crate::types::*;

#[derive(Debug)]
pub struct Import;

impl Callable for Import {
    fn name(&self) -> &str {
        "import"
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::Exact(1)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        let name = match args[0].compute(env, scope)? {
            RValue::Atom(RAtom::String(x)) => x,
            x => {
                return Err(RError::type_(format!(
                    "`import` needs a string as argument but got `{}`.",
                    x
                )))
            }
        };

        env.load(scope, name)
    }
}

#[derive(Debug)]
pub struct Stream<'a, T: io::Write>(&'a str, RefCell<T>);

pub fn stdout<'a>() -> Stream<'a, io::Stdout> {
    Stream("stdout", RefCell::new(io::stdout()))
}

pub fn stderr<'a>() -> Stream<'a, io::Stderr> {
    Stream("stderr", RefCell::new(io::stderr()))
}

impl<T: io::Write> Callable for Stream<'_, T> {
    fn name(&self) -> &str {
        self.0
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::Exact(1)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        match self
            .1
            .borrow_mut()
            .write(&args[0].compute(env, scope)?.to_printable().as_bytes())
        {
            Ok(_) => Ok(RValue::nil()),
            Err(err) => Err(RError::io(err.to_string())),
        }
    }
}
