use std::fmt;
use std::rc::Rc;

use crate::env::Env;
use crate::scope::Scope;
use crate::types::*;

#[derive(Debug)]
pub struct Func;

impl Callable for Func {
    fn name(&self) -> &str {
        "func"
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::AtLeast(1)
    }

    fn call(&self, _: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        Ok(RValue::Func(Rc::new(RFunc::Pure {
            args: match &args[0] {
                RValue::Atom(RAtom::Symbol(name)) => ArgumentDefinition::Variable(name.clone()),
                RValue::List(list) => {
                    let mut symbols: Vec<String> = Vec::new();
                    for x in list.iter() {
                        if let RValue::Atom(RAtom::Symbol(name)) = x {
                            symbols.push(name.into())
                        } else {
                            return Err(RError::type_(
                                "the first argument of `func` should be a list of symbol.".into(),
                            ));
                        }
                    }
                    ArgumentDefinition::Certain(symbols)
                }
                _ => {
                    return Err(RError::type_(
                        "first argument of `func` should be a single symbol or a list of symbol."
                            .into(),
                    ))
                }
            },
            body: RList::new(args[1..].into(), None),
            capture: scope.clone(),
        })))
    }
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "func")
    }
}

#[derive(Debug)]
pub struct Apply;

impl Callable for Apply {
    fn name(&self) -> &str {
        "apply"
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::Exact(2)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        let func = match args[0].compute(env, scope)? {
            RValue::Func(func) => func,
            x => {
                return Err(RError::type_(format!(
                    "the first argument of `apply` should be a func but got {}.",
                    x.type_str(),
                )))
            }
        };

        let args = match args[1].compute(env, scope)? {
            RValue::List(list) => list,
            x => {
                return Err(RError::type_(format!(
                    "the second argument of `apply` should be a list but got {}.",
                    x.type_str(),
                )))
            }
        };

        func.call(env, scope, args)
    }
}
