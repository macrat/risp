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

    fn call(&self, _: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        if args.len() == 0 {
            return Err(RError::Argument(
                "`func` needs at least 1 argument but got no argument.".into(),
            ));
        }

        Ok(RValue::Func(Rc::new(RFunc::Pure {
            args: match &args[0] {
                RValue::List(list) => {
                    let mut symbols: Vec<String> = Vec::new();
                    for x in list.iter() {
                        if let RValue::Atom(RAtom::Symbol(name)) = x {
                            symbols.push(String::from(name))
                        } else {
                            return Err(RError::Type(String::from(
                                "the first argument of `func` should be a list of symbol.",
                            )));
                        }
                    }
                    symbols
                }
                _ => {
                    return Err(RError::Type(String::from(
                        "first argument of `func` should be a list of symbol.",
                    )))
                }
            },
            body: RList::from(&args[1..], None),
            capture: scope.clone(),
        })))
    }
}

impl fmt::Display for Func {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "func")
    }
}
