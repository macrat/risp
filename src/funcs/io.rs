use crate::env::Env;
use crate::scope::Scope;
use crate::types::*;

#[derive(Debug)]
pub struct Import;

impl Callable for Import {
    fn name(&self) -> &str {
        "import"
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        if args.len() != 1 {
            return Err(RError::Argument(format!(
                "`import` needs exact 1 argument but got `{}`.",
                args
            )));
        }

        let name = match args[0].compute(env, scope)? {
            RValue::Atom(RAtom::String(x)) => x,
            x => {
                return Err(RError::Type(format!(
                    "`import` needs a string as argument but got `{}`.",
                    x
                )))
            }
        };

        env.load(scope, name)
    }
}

#[derive(Debug)]
pub struct PrintFunc<'a>(&'a str, &'a str);

impl Callable for PrintFunc<'_> {
    fn name(&self) -> &str {
        self.0
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        let xs = args.compute_each(env, scope)?;
        print!("{}{}", xs.to_bare_printable(), self.1);
        Ok(RValue::nil())
    }
}

pub const PRINT: PrintFunc = PrintFunc("print", "");
pub const PRINTLN: PrintFunc = PrintFunc("println", "\n");
