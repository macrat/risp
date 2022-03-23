use std::cell::RefCell;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::rc::Rc;

use super::scope::Scope;
use super::Context;
use crate::parser::Parser;
use crate::types::*;

#[derive(Debug)]
pub struct Module {
    name: String,
    scope: Rc<RefCell<Scope>>,
}

impl Module {
    fn load_from<T: BufRead>(ctx: &mut Context, name: String, source: T) -> Result<Module, RError> {
        let mut parser = Parser::new(name.clone());

        for line in source.lines() {
            match line {
                Ok(line) => {
                    if let Err(err) = parser.feed(line.as_str()) {
                        return Err(err);
                    }
                    if let Err(err) = parser.feed("\n") {
                        return Err(err);
                    }
                }
                Err(err) => return Err(RError::IO(format!("failed to read {}: {}", name, err))),
            };

            while let Some(expr) = parser.pop() {
                if let Err(err) = expr.compute(ctx) {
                    return Err(err);
                }
            }
        }

        if let Err(err) = parser.close() {
            return Err(err);
        }

        while let Some(expr) = parser.pop() {
            if let Err(err) = expr.compute(ctx) {
                return Err(err);
            }
        }

        Ok(Module {
            name,
            scope: ctx.scope(),
        })
    }

    pub fn load(ctx: &mut Context, name: String) -> Result<Module, RError> {
        match File::open(&name) {
            Ok(file) => Module::load_from(ctx, name, BufReader::new(file)),
            Err(err) => Err(RError::IO(format!("failed to open {}: {}", name, err))),
        }
    }
}

impl Callable for Module {
    fn name(&self) -> &str {
        self.name.as_str()
    }

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError> {
        if args.len() != 1 {
            return Err(RError::Argument(format!(
                "`{}` needs exact 1 argument but got {}.",
                self.name(),
                args,
            )));
        }

        args[0].compute(&mut ctx.overload(Rc::clone(&self.scope)))
    }
}
