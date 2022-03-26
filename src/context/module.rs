use std::cell::RefCell;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;
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
            name: format!("(import {:?})", name),
            scope: ctx.scope(),
        })
    }

    pub fn load(ctx: &mut Context, name: String) -> Result<Module, RError> {
        let cwd = match ctx.trace().borrow().position() {
            Some(p) => p.file,
            None => ".".to_string(),
        };
        let path = match Path::new(cwd.as_str())
            .with_file_name(name.clone())
            .canonicalize()
        {
            Ok(p) => match p.to_str() {
                Some(p) => String::from(p),
                None => return Err(RError::IO(format!("failed to lookup {}", name))),
            },
            Err(err) => return Err(RError::IO(format!("failed to lookup {}: {}", name, err))),
        };
        match File::open(path.clone()) {
            Ok(file) => Module::load_from(ctx, path, BufReader::new(file)),
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
                "{} needs exact 1 argument but got {}.",
                self.name(),
                args,
            )));
        }

        args[0].compute(&mut ctx.overload(Rc::clone(&self.scope)))
    }
}
