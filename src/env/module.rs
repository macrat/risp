use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

use super::Env;
use crate::parser::Parser;
use crate::scope::Scope;
use crate::types::*;

#[derive(Debug, Clone)]
pub struct Module {
    name: String,
    scope: Scope,
}

impl Module {
    fn load_from<T: BufRead>(
        env: &mut Env,
        scope: Scope,
        name: String,
        source: T,
    ) -> Result<Module, RError> {
        let mut parser = Parser::new(name.clone());

        for line in source.lines() {
            match line {
                Ok(line) => {
                    parser.feed(line.as_str())?;
                    parser.feed_char('\n')?;
                }
                Err(err) => return Err(RError::IO(format!("failed to read {}: {}", name, err))),
            };

            while let Some(expr) = parser.pop() {
                expr.compute(env, &scope)?;
            }
        }

        parser.close()?;

        while let Some(expr) = parser.pop() {
            expr.compute(env, &scope)?;
        }

        Ok(Module {
            name: format!("(import {:?})", name),
            scope,
        })
    }

    pub fn load(env: &mut Env, scope: Scope, name: String) -> Result<Module, RError> {
        let cwd = match env.trace.position() {
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
            Ok(file) => Module::load_from(env, scope, path, BufReader::new(file)),
            Err(err) => Err(RError::IO(format!("failed to open {}: {}", name, err))),
        }
    }
}

impl Callable for Module {
    fn name(&self) -> &str {
        self.name.as_str()
    }

    fn call(&self, env: &mut Env, _: &Scope, args: RList) -> Result<RValue, RError> {
        if args.len() != 1 {
            return Err(RError::Argument(format!(
                "{} needs exact 1 argument but got {}.",
                self.name(),
                args,
            )));
        }

        args[0].compute(env, &mut self.scope.clone())
    }
}
