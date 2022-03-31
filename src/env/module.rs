use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

use super::Env;
use crate::parser::Parser;
use crate::scope::Scope;
use crate::types::*;

#[derive(Debug, Clone)]
pub struct Module {
    path: String,
    name: String,
    scope: Scope,
}

impl Module {
    pub fn new(name: String, scope: Scope) -> Module {
        Module {
            name: format!("(import {:?})", name),
            path: name,
            scope,
        }
    }

    fn load_from<T: BufRead>(
        &mut self,
        env: &mut Env,
        path: String,
        source: T,
    ) -> Result<(), RError> {
        let mut parser = Parser::new(path);

        for line in source.lines() {
            match line {
                Ok(line) => {
                    parser.feed(line.as_str())?;
                    parser.feed_char('\n')?;
                }
                Err(err) => {
                    return Err(RError::io(format!("failed to read {}: {}", self.name, err)))
                }
            };

            while let Some(expr) = parser.pop() {
                expr.compute(env, &self.scope)?;
            }
        }

        parser.close()?;

        while let Some(expr) = parser.pop() {
            expr.compute(env, &self.scope)?;
        }

        Ok(())
    }

    pub fn load(&mut self, env: &mut Env) -> Result<(), RError> {
        let cwd = match env.trace.position() {
            Some(p) => p.file,
            None => ".".to_string(),
        };
        let raw_path = Path::new(cwd.as_str()).with_file_name(self.path.clone());
        let path = match raw_path.canonicalize() {
            Ok(p) => match p.to_str() {
                Some(p) => p.to_string(),
                None => return Err(RError::io(format!("failed to lookup {:?}", raw_path))),
            },
            Err(err) => {
                return Err(RError::io(format!(
                    "failed to lookup {:?}: {}",
                    raw_path, err
                )))
            }
        };
        match File::open(path.clone()) {
            Ok(file) => self.load_from(env, path, BufReader::new(file)),
            Err(err) => Err(RError::io(format!("failed to open {:?}: {}", path, err))),
        }
    }
}

impl Callable for Module {
    fn name(&self) -> &str {
        self.name.as_str()
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::Exact(1)
    }

    fn call(&self, env: &mut Env, _: &Scope, args: RList) -> Result<RValue, RError> {
        args[0].compute(env, &mut self.scope.clone())
    }
}
