use std::rc::Rc;

use crate::scope::Scope;
use crate::types::{RError, RFunc, RValue};

pub mod module;
pub mod trace;

pub struct Env {
    pub modules: Scope,
    pub trace: trace::Trace,
}

impl Env {
    pub fn new() -> Env {
        Env {
            modules: Scope::new(),
            trace: trace::Trace::new(),
        }
    }

    pub fn load(&mut self, scope: &Scope, name: String) -> Result<RValue, RError> {
        if let Ok(module) = self.modules.get(&name) {
            return Ok(module);
        }

        let mut module = module::Module::new(name.clone(), scope.root().child());

        let modfunc = RValue::Func(Rc::new(RFunc::Binary(Box::new(module.clone()))));
        self.modules.define(name.clone(), modfunc.clone())?;

        module.load(self)?;

        self.modules.set(name, modfunc.clone())?;
        Ok(modfunc)
    }
}
