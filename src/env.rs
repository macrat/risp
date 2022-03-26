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

        self.modules.define(name.clone(), RValue::nil())?;

        let module = module::Module::load(self, scope.root().child(), name.clone())?;
        let module = RValue::Func(Rc::new(RFunc::Binary(Box::new(module))));

        self.modules.set(name, module.clone())?;
        Ok(module)
    }
}
