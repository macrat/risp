use std::cell::RefCell;
use std::rc::Rc;

use crate::types::{RError, RFunc, RType};

pub mod module;
pub mod scope;
pub mod trace;

pub struct Context {
    root_scope: Rc<RefCell<scope::Scope>>,
    scope: Rc<RefCell<scope::Scope>>,
    modules: Rc<RefCell<scope::Scope>>,
    trace: Rc<RefCell<trace::Trace>>,
}

impl Context {
    pub fn new(root_scope: Rc<RefCell<scope::Scope>>) -> Context {
        Context {
            root_scope: Rc::clone(&root_scope),
            scope: root_scope,
            modules: scope::Scope::new(),
            trace: Rc::new(RefCell::new(trace::Trace::new())),
        }
    }

    pub fn child(&mut self) -> Context {
        Context {
            root_scope: Rc::clone(&self.root_scope),
            scope: scope::Scope::child(&self.scope),
            modules: Rc::clone(&self.modules),
            trace: Rc::clone(&self.trace),
        }
    }

    pub fn overload(&mut self, scope: Rc<RefCell<scope::Scope>>) -> Context {
        Context {
            root_scope: Rc::clone(&self.root_scope),
            scope,
            modules: Rc::clone(&self.modules),
            trace: Rc::clone(&self.trace),
        }
    }

    pub fn scope(&self) -> Rc<RefCell<scope::Scope>> {
        Rc::clone(&self.scope)
    }

    pub fn trace(&mut self) -> Rc<RefCell<trace::Trace>> {
        Rc::clone(&self.trace)
    }

    pub fn load(&mut self, name: String) -> Result<RType, RError> {
        if let Some(module) = self.modules.borrow().get(&name) {
            return Ok((*module).clone());
        }

        if let Err(err) = self
            .modules
            .borrow_mut()
            .define(name.clone(), Rc::new(RType::nil()))
        {
            return Err(err);
        }

        match module::Module::load(
            &mut self.overload(Rc::clone(&self.root_scope)),
            name.clone(),
        ) {
            Ok(module) => {
                let module = Rc::new(RType::Func(Rc::new(RFunc::Binary(Box::new(module)))));
                match self.modules.borrow_mut().set(name, Rc::clone(&module)) {
                    Ok(_) => Ok((*module).clone()),
                    Err(err) => Err(err),
                }
            }
            Err(err) => Err(err),
        }
    }

    pub fn define_value(&self, name: String, value: Rc<RType>) -> Result<(), RError> {
        self.scope.borrow_mut().define(name, value)
    }

    pub fn set_value(&self, name: String, value: Rc<RType>) -> Result<(), RError> {
        self.scope.borrow_mut().set(name, value)
    }

    pub fn get_value(&self, name: &String) -> Option<Rc<RType>> {
        self.scope.borrow().get(name)
    }

    pub fn reset_trace(&mut self) {
        self.trace.borrow_mut().clear()
    }
}
