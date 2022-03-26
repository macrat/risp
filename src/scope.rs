use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::types::{RError, RValue};

struct Internal {
    pub parent: Option<Scope>,
    values: HashMap<String, RValue>,
}

impl Drop for Internal {
    fn drop(&mut self) {
        let keys: Vec<String> = self.values.keys().map(|x| x.to_string()).collect();
        for key in keys {
            self.values.remove(&key);
        }
    }
}

impl Internal {
    pub fn define(&mut self, name: String, value: RValue) -> Result<(), RError> {
        if self.values.contains_key(&name) {
            Err(RError::AlreadyExist(name))
        } else {
            self.values.insert(name, value);
            Ok(())
        }
    }

    pub fn set(&mut self, name: String, value: RValue) -> Result<(), RError> {
        if self.values.contains_key(&name) {
            self.values.insert(name, value);
            Ok(())
        } else if let Some(parent) = &mut self.parent {
            parent.set(name, value)
        } else {
            Err(RError::NotExist(name))
        }
    }

    pub fn get(&self, name: &String) -> Result<RValue, RError> {
        if let Some(x) = self.values.get(name) {
            Ok(x.clone())
        } else if let Some(parent) = &self.parent {
            parent.get(name)
        } else {
            Err(RError::NotExist(name.clone()))
        }
    }
}

#[derive(Clone)]
pub struct Scope(Rc<RefCell<Internal>>);

impl Scope {
    pub fn new() -> Scope {
        Scope(Rc::new(RefCell::new(Internal {
            parent: None,
            values: HashMap::new(),
        })))
    }

    pub fn child(&self) -> Scope {
        Scope(Rc::new(RefCell::new(Internal {
            parent: Some(self.clone()),
            values: HashMap::new(),
        })))
    }

    pub fn root(&self) -> Scope {
        if let Some(parent) = &self.0.borrow().parent {
            parent.root()
        } else {
            self.clone()
        }
    }

    pub fn define(&self, name: String, value: RValue) -> Result<(), RError> {
        self.0.borrow_mut().define(name, value)
    }

    pub fn set(&self, name: String, value: RValue) -> Result<(), RError> {
        self.0.borrow_mut().set(name, value)
    }

    pub fn get(&self, name: &String) -> Result<RValue, RError> {
        self.0.borrow().get(name)
    }
}

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Scope:\n{}", self)
    }
}

impl fmt::Display for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut xs: Vec<String> = Vec::new();
        for (key, value) in &self.0.borrow().values {
            xs.push(format!("{} = {}", key, value));
        }
        write!(f, "{}", xs.join("\n"))
    }
}
