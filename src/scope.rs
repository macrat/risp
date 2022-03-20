use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::types::{RError, RType};

#[derive(Debug)]
pub struct Scope {
    pub parent: Option<Rc<RefCell<Scope>>>,
    values: HashMap<String, Rc<RType>>,
}

impl Scope {
    pub fn new(parent: Option<Rc<RefCell<Scope>>>) -> Rc<RefCell<Scope>> {
        Rc::new(RefCell::new(Scope {
            parent,
            values: HashMap::new(),
        }))
    }

    pub fn define(&mut self, name: String, value: Rc<RType>) -> Result<(), RError> {
        if self.values.contains_key(&name) {
            Err(RError::AlreadyExist(format!(
                "`{}` is already exist in this scope",
                name
            )))
        } else {
            self.values.insert(name, value);
            Ok(())
        }
    }

    pub fn set(&mut self, name: String, value: Rc<RType>) -> Result<(), RError> {
        if self.values.contains_key(&name) {
            self.values.insert(name, value);
            Ok(())
        } else {
            Err(RError::NotExist(format!(
                "`{}` is not exist in this scope",
                name
            )))
        }
    }

    pub fn get(&self, name: &String) -> Option<Rc<RType>> {
        let result = self.values.get(name);
        if let Some(x) = result {
            Some(Rc::clone(x))
        } else if let Some(parent) = &self.parent {
            (*parent).borrow().get(name)
        } else {
            None
        }
    }
}

impl fmt::Display for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut xs: Vec<String> = Vec::new();
        for (key, value) in &self.values {
            xs.push(format!("{} = {}", key, value));
        }
        write!(f, "{}", xs.join("\n"))
    }
}

impl Drop for Scope {
    fn drop(&mut self) {
        let keys: Vec<String> = self.values.keys().map(|x| x.to_string()).collect();
        for key in keys {
            self.values.remove(&key);
        }
    }
}
