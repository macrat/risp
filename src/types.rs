use std::cell::RefCell;
use std::convert::From;
use std::fmt;
use std::ops::{Index, RangeFrom};
use std::rc::Rc;

use super::scope::Scope;

#[derive(Debug)]
pub enum RError {
    Type(String),
    AlreadyExist(String),
    NotExist(String),
    Argument(String),
}

impl fmt::Display for RError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RError::Type(reason) => write!(f, "TypeError: {}", reason),
            RError::AlreadyExist(reason) => write!(f, "AlreadyExistError: {}", reason),
            RError::NotExist(reason) => write!(f, "NotExistError: {}", reason),
            RError::Argument(reason) => write!(f, "ArgumentError: {}", reason),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RAtom {
    Symbol(String),
    Int(i64),
    String(String),
}

impl RAtom {
    fn parse(s: String) -> RAtom {
        // TODO: make way to parse string

        if let Ok(i) = s.parse::<i64>() {
            RAtom::Int(i)
        } else {
            RAtom::Symbol(s)
        }
    }

    fn compute(&self, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        match self {
            RAtom::Symbol(name) => match scope.borrow().get(name) {
                Some(val) => val.compute(Rc::clone(&scope)),
                None => Err(RError::NotExist(format!("`{}` does not exist.", name))),
            },
            _ => Ok(RType::Atom(self.clone())),
        }
    }
}

impl fmt::Display for RAtom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RAtom::Symbol(name) => write!(f, "{}", name),
            RAtom::Int(value) => write!(f, "{}", value),
            RAtom::String(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug)]
pub struct RList(Vec<RType>);

impl RList {
    pub fn new(items: Vec<RType>) -> RList {
        RList(items)
    }

    pub fn from(items: &[RType]) -> RList {
        RList::new(items.to_vec())
    }

    pub fn empty() -> RList {
        RList::new(Vec::new())
    }

    pub fn push(&mut self, val: RType) {
        self.0.push(val)
    }

    pub fn compute(&self, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        match self.0[0].compute(Rc::clone(&scope)) {
            Ok(first) => match &first {
                RType::Func(func) => func.call(RList(self.0[1..].to_vec()), scope),
                _ => Err(RError::Type(format!("`{}` is not a function.", first))),
            },
            Err(err) => Err(err),
        }
    }

    pub fn compute_each(&self, scope: Rc<RefCell<Scope>>) -> Result<RList, RError> {
        let mut result = RList::empty();
        for x in &self.0 {
            match x.compute(Rc::clone(&scope)) {
                Ok(x) => result.push(x),
                Err(err) => return Err(err),
            }
        }
        Ok(result)
    }

    pub fn to_bare_string(&self) -> String {
        let mut vec: Vec<String> = Vec::new();
        for x in &self.0 {
            vec.push(x.to_string());
        }
        vec.join(" ")
    }

    pub fn iter(&self) -> std::slice::Iter<RType> {
        self.0.iter()
    }
}

impl fmt::Display for RList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({})", self.to_bare_string())
    }
}

impl Clone for RList {
    fn clone(&self) -> RList {
        let mut result = RList::empty();
        for x in &self.0 {
            result.push((*x).clone());
        }
        result
    }
}

impl Index<usize> for RList {
    type Output = RType;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl Index<RangeFrom<usize>> for RList {
    type Output = [RType];

    fn index(&self, index: RangeFrom<usize>) -> &Self::Output {
        &self.0[index]
    }
}

pub trait Callable {
    fn name(&self) -> &str;

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError>;
}

impl fmt::Display for dyn Callable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl fmt::Debug for dyn Callable {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Callable({})", self)
    }
}

#[derive(Debug)]
pub enum RFunc {
    Pure { args: Vec<String>, body: RList },
    Binary(Box<dyn Callable>),
}

impl Callable for RFunc {
    fn name(&self) -> &str {
        match self {
            RFunc::Pure { args: _, body: _ } => "unnamed_function",
            RFunc::Binary(c) => c.name(),
        }
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        match self {
            RFunc::Pure {
                args: arg_names,
                body,
            } => {
                let scope = Scope::new(Some(scope));
                if args.0.len() != arg_names.len() {
                    return Err(RError::Argument(format!(
                        "this function needs {} arguments but got {} arguments.",
                        arg_names.len(),
                        args.0.len()
                    )));
                }

                for (name, value) in arg_names.iter().zip(args.0) {
                    if let Err(err) = (*scope)
                        .borrow_mut()
                        .define(String::from(name), Rc::new(value))
                    {
                        return Err(err);
                    }
                }

                match body.compute_each(scope) {
                    Ok(RList(result)) => {
                        if let Some(x) = result.last() {
                            Ok(x.clone())
                        } else {
                            Ok(RType::nil())
                        }
                    }
                    Err(err) => Err(err),
                }
            }
            RFunc::Binary(c) => c.call(args, scope),
        }
    }
}

impl fmt::Display for RFunc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RFunc::Pure { args, body } => {
                write!(f, "(func ({}) {})", args.join(" "), body.to_bare_string())
            }
            RFunc::Binary(c) => write!(f, "{}", *c),
        }
    }
}

#[derive(Debug)]
pub enum RType {
    Atom(RAtom),
    List(RList),
    Func(Rc<RFunc>),
}

impl RType {
    pub fn nil() -> RType {
        RType::List(RList::empty())
    }

    pub fn parse(s: String) -> RType {
        RType::Atom(RAtom::parse(s))
    }

    pub fn compute(&self, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        match self {
            RType::Atom(atom) => atom.compute(scope),
            RType::List(list) => list.compute(scope),
            RType::Func(func) => Ok(RType::Func(Rc::clone(func))),
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            RType::List(list) if list.0.len() == 0 => true,
            _ => false,
        }
    }
}

impl fmt::Display for RType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RType::Atom(atom) => atom.fmt(f),
            RType::List(list) => list.fmt(f),
            RType::Func(func) => func.fmt(f),
        }
    }
}

impl Clone for RType {
    fn clone(&self) -> RType {
        match self {
            RType::Atom(atom) => RType::Atom(atom.clone()),
            RType::List(list) => RType::List(list.clone()),
            RType::Func(func) => RType::Func(Rc::clone(func)),
        }
    }
}
