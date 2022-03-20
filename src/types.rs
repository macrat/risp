use std::cell::RefCell;
use std::convert::From;
use std::fmt;
use std::ops::{Index, RangeFrom};
use std::rc::Rc;

use crate::scope::Scope;

#[derive(Debug, PartialEq)]
pub enum RError {
    Type(String),
    AlreadyExist(String),
    NotExist(String),
    Argument(String),
    Incompleted(String),
    InvalidSymbol(String),
}

impl fmt::Display for RError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RError::Type(reason) => write!(f, "TypeError: {}", reason),
            RError::AlreadyExist(name) => {
                write!(f, "AlreadyExistError: `{}` is already exist.", name)
            }
            RError::NotExist(name) => write!(f, "NotExistError: `{}` does not exist.", name),
            RError::Argument(reason) => write!(f, "ArgumentError: {}", reason),
            RError::Incompleted(buf) => {
                write!(f, "IncompletedError: expression is not completed: {}", buf)
            }
            RError::InvalidSymbol(c) => write!(f, "InvalidSymbolError: `{}` is invalid symbol.", c),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum RAtom {
    Symbol(String),
    Int(i64),
    String(String),
}

fn is_valid_symbol(s: &String) -> bool {
    if let Some(c) = s.chars().next() {
        !('0' <= c && c <= '9')
    } else {
        false
    }
}

impl RAtom {
    fn parse(s: String) -> Result<RAtom, RError> {
        // TODO: make way to parse string

        if s.len() == 0 {
            Err(RError::InvalidSymbol(String::new()))
        } else if let Ok(i) = s.parse::<i64>() {
            Ok(RAtom::Int(i))
        } else if !is_valid_symbol(&s) {
            Err(RError::InvalidSymbol(s))
        } else {
            Ok(RAtom::Symbol(s))
        }
    }

    fn compute(&self, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        match self {
            RAtom::Symbol(name) => match scope.borrow().get(name) {
                Some(val) => Ok((*val).clone()),
                None => Err(RError::NotExist(name.to_string())),
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

impl From<RAtom> for bool {
    fn from(item: RAtom) -> bool {
        match item {
            RAtom::Symbol(_) => true,
            RAtom::Int(i) => i != 0,
            RAtom::String(s) => s.len() != 0,
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
        if self.0.len() == 0 {
            return Ok(RType::nil());
        }

        match self.0[0].compute(Rc::clone(&scope)) {
            Ok(first) => match &first {
                RType::Func(func) => func.call(RList(self.0[1..].to_vec()), scope),
                _ => Err(RError::Type(format!("`{}` is not a function.", first))),
            },
            Err(err) => Err(err),
        }
    }

    pub fn compute_last(&self, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        let mut result = RType::nil();
        for x in &self.0 {
            match x.compute(Rc::clone(&scope)) {
                Ok(x) => {
                    result = x;
                }
                Err(err) => return Err(err),
            }
        }
        Ok(result)
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

    pub fn len(&self) -> usize {
        self.0.len()
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

impl From<RList> for bool {
    fn from(item: RList) -> bool {
        item.len() != 0
    }
}

impl std::cmp::PartialEq for RList {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }

        for (x, y) in self.iter().zip(other.iter()) {
            if *x != *y {
                return false;
            }
        }

        true
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
                let local = Scope::new(Some(Rc::clone(&scope)));
                if args.0.len() != arg_names.len() {
                    return Err(RError::Argument(format!(
                        "this function needs {} arguments but got {} arguments.",
                        arg_names.len(),
                        args.0.len()
                    )));
                }

                for (name, value) in arg_names.iter().zip(args.0) {
                    match value.compute(Rc::clone(&scope)) {
                        Ok(value) => {
                            if let Err(err) = (*local)
                                .borrow_mut()
                                .define(String::from(name), Rc::new(value))
                            {
                                return Err(err);
                            }
                        }
                        Err(err) => return Err(err),
                    }
                }

                match body.compute_last(local) {
                    Ok(result) => Ok(result),
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

    pub fn parse(s: String) -> Result<RType, RError> {
        match RAtom::parse(s) {
            Ok(x) => Ok(RType::Atom(x)),
            Err(err) => Err(err),
        }
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

impl From<RType> for bool {
    fn from(item: RType) -> bool {
        match item {
            RType::Atom(atom) => atom.into(),
            RType::List(list) => list.into(),
            RType::Func(_) => true,
        }
    }
}

impl std::cmp::PartialEq for RType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RType::Atom(a), RType::Atom(b)) => a == b,
            (RType::List(a), RType::List(b)) => a == b,
            (RType::Func(a), RType::Func(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_int() {
        assert_eq!(RAtom::parse(String::from("42")), Ok(RAtom::Int(42)));
        assert_eq!(RAtom::parse(String::from("-1")), Ok(RAtom::Int(-1)));
        assert_eq!(RAtom::parse(String::from("0")), Ok(RAtom::Int(0)));
    }

    #[test]
    fn parse_symbol() {
        assert_eq!(
            RAtom::parse(String::from("hello")),
            Ok(RAtom::Symbol(String::from("hello")))
        );
        assert_eq!(
            RAtom::parse(String::from("A")),
            Ok(RAtom::Symbol(String::from("A")))
        );
        assert_eq!(
            RAtom::parse(String::from("日本語")),
            Ok(RAtom::Symbol(String::from("日本語")))
        );
    }

    #[test]
    fn parse_invalid() {
        assert_eq!(
            RAtom::parse(String::from("")),
            Err(RError::InvalidSymbol(String::from("")))
        );
        assert_eq!(
            RAtom::parse(String::from("1hello")),
            Err(RError::InvalidSymbol(String::from("1hello")))
        );
    }
}
