use std::cmp::Ordering;
use std::convert::From;
use std::fmt;
use std::ops::{Index, RangeFrom};
use std::rc::Rc;

use crate::env::{trace::Position, Env};
use crate::scope::Scope;

#[derive(Debug, PartialEq)]
pub enum RError {
    System(String, String),
    User(RValue),
}

fn escape_string(s: &String) -> String {
    let s = format!("{:?}", s);
    s[1..s.len() - 1].to_string()
}

impl RError {
    pub fn type_(reason: String) -> RError {
        RError::System("incompatible type".to_string(), reason)
    }

    pub fn already_exist(name: String) -> RError {
        RError::System(
            "already exist".to_string(),
            format!("{} is already exist.", name),
        )
    }

    pub fn not_exist(name: String) -> RError {
        RError::System("not exist".to_string(), format!("{} does not exist.", name))
    }

    pub fn argument(reason: String) -> RError {
        RError::System("invalid argument".to_string(), reason)
    }

    pub fn incompleted(buf: String) -> RError {
        RError::System(
            "incompleted expression".to_string(),
            if buf == "" {
                "expression is not completed".to_string()
            } else {
                format!("expression is not completed: {}", escape_string(&buf))
            },
        )
    }

    pub fn invalid_literal(literal: String) -> RError {
        RError::System(
            "invalid literal".to_string(),
            format!("{} is invalid literal", escape_string(&literal),),
        )
    }

    pub fn invalid_escape(c: char) -> RError {
        RError::System(
            "invalid escape".to_string(),
            format!(r#""\{}" is invalid escape in string."#, c,),
        )
    }

    pub fn io(reason: String) -> RError {
        RError::System("io".to_string(), reason)
    }
}

impl fmt::Display for RError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RError::System(category, detail) => write!(f, "{}: {}", category, detail),
            RError::User(obj) => write!(f, "{}", obj),
        }
    }
}

impl From<RValue> for RError {
    fn from(val: RValue) -> RError {
        RError::User(val)
    }
}

impl From<RError> for RValue {
    fn from(err: RError) -> RValue {
        match err {
            RError::System(category, detail) => RValue::List(RList::from(
                &[
                    RValue::Atom(RAtom::String(category)),
                    RValue::Atom(RAtom::String(detail)),
                ],
                None,
            )),
            RError::User(obj) => obj.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum RAtom {
    Symbol(String),
    Number(f64),
    String(String),
}

impl RAtom {
    fn compute(&self, _: &mut Env, scope: &Scope) -> Result<RValue, RError> {
        match self {
            RAtom::Symbol(name) => scope.get(name),
            _ => Ok(RValue::Atom(self.clone())),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            RAtom::Symbol(_) => true,
            RAtom::Number(n) => *n != 0.0,
            RAtom::String(s) => s.len() != 0,
        }
    }

    pub fn to_printable(&self) -> String {
        match self {
            RAtom::Symbol(name) => name.clone(),
            RAtom::Number(value) => format!("{}", value),
            RAtom::String(value) => format!("{}", value),
        }
    }
}

impl fmt::Display for RAtom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RAtom::Symbol(name) => write!(f, "{}", name),
            RAtom::Number(value) => write!(f, "{}", value),
            RAtom::String(value) => write!(f, "{:?}", value),
        }
    }
}

#[derive(Debug)]
pub struct RList(Vec<RValue>, Option<Position>);

impl RList {
    pub fn new(items: Vec<RValue>, pos: Option<Position>) -> RList {
        RList(items, pos)
    }

    pub fn from(items: &[RValue], pos: Option<Position>) -> RList {
        RList::new(items.to_vec(), pos)
    }

    pub fn empty(pos: Option<Position>) -> RList {
        RList::new(Vec::new(), pos)
    }

    pub fn push(&mut self, val: RValue) {
        self.0.push(val)
    }

    pub fn compute(&self, env: &mut Env, scope: &Scope) -> Result<RValue, RError> {
        if self.0.len() == 0 {
            return Ok(RValue::nil());
        }

        env.trace.push(self.clone());
        match self.0[0].compute(env, scope) {
            Ok(first) => match &first {
                RValue::Func(func) => {
                    match func.call(env, scope, RList::from(&self.0[1..], None)) {
                        Ok(x) => {
                            env.trace.pop();
                            Ok(x)
                        }
                        Err(err) => Err(err),
                    }
                }
                _ => Err(RError::type_(format!("`{}` is not a function.", first))),
            },
            Err(err) => Err(err),
        }
    }

    pub fn compute_last(&self, env: &mut Env, scope: &Scope) -> Result<RValue, RError> {
        let mut result = RValue::nil();
        for x in &self.0 {
            match x.compute(env, scope) {
                Ok(x) => {
                    result = x;
                }
                Err(err) => return Err(err),
            }
        }
        Ok(result)
    }

    pub fn compute_each(&self, env: &mut Env, scope: &Scope) -> Result<RList, RError> {
        let mut result = RList::empty(None);
        for x in &self.0 {
            match x.compute(env, scope) {
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

    pub fn to_bare_printable(&self) -> String {
        let mut vec: Vec<String> = Vec::new();
        for x in &self.0 {
            vec.push(x.to_printable());
        }
        vec.join(" ")
    }

    pub fn to_printable(&self) -> String {
        format!("({})", self.to_bare_printable())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> std::slice::Iter<RValue> {
        self.0.iter()
    }

    pub fn cmp(&self, other: &RList) -> Result<Ordering, RError> {
        for (x, y) in self.iter().zip(other.iter()) {
            match x.cmp(y) {
                Ok(Ordering::Equal) => {}
                Ok(x) => return Ok(x),
                Err(err) => return Err(err),
            }
        }
        Ok(self.len().cmp(&other.len()))
    }

    pub fn as_bool(&self) -> bool {
        self.len() != 0
    }

    pub fn position(&self) -> Option<Position> {
        match &self.1 {
            Some(x) => Some(x.clone()),
            None => None,
        }
    }
}

impl fmt::Display for RList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({})", self.to_bare_string())
    }
}

impl Clone for RList {
    fn clone(&self) -> RList {
        let mut result = RList::empty(self.1.clone());
        for x in &self.0 {
            result.push((*x).clone());
        }
        result
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
    type Output = RValue;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl Index<RangeFrom<usize>> for RList {
    type Output = [RValue];

    fn index(&self, index: RangeFrom<usize>) -> &Self::Output {
        &self.0[index]
    }
}

pub trait Callable {
    fn name(&self) -> &str;

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError>;
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
    Pure {
        args: Vec<String>,
        body: RList,
        capture: Scope,
    },
    Binary(Box<dyn Callable>),
}

impl RFunc {
    pub fn to_printable(&self) -> String {
        self.to_string()
    }
}

impl Callable for RFunc {
    fn name(&self) -> &str {
        match self {
            RFunc::Pure { .. } => "pure-func",
            RFunc::Binary(c) => c.name(),
        }
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        match self {
            RFunc::Pure {
                args: arg_names,
                body,
                capture,
            } => {
                if args.0.len() != arg_names.len() {
                    return Err(RError::argument(format!(
                        "this function needs {} arguments but got {} arguments.",
                        arg_names.len(),
                        args.0.len()
                    )));
                }

                let local = capture.child();
                for (name, value) in arg_names.iter().zip(args.0) {
                    local.define(String::from(name), value.compute(env, scope)?)?;
                }

                body.compute_last(env, &local)
            }
            RFunc::Binary(c) => c.call(env, scope, args),
        }
    }
}

impl fmt::Display for RFunc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RFunc::Pure { args, body, .. } => {
                if body.len() > 0 {
                    write!(f, "(func ({}) {})", args.join(" "), body.to_bare_string())
                } else {
                    write!(f, "(func ({}))", args.join(" "))
                }
            }
            RFunc::Binary(c) => write!(f, "{}", *c),
        }
    }
}

#[derive(Debug)]
pub enum RValue {
    Atom(RAtom),
    List(RList),
    Func(Rc<RFunc>),
}

impl RValue {
    pub fn nil() -> RValue {
        RValue::List(RList::empty(None))
    }

    pub fn compute(&self, env: &mut Env, scope: &Scope) -> Result<RValue, RError> {
        match self {
            RValue::Atom(atom) => atom.compute(env, scope),
            RValue::List(list) => list.compute(env, scope),
            RValue::Func(func) => Ok(RValue::Func(Rc::clone(func))),
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            RValue::List(list) if list.0.len() == 0 => true,
            _ => false,
        }
    }

    pub fn cmp(&self, other: &RValue) -> Result<Ordering, RError> {
        match (self, other) {
            (RValue::Atom(RAtom::Number(x)), RValue::Atom(RAtom::Number(y))) => {
                if let Some(ord) = x.partial_cmp(y) {
                    Ok(ord)
                } else {
                    Err(RError::type_(format!(
                        "`{}` and `{}` are not comparable.",
                        self, other
                    )))
                }
            }
            (RValue::Atom(RAtom::String(x)), RValue::Atom(RAtom::String(y))) => Ok(x.cmp(y)),
            (RValue::List(x), RValue::List(y)) => x.cmp(y),
            (_, _) => Err(RError::type_(format!(
                "`{}` and `{}` are not comparable.",
                self, other
            ))),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            RValue::Atom(atom) => atom.as_bool(),
            RValue::List(list) => list.as_bool(),
            RValue::Func(_) => true,
        }
    }

    pub fn to_printable(&self) -> String {
        match self {
            RValue::Atom(atom) => atom.to_printable(),
            RValue::List(list) => list.to_printable(),
            RValue::Func(func) => func.to_printable(),
        }
    }
}

impl fmt::Display for RValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RValue::Atom(atom) => atom.fmt(f),
            RValue::List(list) => list.fmt(f),
            RValue::Func(func) => func.fmt(f),
        }
    }
}

impl Clone for RValue {
    fn clone(&self) -> RValue {
        match self {
            RValue::Atom(atom) => RValue::Atom(atom.clone()),
            RValue::List(list) => RValue::List(list.clone()),
            RValue::Func(func) => RValue::Func(Rc::clone(func)),
        }
    }
}

impl std::cmp::PartialEq for RValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RValue::Atom(a), RValue::Atom(b)) => a == b,
            (RValue::List(a), RValue::List(b)) => a == b,
            (RValue::Func(a), RValue::Func(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}
