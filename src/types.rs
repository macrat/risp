use std::cell::RefCell;
use std::cmp::Ordering;
use std::convert::From;
use std::fmt;
use std::ops::{Index, RangeFrom};
use std::rc::Rc;

use crate::context::{scope::Scope, trace::Position, Context};

#[derive(Debug, PartialEq)]
pub enum RError {
    Type(String),
    AlreadyExist(String),
    NotExist(String),
    Argument(String),
    Incompleted(String),
    InvalidLiteral(String),
    InvalidEscape(char),
    IO(String),
    User(RType),
}

fn escape_string(s: &String) -> String {
    let s = format!("{:?}", s);
    s[1..s.len() - 1].to_string()
}

impl fmt::Display for RError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RError::Type(reason) => write!(f, "TypeError: {}", reason),
            RError::AlreadyExist(name) => {
                write!(f, "AlreadyExistError: {} is already exist.", name)
            }
            RError::NotExist(name) => write!(f, "NotExistError: {} does not exist.", name),
            RError::Argument(reason) => write!(f, "ArgumentError: {}", reason),
            RError::Incompleted(buf) => {
                write!(
                    f,
                    "IncompletedError: expression is not completed: {}",
                    escape_string(buf)
                )
            }
            RError::InvalidLiteral(literal) => write!(
                f,
                "InvalidLiteralError: {} is invalid literal.",
                escape_string(literal),
            ),
            RError::InvalidEscape(c) => write!(
                f,
                r#"InvalidEscapeError: "\{}" is invalid escape in string."#,
                c
            ),
            RError::IO(reason) => write!(f, "IOError: {}", reason),
            RError::User(message) => write!(f, "UserError: {}", message.to_string()),
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
    fn compute(&self, ctx: &mut Context) -> Result<RType, RError> {
        match self {
            RAtom::Symbol(name) => match ctx.get_value(name) {
                Some(val) => Ok((*val).clone()),
                None => Err(RError::NotExist(name.to_string())),
            },
            _ => Ok(RType::Atom(self.clone())),
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
pub struct RList(Vec<RType>, Option<Position>);

impl RList {
    pub fn new(items: Vec<RType>, pos: Option<Position>) -> RList {
        RList(items, pos)
    }

    pub fn from(items: &[RType], pos: Option<Position>) -> RList {
        RList::new(items.to_vec(), pos)
    }

    pub fn empty(pos: Option<Position>) -> RList {
        RList::new(Vec::new(), pos)
    }

    pub fn push(&mut self, val: RType) {
        self.0.push(val)
    }

    pub fn compute(&self, ctx: &mut Context) -> Result<RType, RError> {
        if self.0.len() == 0 {
            return Ok(RType::nil());
        }

        ctx.trace().borrow_mut().push(self.clone());
        match self.0[0].compute(ctx) {
            Ok(first) => match &first {
                RType::Func(func) => match func.call(ctx, RList::from(&self.0[1..], None)) {
                    Ok(x) => {
                        ctx.trace().borrow_mut().pop();
                        Ok(x)
                    }
                    Err(err) => Err(err),
                },
                _ => Err(RError::Type(format!("`{}` is not a function.", first))),
            },
            Err(err) => Err(err),
        }
    }

    pub fn compute_last(&self, ctx: &mut Context) -> Result<RType, RError> {
        let mut result = RType::nil();
        for x in &self.0 {
            match x.compute(ctx) {
                Ok(x) => {
                    result = x;
                }
                Err(err) => return Err(err),
            }
        }
        Ok(result)
    }

    pub fn compute_each(&self, ctx: &mut Context) -> Result<RList, RError> {
        let mut result = RList::empty(None);
        for x in &self.0 {
            match x.compute(ctx) {
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

    pub fn iter(&self) -> std::slice::Iter<RType> {
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

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError>;
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
        capture: Rc<RefCell<Scope>>,
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

    fn call(&self, ctx: &mut Context, args: RList) -> Result<RType, RError> {
        match self {
            RFunc::Pure {
                args: arg_names,
                body,
                capture,
            } => {
                if args.0.len() != arg_names.len() {
                    return Err(RError::Argument(format!(
                        "this function needs {} arguments but got {} arguments.",
                        arg_names.len(),
                        args.0.len()
                    )));
                }

                let local = Scope::child(capture);
                for (name, value) in arg_names.iter().zip(args.0) {
                    match value.compute(ctx) {
                        Ok(value) => {
                            if let Err(err) = local
                                .borrow_mut()
                                .define(String::from(name), Rc::new(value))
                            {
                                return Err(err);
                            }
                        }
                        Err(err) => return Err(err),
                    }
                }

                match body.compute_last(&mut ctx.overload(local)) {
                    Ok(result) => Ok(result),
                    Err(err) => Err(err),
                }
            }
            RFunc::Binary(c) => c.call(ctx, args),
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
pub enum RType {
    Atom(RAtom),
    List(RList),
    Func(Rc<RFunc>),
}

impl RType {
    pub fn nil() -> RType {
        RType::List(RList::empty(None))
    }

    pub fn compute(&self, ctx: &mut Context) -> Result<RType, RError> {
        match self {
            RType::Atom(atom) => atom.compute(ctx),
            RType::List(list) => list.compute(ctx),
            RType::Func(func) => Ok(RType::Func(Rc::clone(func))),
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            RType::List(list) if list.0.len() == 0 => true,
            _ => false,
        }
    }

    pub fn cmp(&self, other: &RType) -> Result<Ordering, RError> {
        match (self, other) {
            (RType::Atom(RAtom::Number(x)), RType::Atom(RAtom::Number(y))) => {
                if let Some(ord) = x.partial_cmp(y) {
                    Ok(ord)
                } else {
                    Err(RError::Type(format!(
                        "`{}` and `{}` are not comparable.",
                        self, other
                    )))
                }
            }
            (RType::Atom(RAtom::String(x)), RType::Atom(RAtom::String(y))) => Ok(x.cmp(y)),
            (RType::List(x), RType::List(y)) => x.cmp(y),
            (_, _) => Err(RError::Type(format!(
                "`{}` and `{}` are not comparable.",
                self, other
            ))),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            RType::Atom(atom) => atom.as_bool(),
            RType::List(list) => list.as_bool(),
            RType::Func(_) => true,
        }
    }

    pub fn to_printable(&self) -> String {
        match self {
            RType::Atom(atom) => atom.to_printable(),
            RType::List(list) => list.to_printable(),
            RType::Func(func) => func.to_printable(),
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
