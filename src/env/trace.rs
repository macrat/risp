use std::convert::From;
use std::fmt;

use crate::types::{RAtom, RError, RList, RValue};

#[derive(Debug, Clone)]
pub struct Position {
    pub file: String,
    pub line: u32,
    pub col: u32,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.col)
    }
}

#[derive(Debug)]
pub struct Trace {
    stack: Vec<RList>,
}

impl Trace {
    pub fn new() -> Trace {
        Trace { stack: Vec::new() }
    }

    pub fn push(&mut self, item: RList) {
        self.stack.push(item);
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }

    pub fn save(&self) -> usize {
        self.stack.len()
    }

    pub fn restore(&mut self, saved: usize) {
        self.stack.truncate(saved);
    }

    pub fn position(&self) -> Option<Position> {
        for x in self.stack.iter().rev() {
            if let Some(x) = x.position() {
                return Some(x);
            }
        }
        None
    }

    pub fn print(&self, err: RError) {
        for x in &self.stack {
            match x.position() {
                Some(pos) => println!("! {} {}", pos, x),
                None => println!("! <dynamic>:0:0 {}", x),
            }
        }
        println!("! {}", err);
    }

    pub fn clear(&mut self) {
        self.stack.clear()
    }
}

impl From<&Trace> for RValue {
    fn from(trace: &Trace) -> RValue {
        let mut list = RList::empty(None);
        for x in &trace.stack {
            let (file, line, col_) = match x.position() {
                Some(pos) => (pos.file, pos.line, pos.col),
                None => ("<dynamic>".to_string(), 0, 0),
            };
            list.push(RValue::List(RList::from(
                &[
                    RValue::Atom(RAtom::String(file)),
                    RValue::Atom(RAtom::Number(line.into())),
                    RValue::Atom(RAtom::Number(col_.into())),
                    RValue::Atom(RAtom::String(x.to_string())),
                ],
                None,
            )));
        }
        RValue::List(list)
    }
}
