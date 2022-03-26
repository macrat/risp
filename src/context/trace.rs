use std::fmt;

use crate::types::{RError, RList};

#[derive(Debug, Clone)]
pub struct Position {
    pub file: String,
    pub line: u64,
    pub col: u64,
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
