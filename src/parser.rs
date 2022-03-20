use std::collections::VecDeque;
use std::mem;

use crate::types::{RError, RList, RType};

#[derive(Debug)]
pub struct Parser {
    buf: String,
    stack: Vec<RList>,
    queue: VecDeque<RType>,
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            buf: String::new(),
            stack: Vec::new(),
            queue: VecDeque::new(),
        }
    }

    pub fn feed(&mut self, text: &str) -> Result<(), RError> {
        for c in text.chars() {
            match c {
                '(' => {
                    if let Err(err) = self.flush() {
                        return Err(err);
                    }
                    self.stack.push(RList::empty());
                }
                ')' => {
                    if let Err(err) = self.flush() {
                        return Err(err);
                    }
                    self.pop_stack();
                }
                ' ' | '\t' | '\r' | '\n' => {
                    if let Err(err) = self.flush() {
                        return Err(err);
                    }
                }
                _ => self.buf.push(c),
            }
        }
        Ok(())
    }

    pub fn flush(&mut self) -> Result<(), RError> {
        if self.buf.len() == 0 {
            Ok(())
        } else {
            let buf = mem::replace(&mut self.buf, String::new());
            match RType::parse(buf) {
                Ok(x) => {
                    self.push_stack(x);
                    Ok(())
                }
                Err(err) => Err(err),
            }
        }
    }

    fn push_stack(&mut self, value: RType) {
        match self.stack.pop() {
            Some(mut top) => {
                top.push(value);
                self.stack.push(top);
            }
            None => self.queue.push_back(value),
        }
    }

    fn pop_stack(&mut self) {
        if let Some(top) = self.stack.pop() {
            self.push_stack(RType::List(top));
        }
    }

    pub fn is_completed(&self) -> bool {
        self.buf.len() == 0 && self.stack.len() == 0
    }

    pub fn pop(&mut self) -> Option<RType> {
        self.queue.pop_front()
    }
}

pub fn parse(text: &str) -> Result<RList, RError> {
    let mut p = Parser::new();

    if let Err(err) = p.feed(text) {
        return Err(err);
    }

    if let Err(err) = p.flush() {
        return Err(err);
    }

    if !p.is_completed() {
        return Err(RError::Incompleted(p.buf));
    }

    Ok(RList::new(p.queue.into()))
}
