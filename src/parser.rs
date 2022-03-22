use std::collections::VecDeque;

use crate::context::trace::Position;
use crate::types::{RAtom, RError, RList, RType};

#[derive(Debug)]
enum AtomBuilder {
    Symbol(String),
    Int(String),
    String {
        buf: String,
        escape: bool,
        completed: bool,
    },
    Nil,
}

impl AtomBuilder {
    fn new(c: char) -> AtomBuilder {
        let mut buf = String::new();

        if '0' <= c && c <= '9' {
            buf.push(c);
            AtomBuilder::Int(buf)
        } else if '"' == c {
            AtomBuilder::String {
                buf,
                escape: false,
                completed: false,
            }
        } else {
            buf.push(c);
            AtomBuilder::Symbol(buf)
        }
    }

    fn is_string(&self) -> bool {
        match self {
            AtomBuilder::String {
                buf: _,
                escape: _,
                completed: false,
            } => true,
            _ => false,
        }
    }

    fn push(&mut self, c: char) -> Result<bool, RError> {
        match self {
            AtomBuilder::Symbol(buf) => buf.push(c),
            AtomBuilder::Int(buf) => {
                buf.push(c);
                if c < '0' || '9' < c {
                    return Err(RError::InvalidLiteral(buf.clone()));
                }
            }
            AtomBuilder::String {
                buf,
                escape: _,
                completed: true,
            } => {
                buf.push(c);
                return Err(RError::InvalidLiteral(buf.clone()));
            }
            AtomBuilder::String {
                buf,
                escape,
                completed,
            } => {
                if *escape {
                    buf.push(match c {
                        'r' => '\r',
                        'n' => '\n',
                        't' => '\t',
                        '0' => '\0',
                        '\\' => '\\',
                        '"' => '"',
                        _ => return Err(RError::InvalidEscape(c)),
                    });
                    *escape = false;
                } else {
                    match c {
                        '\\' => *escape = true,
                        '"' => {
                            *completed = true;
                            return Ok(true);
                        }
                        _ => buf.push(c),
                    }
                }
            }
            AtomBuilder::Nil => {
                *self = AtomBuilder::new(c);
            }
        }
        Ok(false)
    }

    fn get_buf(&self) -> String {
        match self {
            AtomBuilder::Symbol(buf) => buf.clone(),
            AtomBuilder::Int(buf) => buf.clone(),
            AtomBuilder::String {
                buf,
                escape: _,
                completed: _,
            } => buf.clone(),
            AtomBuilder::Nil => String::new(),
        }
    }

    fn build(&mut self) -> Result<RAtom, RError> {
        let result = match self {
            AtomBuilder::Symbol(buf) => Ok(RAtom::Symbol(buf.clone())),
            AtomBuilder::Int(buf) => match buf.parse::<i64>() {
                Ok(i) => Ok(RAtom::Int(i)),
                Err(_) => Err(RError::InvalidLiteral(buf.clone())),
            },
            AtomBuilder::String {
                buf,
                escape: _,
                completed: false,
            } => Err(RError::Incompleted(buf.clone())),
            AtomBuilder::String {
                buf,
                escape: _,
                completed: true,
            } => Ok(RAtom::String(buf.clone())),
            AtomBuilder::Nil => Err(RError::InvalidLiteral(String::new())),
        };
        *self = AtomBuilder::Nil;
        result
    }
}

#[derive(Debug)]
pub struct Parser {
    position: Position,
    builder: AtomBuilder,
    stack: Vec<RList>,
    queue: VecDeque<RType>,
}

impl Parser {
    pub fn new(file_name: String) -> Parser {
        Parser {
            position: Position {
                file: file_name,
                line: 1,
                col: 0,
            },
            builder: AtomBuilder::Nil,
            stack: Vec::new(),
            queue: VecDeque::new(),
        }
    }

    pub fn feed(&mut self, text: &str) -> Result<(), RError> {
        for c in text.chars() {
            if c == '\n' {
                self.position.line += 1;
                self.position.col = 0;
            } else {
                self.position.col += 1;
            }

            match c {
                '(' if !self.builder.is_string() => {
                    if let Err(err) = self.flush() {
                        return Err(err);
                    }
                    self.stack.push(RList::empty(Some(self.position.clone())));
                }
                ')' if !self.builder.is_string() => {
                    if let Err(err) = self.flush() {
                        return Err(err);
                    }
                    self.pop_stack();
                }
                ' ' | '\t' if !self.builder.is_string() => {
                    if let Err(err) = self.flush() {
                        return Err(err);
                    }
                }
                '\r' | '\n' => {
                    if let Err(err) = self.flush() {
                        return Err(err);
                    }
                }
                _ => match self.builder.push(c) {
                    Ok(true) => {
                        if let Err(err) = self.flush() {
                            return Err(err);
                        }
                    }
                    Ok(false) => {}
                    Err(err) => return Err(err),
                },
            }
        }
        Ok(())
    }

    fn flush(&mut self) -> Result<(), RError> {
        if let AtomBuilder::Nil = self.builder {
            Ok(())
        } else {
            match self.builder.build() {
                Ok(atom) => {
                    self.push_stack(RType::Atom(atom));
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
        if let AtomBuilder::Nil = self.builder {
            self.stack.len() == 0
        } else {
            false
        }
    }

    pub fn pop(&mut self) -> Option<RType> {
        self.queue.pop_front()
    }

    pub fn close(&mut self) -> Result<usize, RError> {
        if let Err(err) = self.flush() {
            return Err(err);
        }

        if !self.is_completed() {
            return Err(RError::Incompleted(self.builder.get_buf()));
        }

        Ok(self.stack.len())
    }

    pub fn reset(&mut self) {
        *self = Parser::new(self.position.file.clone());
    }
}

#[cfg(test)]
pub mod test {
    use super::*;

    pub fn parse(text: &str) -> Result<RList, RError> {
        let mut p = Parser::new("<test>".into());

        if let Err(err) = p.feed(text) {
            return Err(err);
        }

        if let Err(err) = p.close() {
            return Err(err);
        }

        Ok(RList::new(p.queue.into(), None))
    }

    fn assert_atom(expect: RAtom, code: &str) {
        match parse(code) {
            Ok(val) => {
                assert_eq!(1, val.len());
                assert_eq!(RType::Atom(expect), val[0]);
            }
            Err(err) => panic!("{}", err),
        }
    }

    fn assert_err(expect: RError, code: &str) {
        match parse(code) {
            Ok(val) => panic!("expected error `{}` but got {}", expect, val),
            Err(err) => assert_eq!(expect, err),
        }
    }

    fn assert_symbol(expect: &str, code: &str) {
        assert_atom(RAtom::Symbol(String::from(expect)), code)
    }

    fn assert_str(expect: &str, code: &str) {
        assert_atom(RAtom::String(String::from(expect)), code)
    }

    #[test]
    fn symbol() {
        assert_symbol("hello", "hello");
        assert_symbol("hello-world", " hello-world ");
    }

    #[test]
    fn int() {
        assert_atom(RAtom::Int(1), "1");
        assert_atom(RAtom::Int(2), " 2 ");
        assert_atom(RAtom::Int(42), " 42 ");

        assert_err(RError::InvalidLiteral(String::from("0h")), "0hello");
        assert_err(RError::InvalidLiteral(String::from("123_")), "123_456");
    }

    #[test]
    fn string() {
        assert_str("hello world", r#"  "hello world"  "#);
        assert_str(" ", r#"  " "  "#);
        assert_str("\t\r\n\\", r#"  "\t\r\n\\"  "#);
        assert_str("hello (world)", r#"  "hello (world)"  "#);

        assert_err(RError::InvalidEscape('a'), r#""\a""#);
        assert_err(RError::Incompleted(String::from("hello")), "\"hello\n");
        assert_err(
            RError::Incompleted(String::from("hello\"  ")),
            r#"  "hello\"  "#,
        );
    }
}
