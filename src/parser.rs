use std::collections::VecDeque;

use crate::env::trace::Position;
use crate::types::{RAtom, RError, RList, RValue};

#[derive(Debug)]
enum AtomBuilder {
    Symbol(String),
    Number(String),
    String {
        buf: String,
        escape: bool,
        completed: bool,
    },
    Invalid(String),
    Nil,
}

impl AtomBuilder {
    fn new(c: char) -> AtomBuilder {
        let mut buf = String::new();

        if '0' <= c && c <= '9' || c == '-' {
            buf.push(c);
            AtomBuilder::Number(buf)
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
            AtomBuilder::Number(buf) => {
                buf.push(c);
                if (c < '0' || '9' < c) && c != '.' {
                    *self = if buf.chars().nth(0) == Some('-') && buf.len() == 2 {
                        AtomBuilder::Symbol(buf.clone())
                    } else {
                        AtomBuilder::Invalid(buf.clone())
                    }
                }
            }
            AtomBuilder::String {
                buf,
                escape: _,
                completed: true,
            } => {
                buf.push(c);
                return Err(RError::invalid_literal(buf.clone()));
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
                        _ => return Err(RError::invalid_escape(c)),
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
            AtomBuilder::Invalid(buf) => buf.push(c),
            AtomBuilder::Nil => {
                *self = AtomBuilder::new(c);
            }
        }
        Ok(false)
    }

    fn get_buf(&self) -> String {
        match self {
            AtomBuilder::Symbol(buf) => buf.clone(),
            AtomBuilder::Number(buf) => buf.clone(),
            AtomBuilder::String {
                buf,
                escape: _,
                completed: _,
            } => buf.clone(),
            AtomBuilder::Invalid(buf) => buf.clone(),
            AtomBuilder::Nil => String::new(),
        }
    }

    fn build(&mut self) -> Result<RAtom, RError> {
        let result = match self {
            AtomBuilder::Symbol(buf) => Ok(RAtom::Symbol(buf.clone())),
            AtomBuilder::Number(buf) if buf == "-" => Ok(RAtom::Symbol(buf.clone())),
            AtomBuilder::Number(buf) => match buf.parse::<f64>() {
                Ok(n) => Ok(n.into()),
                Err(_) => Err(RError::invalid_literal(buf.clone())),
            },
            AtomBuilder::String {
                buf,
                escape: _,
                completed: false,
            } => Err(RError::incompleted(buf.clone())),
            AtomBuilder::String {
                buf,
                escape: _,
                completed: true,
            } => Ok(buf.clone().into()),
            AtomBuilder::Invalid(buf) => Err(RError::invalid_literal(buf.clone())),
            AtomBuilder::Nil => Err(RError::invalid_literal(String::new())),
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
    queue: VecDeque<RValue>,
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

    pub fn feed_char(&mut self, c: char) -> Result<(), RError> {
        if c == '\n' {
            self.position.line += 1;
            self.position.col = 0;
        } else {
            self.position.col += 1;
        }

        match c {
            '(' if !self.builder.is_string() => {
                self.flush()?;
                self.stack.push(RList::empty(Some(self.position.clone())));
            }
            ')' if !self.builder.is_string() => {
                self.flush()?;
                self.pop_stack()?;
            }
            ' ' | '\t' if !self.builder.is_string() => {
                self.flush()?;
            }
            '\r' | '\n' => {
                self.flush()?;
            }
            _ => {
                if self.builder.push(c)? {
                    self.flush()?;
                }
            }
        }

        Ok(())
    }

    pub fn feed(&mut self, text: &str) -> Result<(), RError> {
        for c in text.chars() {
            self.feed_char(c)?;
        }
        Ok(())
    }

    fn flush(&mut self) -> Result<(), RError> {
        if let AtomBuilder::Nil = self.builder {
            Ok(())
        } else {
            match self.builder.build() {
                Ok(atom) => {
                    self.push_stack(RValue::Atom(atom));
                    Ok(())
                }
                Err(err) => Err(err),
            }
        }
    }

    fn push_stack(&mut self, value: RValue) {
        match self.stack.pop() {
            Some(mut top) => {
                top.push(value);
                self.stack.push(top);
            }
            None => self.queue.push_back(value),
        }
    }

    fn pop_stack(&mut self) -> Result<(), RError> {
        if let Some(top) = self.stack.pop() {
            self.push_stack(RValue::List(top));
            Ok(())
        } else {
            Err(RError::incompleted(String::new()))
        }
    }

    pub fn is_completed(&self) -> bool {
        if let AtomBuilder::Nil = self.builder {
            self.stack.len() == 0
        } else {
            false
        }
    }

    pub fn pop(&mut self) -> Option<RValue> {
        self.queue.pop_front()
    }

    pub fn close(&mut self) -> Result<usize, RError> {
        if let Err(err) = self.flush() {
            return Err(err);
        }

        if !self.is_completed() {
            return Err(RError::incompleted(self.builder.get_buf()));
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

        p.feed(text)?;

        p.close()?;

        Ok(RList::new(p.queue.into(), None))
    }

    fn assert_atom(expect: RAtom, code: &str) {
        match parse(code) {
            Ok(val) => {
                assert_eq!(1, val.len());
                assert_eq!(RValue::Atom(expect), val[0]);
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
        assert_atom(RAtom::Symbol(expect.into()), code)
    }

    fn assert_str(expect: &str, code: &str) {
        assert_atom(expect.into(), code)
    }

    #[test]
    fn symbol() {
        assert_symbol("hello", "hello");
        assert_symbol("hello-world", " hello-world ");
        assert_symbol("-abc123", " -abc123 ");
        assert_symbol("-", " - ");
    }

    #[test]
    fn int() {
        assert_atom(1.0.into(), "1");
        assert_atom(2.0.into(), " 2 ");
        assert_atom(42.0.into(), " 42 ");
        assert_atom(12.345.into(), " 12.345 ");
        assert_atom((-1.234).into(), " -1.234 ");

        assert_err(RError::invalid_literal("0hello".into()), "0hello world1");
        assert_err(RError::invalid_literal("-1abc".into()), "-1abc");
        assert_err(RError::invalid_literal("123_456".into()), "123_456 abc");
        assert_err(RError::invalid_literal("12.34.56".into()), "12.34.56");
    }

    #[test]
    fn string() {
        assert_str("hello world", r#"  "hello world"  "#);
        assert_str(" ", r#"  " "  "#);
        assert_str("\t\r\n\\", r#"  "\t\r\n\\"  "#);
        assert_str("hello (world)", r#"  "hello (world)"  "#);

        assert_err(RError::invalid_escape('a'), r#""\a""#);
        assert_err(RError::incompleted("hello".into()), "\"hello\n");
        assert_err(RError::incompleted("hello\"  ".into()), r#"  "hello\"  "#);
    }

    #[test]
    fn list() {
        assert_err(RError::incompleted("".into()), r#"  ("hello"  "#);
        assert_err(RError::incompleted("".into()), r#"  123)  "#);
    }
}
