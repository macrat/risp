use std::fmt;
use std::convert::From;
use std::collections::VecDeque;
use std::ptr;

#[derive(Debug)]
struct Cons(Box<RType>, Box<RType>);

#[derive(Debug)]
enum RType {
    Symbol(String),
    Int(i64),
    // TODO: implement function here
    Cons(Cons),
    Nil,
}

impl RType {
    fn parse(s: String) -> RType {
        if s == "nil" {
            return RType::Nil
        }
        if let Some(first) = s.chars().nth(0) {
            if '0' <= first && first <= '9' {
                RType::Int(s.parse().unwrap())
            } else {
                RType::Symbol(s)
            }
        } else {
            RType::Symbol(s)
        }
    }

    fn compute(&self) -> Box<RType> {
        match self {
            RType::Symbol(name) => Box::new(RType::Symbol(String::from(name))),
            RType::Int(value) => Box::new(RType::Int(*value)),
            RType::Cons(Cons(car, cdr)) => Box::new(RType::Cons(Cons(car.compute(), cdr.compute()))), // TODO: call function here
            RType::Nil => Box::new(RType::Nil),
        }
    }

    fn is_list(&self) -> bool {
        match self {
            RType::Nil => true,
            RType::Cons(Cons(_, cdr)) => (*cdr).is_list(),
            _ => false,
        }
    }
}

impl fmt::Display for RType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RType::Symbol(name) => write!(f, "{}", name),
            RType::Int(value) => write!(f, "{}", value),
            RType::Cons(Cons(car, cdr)) => write!(f, "(cons {} {})", car, cdr),
            RType::Nil => write!(f, "nil"),
        }
    }
}

impl From<i64> for RType {
    fn from(item: i64) -> RType {
        RType::Int(item)
    }
}

#[derive(Debug)]
enum Token {
    OpenList,
    CloseList,
    Value(RType),
}

struct TokenIterator<'a> {
    chars: &'a mut dyn Iterator<Item=char>,
    tokens: VecDeque<Token>,
    buf: String,
}

impl TokenIterator<'_> {
    fn new(chars: &mut dyn Iterator<Item=char>) -> TokenIterator {
        TokenIterator{
            chars,
            tokens: VecDeque::new(),
            buf: String::new(),
        }
    }

    fn flush(&mut self) {
        if self.buf.len() > 0 {
            self.tokens.push_front(Token::Value(RType::parse(self.buf.to_string())));
            self.buf = String::new()
        }
    }

    fn push(&mut self, c: char) {
        match c {
            '(' => {
                self.flush();
                self.tokens.push_front(Token::OpenList);
            },
            ')' => {
                self.flush();
                self.tokens.push_front(Token::CloseList);
            },
            ' ' | '\t' | '\r' | '\n' => self.flush(),
            _ => self.buf.push(c),
        }
    }
}

impl Iterator for TokenIterator<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        while self.tokens.len() == 0 {
            match self.chars.next() {
                Some(c) => self.push(c),
                None => {
                    self.flush();
                    return self.tokens.pop_back()
                },
            }
        }
        self.tokens.pop_back()
    }
}

struct ListBuilder {
    head: RType,
    tail: *mut RType,
}

impl ListBuilder {
    fn new() -> ListBuilder {
        ListBuilder{
            head: RType::Nil,
            tail: ptr::null_mut(),
        }
    }

    fn push(&mut self, val: RType) {
        if !self.tail.is_null() {
            unsafe {
                if let RType::Cons(cons) = &mut *self.tail {
                    cons.1 = Box::new(RType::Cons(Cons(Box::new(val), Box::new(RType::Nil))));
                    return
                }
            }
        }

        self.head = RType::Cons(Cons(Box::new(val), Box::new(RType::Nil)));
        self.tail = &mut self.head;
    }
}

struct ValueIterator<'a> {
    tokens: &'a mut dyn Iterator<Item=Token>,
    head: Option<RType>,
    tail: Option<RType>,
}

impl ValueIterator<'_> {
    fn new(tokens: &mut dyn Iterator<Item=Token>) -> ValueIterator {
        ValueIterator{
            tokens,
            head: None,
            tail: None,
        }
    }
}

impl Iterator for ValueIterator<'_> {
    type Item = RType;

    fn next(&mut self) -> Option<RType> {
        match self.tokens.next() {
            Some(Token::OpenList) => self.next(), // TODO: start using ListBuilder here
            Some(Token::CloseList) => self.next(), // TODO: and return built list here
            Some(Token::Value(value)) => Some(value),
            None => None,
        }
    }
}

fn main() {
    let input = "(println\n (+ 42 123))".to_string();
    println!("{}", input);
    for x in TokenIterator::new(&mut input.chars()) {
        println!("{:?}", x)
    }

    println!("-----");

    let mut b = ListBuilder::new();
    b.push(RType::Int(123));
    b.push(RType::Int(42));
    println!("{}", b.head);
}
