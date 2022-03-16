use std::cell::RefCell;
use std::collections::VecDeque;
use std::convert::From;
use std::fmt;
use std::mem;

#[derive(Debug)]
enum RTypeError {
    IsNotList,
}

#[derive(Debug)]
enum RType {
    Symbol(String),
    Int(i64),
    // TODO: implement function here
    Cons(RefCell<Box<RType>>, RefCell<Box<RType>>),
    Nil,
}

impl RType {
    fn pair(car: RefCell<Box<RType>>, cdr: RefCell<Box<RType>>) -> RefCell<Box<RType>> {
        RefCell::new(Box::new(RType::Cons(car, cdr)))
    }

    fn single(car: RefCell<Box<RType>>) -> RefCell<Box<RType>> {
        RType::pair(car, RefCell::new(Box::new(RType::Nil)))
    }

    fn parse(s: String) -> RefCell<Box<RType>> {
        RefCell::new(Box::new(if let Ok(i) = s.parse::<i64>() {
            RType::Int(i)
        } else {
            RType::Symbol(s)
        }))
    }

    fn compute(&self) -> RefCell<Box<RType>> {
        RefCell::new(Box::new(match self {
            RType::Symbol(name) => RType::Symbol(name.to_string()), // TODO: fetch actual value here
            RType::Int(i) => RType::Int(*i),
            RType::Cons(car, cdr) => {
                RType::Cons((*car).borrow_mut().compute(), (*cdr).borrow_mut().compute())
            } // TODO: call function here
            RType::Nil => RType::Nil,
        }))
    }

    fn push(&mut self, val: RefCell<Box<RType>>) -> Result<(), RTypeError> {
        let mut cur = self;
        loop {
            match cur {
                RType::Nil => {
                    mem::swap(cur, &mut **RType::single(val).borrow_mut());
                    return Ok(());
                }
                RType::Cons(_, cdr) => {
                    cur = &mut **cdr.borrow_mut();
                }
                _ => return Err(RTypeError::IsNotList),
            }
        }
    }
}

impl fmt::Display for RType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RType::Symbol(name) => write!(f, "{}", name),
            RType::Int(value) => write!(f, "{}", value),
            RType::Cons(_, _) => {
                let mut cur = self;
                let mut vec: Vec<String> = Vec::new();
                loop {
                    match cur {
                        RType::Nil => break,
                        RType::Cons(car, cdr) => {
                            vec.push(format!("{}", *car.borrow()));
                            cur = &mut **cdr.borrow();
                        }
                        x => {
                            vec.push(".".to_string());
                            vec.push(format!("{}", x));
                            break;
                        }
                    }
                }
                write!(f, "({})", vec.join(" "))
            }
            RType::Nil => write!(f, "()"),
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
    Value(RefCell<Box<RType>>),
}

struct TokenIterator<'a> {
    chars: &'a mut dyn Iterator<Item = char>,
    tokens: VecDeque<Token>,
    buf: String,
}

impl TokenIterator<'_> {
    fn new(chars: &mut dyn Iterator<Item = char>) -> TokenIterator {
        TokenIterator {
            chars,
            tokens: VecDeque::new(),
            buf: String::new(),
        }
    }

    fn flush(&mut self) {
        if self.buf.len() > 0 {
            self.tokens
                .push_front(Token::Value(RType::parse(self.buf.to_string())));
            self.buf = String::new()
        }
    }

    fn push(&mut self, c: char) {
        match c {
            '(' => {
                self.flush();
                self.tokens.push_front(Token::OpenList);
            }
            ')' => {
                self.flush();
                self.tokens.push_front(Token::CloseList);
            }
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
                    return self.tokens.pop_back();
                }
            }
        }
        self.tokens.pop_back()
    }
}

struct ValueIterator<'a> {
    tokens: &'a mut dyn Iterator<Item = Token>,
    stack: Vec<RefCell<Box<RType>>>,
}

impl ValueIterator<'_> {
    fn new(tokens: &mut dyn Iterator<Item = Token>) -> ValueIterator {
        ValueIterator {
            tokens,
            stack: Vec::new(),
        }
    }
}

impl Iterator for ValueIterator<'_> {
    type Item = RefCell<Box<RType>>;

    fn next(&mut self) -> Option<RefCell<Box<RType>>> {
        loop {
            match self.tokens.next() {
                Some(Token::OpenList) => {
                    let list = RefCell::new(Box::new(RType::Nil));
                    self.stack.push(list);
                }
                Some(Token::CloseList) => {
                    if let Some(value) = self.stack.pop() {
                        if let Some(last) = self.stack.pop() {
                            last.borrow_mut().push(value);
                            self.stack.push(last);
                        } else {
                            return Some(value);
                        }
                    }
                }
                Some(Token::Value(value)) => {
                    if let Some(last) = self.stack.pop() {
                        last.borrow_mut().push(value);
                        self.stack.push(last);
                    } else {
                        return Some(value);
                    }
                }
                None => return None,
            }
        }
    }
}

fn main() {
    let input = "(println\n (+ 1 2))".to_string();
    println!("{}\n", input);
    for x in ValueIterator::new(&mut TokenIterator::new(&mut input.chars())) {
        println!("{}", x.borrow());
    }
}
