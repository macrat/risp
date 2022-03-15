use std::fmt;
use std::convert::From;
use std::collections::VecDeque;

#[derive(Debug)]
enum RTypeError {
    IsNotList
}

#[derive(Debug)]
enum RType {
    Symbol(String),
    Int(i64),
    // TODO: implement function here
    Cons(Box<RType>, Box<RType>),
    Nil,
}

impl RType {
    fn pair(car: RType, cdr: RType) -> RType {
        RType::Cons(
            Box::new(car),
            Box::new(cdr),
        )
    }

    fn single(car: RType) -> RType {
        RType::pair(car, RType::Nil)
    }

    fn parse(s: String) -> RType {
        if let Ok(i) = s.parse::<i64>() {
            RType::Int(i)
        } else {
            RType::Symbol(s)
        }
    }

    fn compute(&self) -> Box<RType> {
        match self {
            RType::Symbol(name) => Box::new(RType::Symbol(String::from(name))),
            RType::Int(value) => Box::new(RType::Int(*value)),
            RType::Cons(car, cdr) => Box::new(RType::Cons(car.compute(), cdr.compute())), // TODO: call function here
            RType::Nil => Box::new(RType::Nil),
        }
    }

    fn push(&mut self, val: RType) -> Result<(), RTypeError> {
        let mut cur = self;
        loop {
            match cur {
                RType::Nil => {
                    *cur = RType::single(val);
                    return Ok(());
                },
                RType::Cons(_, cdr) => {
                    cur = cdr;
                },
                _ => return Err(RTypeError::IsNotList)
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
                            vec.push(format!("{}", car));
                            cur = cdr;
                        },
                        x => {
                            vec.push(".".to_string());
                            vec.push(format!("{}", x));
                            break;
                        },
                    }
                }
                write!(f, "({})", vec.join(" "))
            },
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

struct ValueIterator<'a> {
    tokens: &'a mut dyn Iterator<Item=Token>,
    stack: Vec<RType>,
}

impl ValueIterator<'_> {
    fn new(tokens: &mut dyn Iterator<Item=Token>) -> ValueIterator {
        ValueIterator{
            tokens,
            stack: Vec::new(),
        }
    }
}

impl Iterator for ValueIterator<'_> {
    type Item = RType;

    fn next(&mut self) -> Option<RType> {
        loop {
            match self.tokens.next() {
                Some(Token::OpenList) => {
                    let list = RType::Nil;
                    self.stack.push(list);
                },
                Some(Token::CloseList) => {
                    if let Some(value) = self.stack.pop() {
                        if let Some(mut last) = self.stack.pop() {
                            last.push(value);
                            self.stack.push(last);
                        } else {
                            return Some(value);
                        }
                    }
                }
                Some(Token::Value(value)) => {
                    if let Some(mut last) = self.stack.pop() {
                        last.push(value);
                        self.stack.push(last);
                    } else {
                        return Some(value);
                    }
                },
                None => return None,
            }
        }
    }
}

fn main() {
    let input = "(println\n (+ 1 2))".to_string();
    println!("{}\n", input);
    for x in ValueIterator::new(&mut TokenIterator::new(&mut input.chars())) {
        println!("{}", x);
    }
}
