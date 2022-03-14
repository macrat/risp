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
            RType::Cons(Cons(car, cdr)) => Box::new(RType::Cons(Cons(car.compute(), cdr.compute()))), // TODO: call function here
            RType::Nil => Box::new(RType::Nil),
        }
    }

    fn is_nil(&self) -> bool {
        if let RType::Nil = self {
            true
        } else {
            false
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
            RType::Cons(cons) => {
                if self.is_list() {
                    let mut c = self;
                    let mut vec: Vec<String> = Vec::new();
                    while !(*c).is_nil() {
                        if let RType::Cons(cons) = &*c {
                            vec.push(format!("{}", cons.0));
                            c = &*cons.1;
                        }
                    }
                    write!(f, "({})", vec.join(" "))
                } else {
                    write!(f, "(cons {} {})", cons.0, cons.1)
                }
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

    fn push(&mut self, val: Box<RType>) {
        if !self.tail.is_null() {
            unsafe {
                if let RType::Cons(cons) = &mut *self.tail {
                    cons.1 = Box::new(RType::Cons(Cons(val, Box::new(RType::Nil))));
                    self.tail = &mut *cons.1;
                    return
                }
            }
        }

        self.head = RType::Cons(Cons(val, Box::new(RType::Nil)));
        self.tail = &mut self.head;
    }
}

impl fmt::Display for ListBuilder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.head.fmt(f)
    }
}

struct ValueIterator<'a> {
    tokens: &'a mut dyn Iterator<Item=Token>,
    stack: Vec<ListBuilder>,
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
                    let list = ListBuilder::new();
                    println!("add stack: {}", list.head);
                    self.stack.push(list);
                },
                Some(Token::CloseList) => {
                    if let Some(value) = self.stack.pop() {
                        println!("close stack: {}", value.head);
                        if let Some(mut last) = self.stack.pop() {
                            print!("stack again: {} -> {}", value, last);
                            last.push(Box::new(value.head));
                            println!(" => {}", last);
                            self.stack.push(last);
                        } else {
                            println!("return stack: {}", value.head);
                            return Some(value.head);
                        }
                    }
                }
                Some(Token::Value(value)) => {
                    if let Some(mut last) = self.stack.pop() {
                        print!("push item: {}", value);
                        last.push(Box::new(value));
                        println!(" => {}", last);
                        self.stack.push(last);
                    } else {
                        println!("bare item: {}", value);
                        return Some(value);
                    }
                },
                None => return None,
            }
        }
    }
}

fn main() {
    let input = "(a b c)\n(a (b) c)\n((a) b c)\n(a b (c))".to_string();
    println!("{}\n", input);
    for x in ValueIterator::new(&mut TokenIterator::new(&mut input.chars())) {
        println!("{}\n", x);
    }

    /*
    println!("-----");

    let mut a = ListBuilder::new();
    a.push(RType::Symbol("println".to_string()));
    {
        let mut b = ListBuilder::new();
        b.push(RType::Symbol("+".to_string()));
        b.push(RType::Int(42));
        b.push(RType::Int(123));
        a.push(b.head);
    }
    println!("{}", a.head);
    */
}
