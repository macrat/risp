use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::convert::From;
use std::fmt;
use std::rc::Rc;

#[derive(Debug)]
enum RTypeError {
    IsNotList,
}

#[derive(Debug)]
enum RType {
    Symbol(String),
    Int(i64),
    String(String),
    // TODO: implement function here
    Cons(Box<RType>, Box<RType>),
    Nil,
}

impl RType {
    fn pair(car: Box<RType>, cdr: Box<RType>) -> Box<RType> {
        Box::new(RType::Cons(car, cdr))
    }

    fn single(car: Box<RType>) -> Box<RType> {
        RType::pair(car, Box::new(RType::Nil))
    }

    fn parse(s: String) -> Box<RType> {
        // TODO: make way to parse string

        Box::new(if let Ok(i) = s.parse::<i64>() {
            RType::Int(i)
        } else {
            RType::Symbol(s)
        })
    }

    fn compute(&self, scope: Rc<RefCell<Scope>>) -> Box<RType> {
        Box::new(match self {
            RType::Symbol(name) => match scope.borrow().get(name) {
                Some(val) => *val.compute(Rc::clone(&scope)),
                None => RType::Nil,
            },
            RType::Int(i) => RType::Int(*i),
            RType::String(s) => RType::String(s.clone()),
            RType::Cons(car, cdr) => {
                RType::Cons((*car).compute(Rc::clone(&scope)), (*cdr).compute(scope))
            } // TODO: call function here
            RType::Nil => RType::Nil,
        })
    }

    fn push(&mut self, val: Box<RType>) -> Result<(), RTypeError> {
        let mut cur = self;
        loop {
            match cur {
                RType::Nil => {
                    *cur = *RType::single(val);
                    return Ok(());
                }
                RType::Cons(_, cdr) => {
                    cur = &mut **cdr;
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
            RType::String(value) => write!(f, "{}", value),
            RType::Cons(_, _) => {
                let mut cur = self;
                let mut vec: Vec<String> = Vec::new();
                loop {
                    match cur {
                        RType::Nil => break,
                        RType::Cons(car, cdr) => {
                            vec.push(format!("{}", *car));
                            cur = &**cdr;
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
    Value(Box<RType>),
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
    stack: Vec<Box<RType>>,
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
    type Item = Box<RType>;

    fn next(&mut self) -> Option<Box<RType>> {
        loop {
            match self.tokens.next() {
                Some(Token::OpenList) => {
                    let list = Box::new(RType::Nil);
                    self.stack.push(list);
                }
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
                }
                None => return None,
            }
        }
    }
}

#[derive(Debug)]
enum NamespaceError {
    AlreadyExist,
}

#[derive(Debug)]
struct Scope {
    parent: Option<Rc<RefCell<Scope>>>,
    values: HashMap<String, Rc<RType>>, // XXX: I really don't like this Rc.
}

impl Scope {
    fn new(parent: Option<Rc<RefCell<Scope>>>) -> Rc<RefCell<Scope>> {
        Rc::new(RefCell::new(Scope {
            parent,
            values: HashMap::new(),
        }))
    }

    fn is_exist(&self, name: &String) -> bool {
        if self.values.contains_key(name) {
            true
        } else if let Some(parent) = &self.parent {
            (*parent).borrow().is_exist(name)
        } else {
            false
        }
    }

    fn define(&mut self, name: String, value: Rc<RType>) -> Result<(), NamespaceError> {
        if self.is_exist(&name) {
            return Err(NamespaceError::AlreadyExist);
        }

        self.values.insert(name, value);
        Ok(())
    }

    fn get(&self, name: &String) -> Option<Rc<RType>> {
        let result = self.values.get(name);
        if let Some(x) = result {
            Some(Rc::clone(x))
        } else if let Some(parent) = &self.parent {
            (*parent).borrow().get(name)
        } else {
            None
        }
    }
}

impl Drop for Scope {
    fn drop(&mut self) {
        let keys: Vec<String> = self.values.keys().map(|x| x.to_string()).collect();
        for key in keys {
            println!("DEBUG drop {}", key);
            self.values.remove(&key);
        }
    }
}

fn main() {
    let parent = Scope::new(None);
    let child = Scope::new(Some(Rc::clone(&parent)));
    (*parent).borrow_mut().define(
        String::from("hello"),
        Rc::new(RType::String(String::from("world"))),
    );
    (*child)
        .borrow_mut()
        .define(String::from("abc"), Rc::new(RType::Int(123)));
    println!(
        "child.hello = {:?}",
        child.borrow().get(&String::from("hello"))
    );
    println!("child.abc = {:?}", child.borrow().get(&String::from("abc")));
    println!(
        "parent.hello = {:?}",
        parent.borrow().get(&String::from("hello"))
    );
    println!(
        "parent.abc = {:?}",
        parent.borrow().get(&String::from("abc"))
    );

    println!();

    let input = "(hello abc)".to_string();
    println!("{}", input);
    for x in ValueIterator::new(&mut TokenIterator::new(&mut input.chars())) {
        println!("{}", x.compute(Rc::clone(&child)));
    }
}
