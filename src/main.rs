use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::convert::From;
use std::fmt;
use std::mem;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
enum RAtom {
    Symbol(String),
    Int(i64),
    String(String),
}

impl RAtom {
    fn parse(s: String) -> RAtom {
        // TODO: make way to parse string

        if let Ok(i) = s.parse::<i64>() {
            RAtom::Int(i)
        } else {
            RAtom::Symbol(s)
        }
    }

    fn compute(&self, scope: Rc<RefCell<Scope>>) -> RType {
        match self {
            RAtom::Symbol(name) => match scope.borrow().get(name) {
                Some(val) => val.compute(Rc::clone(&scope)),
                None => RType::nil(), // TODO: throw exception here?
            },
            RAtom::Int(_) => RType::Atom(self.clone()),
            RAtom::String(_) => RType::Atom(self.clone()),
        }
    }
}

impl fmt::Display for RAtom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RAtom::Symbol(name) => write!(f, "{}", name),
            RAtom::Int(value) => write!(f, "{}", value),
            RAtom::String(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug)]
struct RList(Vec<RType>);

impl RList {
    fn new() -> RList {
        RList(Vec::new())
    }

    fn push(&mut self, val: RType) {
        self.0.push(val)
    }

    fn compute(&self, scope: Rc<RefCell<Scope>>) -> RType {
        let mut result = RList::new();
        for x in &self.0 {
            result.push(x.compute(Rc::clone(&scope)));
        }
        // TODO: call function here
        RType::List(result)
    }
}

impl fmt::Display for RList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut vec: Vec<String> = Vec::new();
        for x in &self.0 {
            vec.push(x.to_string());
        }
        write!(f, "({})", vec.join(" "))
    }
}

#[derive(Debug)]
enum RType {
    Atom(RAtom),
    List(RList),
    // TODO: implement function here
}

impl RType {
    fn nil() -> RType {
        RType::List(RList::new())
    }

    fn parse(s: String) -> RType {
        RType::Atom(RAtom::parse(s))
    }

    fn compute(&self, scope: Rc<RefCell<Scope>>) -> RType {
        match self {
            RType::Atom(atom) => atom.compute(scope),
            RType::List(list) => list.compute(scope),
        }
    }
}

impl fmt::Display for RType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RType::Atom(atom) => atom.fmt(f),
            RType::List(list) => list.fmt(f),
        }
    }
}

#[derive(Debug)]
enum Token {
    OpenList,
    CloseList,
    Value(RType),
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
            let buf = mem::replace(&mut self.buf, String::new());
            self.tokens.push_front(Token::Value(RType::parse(buf)));
        }
    }

    fn feed(&mut self, c: char) {
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
                Some(c) => self.feed(c),
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
    stack: Vec<RList>,
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
    type Item = RType;

    fn next(&mut self) -> Option<RType> {
        loop {
            match self.tokens.next() {
                Some(Token::OpenList) => {
                    self.stack.push(RList::new());
                }
                Some(Token::CloseList) => {
                    if let Some(value) = self.stack.pop() {
                        if let Some(mut last) = self.stack.pop() {
                            last.push(RType::List(value));
                            self.stack.push(last);
                        } else {
                            return Some(RType::List(value));
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
    values: HashMap<String, Rc<RType>>,
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
            Err(NamespaceError::AlreadyExist)
        } else {
            self.values.insert(name, value);
            Ok(())
        }
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
        Rc::new(RType::Atom(RAtom::String(String::from("world")))),
    );
    (*child)
        .borrow_mut()
        .define(String::from("abc"), Rc::new(RType::Atom(RAtom::Int(123))));
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
