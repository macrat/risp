use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::convert::From;
use std::fmt;
use std::mem;
use std::rc::Rc;

#[derive(Debug)]
enum RError {
    Type(String),
    AlreadyExist(String),
    NotExist(String),
    Argument(String),
}

impl fmt::Display for RError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RError::Type(reason) => write!(f, "TypeError: {}", reason),
            RError::AlreadyExist(reason) => write!(f, "AlreadyExistError: {}", reason),
            RError::NotExist(reason) => write!(f, "NotExistError: {}", reason),
            RError::Argument(reason) => write!(f, "ArgumentError: {}", reason),
        }
    }
}

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

    fn compute(&self, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        match self {
            RAtom::Symbol(name) => match scope.borrow().get(name) {
                Some(val) => val.compute(Rc::clone(&scope)),
                None => Err(RError::NotExist(format!("`{}` does not exist.", name))),
            },
            _ => Ok(RType::Atom(self.clone())),
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

    fn compute(&self, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        match self.0[0].compute(Rc::clone(&scope)) {
            Ok(first) => match &first {
                RType::Func(func) => func.call(RList(self.0[1..].to_vec()), scope),
                _ => Err(RError::Type(format!("`{}` is not a function.", first))),
            },
            Err(err) => Err(err),
        }
    }

    fn compute_each(&self, scope: Rc<RefCell<Scope>>) -> Result<RList, RError> {
        let mut result = RList::new();
        for x in &self.0 {
            match x.compute(Rc::clone(&scope)) {
                Ok(x) => result.push(x),
                Err(err) => return Err(err),
            }
        }
        Ok(result)
    }

    fn to_bare_string(&self) -> String {
        let mut vec: Vec<String> = Vec::new();
        for x in &self.0 {
            vec.push(x.to_string());
        }
        vec.join(" ")
    }
}

impl fmt::Display for RList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({})", self.to_bare_string())
    }
}

impl Clone for RList {
    fn clone(&self) -> RList {
        let mut result = RList::new();
        for x in &self.0 {
            result.push((*x).clone());
        }
        result
    }
}

trait Callable {
    fn name(&self) -> &str;

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError>;
}

impl fmt::Display for dyn Callable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl fmt::Debug for dyn Callable {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Callable({})", self)
    }
}

#[derive(Debug)]
enum RFunc {
    Pure { args: Vec<String>, body: RList },
    Binary(Box<dyn Callable>),
}

macro_rules! binary_func {
    ($func:expr) => {
        RType::Func(Rc::new(RFunc::Binary(Box::new($func))))
    };
}

impl Callable for RFunc {
    fn name(&self) -> &str {
        match self {
            RFunc::Pure { args: _, body: _ } => "unnamed_function",
            RFunc::Binary(c) => c.name(),
        }
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        match self {
            RFunc::Pure {
                args: arg_names,
                body,
            } => {
                let scope = Scope::new(Some(scope));
                if args.0.len() != arg_names.len() {
                    return Err(RError::Argument(format!(
                        "this function needs {} arguments but got {} arguments.",
                        arg_names.len(),
                        args.0.len()
                    )));
                }

                for (name, value) in arg_names.iter().zip(args.0) {
                    if let Err(err) = (*scope)
                        .borrow_mut()
                        .define(String::from(name), Rc::new(value))
                    {
                        return Err(err);
                    }
                }

                match body.compute_each(scope) {
                    Ok(RList(result)) => {
                        if let Some(x) = result.last() {
                            Ok(x.clone())
                        } else {
                            Ok(RType::nil())
                        }
                    }
                    Err(err) => Err(err),
                }
            }
            RFunc::Binary(c) => c.call(args, scope),
        }
    }
}

impl fmt::Display for RFunc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RFunc::Pure { args, body } => {
                write!(f, "(func ({}) {})", args.join(" "), body.to_bare_string())
            }
            RFunc::Binary(c) => write!(f, "{}", *c),
        }
    }
}

#[derive(Debug)]
enum RType {
    Atom(RAtom),
    List(RList),
    Func(Rc<RFunc>),
}

impl RType {
    fn nil() -> RType {
        RType::List(RList::new())
    }

    fn parse(s: String) -> RType {
        RType::Atom(RAtom::parse(s))
    }

    fn compute(&self, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        match self {
            RType::Atom(atom) => atom.compute(scope),
            RType::List(list) => list.compute(scope),
            RType::Func(func) => Ok(RType::Func(Rc::clone(func))),
        }
    }

    fn is_nil(&self) -> bool {
        match self {
            RType::List(list) if list.0.len() == 0 => true,
            _ => false,
        }
    }
}

impl fmt::Display for RType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RType::Atom(atom) => atom.fmt(f),
            RType::List(list) => list.fmt(f),
            RType::Func(func) => func.fmt(f),
        }
    }
}

impl Clone for RType {
    fn clone(&self) -> RType {
        match self {
            RType::Atom(atom) => RType::Atom(atom.clone()),
            RType::List(list) => RType::List(list.clone()),
            RType::Func(func) => RType::Func(Rc::clone(func)),
        }
    }
}

#[derive(Debug)]
struct MakeFunc;

impl Callable for MakeFunc {
    fn name(&self) -> &str {
        "func"
    }

    fn call(&self, args: RList, _: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        let RList(args) = args;
        Ok(RType::Func(Rc::new(RFunc::Pure {
            args: match &args[0] {
                RType::List(list) => {
                    let mut symbols: Vec<String> = Vec::new();
                    for x in &list.0 {
                        if let RType::Atom(RAtom::Symbol(name)) = x {
                            symbols.push(String::from(name))
                        } else {
                            return Err(RError::Type(String::from(
                                "elements in the first argument of `func` should be a symbol.",
                            )));
                        }
                    }
                    symbols
                }
                _ => {
                    return Err(RError::Type(String::from(
                        "first argument of `func` should be a list.",
                    )))
                }
            },
            body: RList(args[1..].to_vec()),
        })))
    }
}

impl fmt::Display for MakeFunc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "func")
    }
}

#[derive(Debug)]
struct DefineFunc;

impl Callable for DefineFunc {
    fn name(&self) -> &str {
        "def"
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        let value = match args.0[1].compute(Rc::clone(&scope)) {
            Ok(x) => x,
            Err(err) => return Err(err),
        };

        if let RType::Atom(RAtom::Symbol(name)) = &args.0[0] {
            if let Err(err) = (*scope)
                .borrow_mut()
                .define(String::from(name), Rc::new(value.clone()))
            {
                Err(err)
            } else {
                Ok(value)
            }
        } else {
            Err(RError::Type(format!(
                "first argument of `def` should be a symbol."
            )))
        }
    }
}

#[derive(Debug)]
struct SetFunc;

impl Callable for SetFunc {
    fn name(&self) -> &str {
        "set"
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        let value = match args.0[1].compute(Rc::clone(&scope)) {
            Ok(x) => x,
            Err(err) => return Err(err),
        };

        if let RType::Atom(RAtom::Symbol(name)) = &args.0[0] {
            if let Err(err) = (*scope)
                .borrow_mut()
                .set(String::from(name), Rc::new(value.clone()))
            {
                Err(err)
            } else {
                Ok(value)
            }
        } else {
            Err(RError::Type(format!(
                "first argument of `set` should be a symbol."
            )))
        }
    }
}

#[derive(Debug)]
struct PrintlnFunc;

impl Callable for PrintlnFunc {
    fn name(&self) -> &str {
        "println"
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        match args.compute_each(scope) {
            Ok(x) => {
                println!("{}", x.to_bare_string());
                Ok(RType::nil())
            }
            Err(err) => Err(err),
        }
    }
}

#[derive(Debug)]
struct OperatorFunc<'a>(&'a str, fn(Vec<RType>) -> Result<RType, RError>);

impl Callable for OperatorFunc<'_> {
    fn name(&self) -> &str {
        self.0
    }

    fn call(&self, args: RList, scope: Rc<RefCell<Scope>>) -> Result<RType, RError> {
        let mut xs: Vec<RType> = Vec::new();
        for x in args.0 {
            match x.compute(Rc::clone(&scope)) {
                Ok(x) => xs.push(x),
                Err(err) => return Err(err),
            }
        }
        self.1(xs)
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

    fn define(&mut self, name: String, value: Rc<RType>) -> Result<(), RError> {
        if self.values.contains_key(&name) {
            Err(RError::AlreadyExist(format!(
                "`{}` is already exist in this scope",
                name
            )))
        } else {
            self.values.insert(name, value);
            Ok(())
        }
    }

    fn set(&mut self, name: String, value: Rc<RType>) -> Result<(), RError> {
        if self.values.contains_key(&name) {
            self.values.insert(name, value);
            Ok(())
        } else {
            Err(RError::NotExist(format!(
                "`{}` is not exist in this scope",
                name
            )))
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

impl fmt::Display for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut xs: Vec<String> = Vec::new();
        for (key, value) in &self.values {
            xs.push(format!("{} = {}", key, value));
        }
        write!(f, "{}", xs.join("\n"))
    }
}

impl Drop for Scope {
    fn drop(&mut self) {
        let keys: Vec<String> = self.values.keys().map(|x| x.to_string()).collect();
        for key in keys {
            self.values.remove(&key);
        }
    }
}

fn main() {
    let scope = Scope::new(None);
    (*scope)
        .borrow_mut()
        .define(String::from("func"), Rc::new(binary_func!(MakeFunc)));
    (*scope)
        .borrow_mut()
        .define(String::from("def"), Rc::new(binary_func!(DefineFunc)));
    (*scope)
        .borrow_mut()
        .define(String::from("set"), Rc::new(binary_func!(SetFunc)));
    (*scope)
        .borrow_mut()
        .define(String::from("println"), Rc::new(binary_func!(PrintlnFunc)));
    (*scope).borrow_mut().define(
        String::from("*"),
        Rc::new(binary_func!(OperatorFunc("*", |xs| {
            let mut result = 1;
            for x in xs {
                if let RType::Atom(RAtom::Int(x)) = x {
                    result *= x;
                } else {
                    return Err(RError::Type(format!("`*` can not apply to `{}`", x)));
                }
            }
            Ok(RType::Atom(RAtom::Int(result)))
        }))),
    );
    println!("{}", scope.borrow());
    println!("-----");

    let input = r"
        (def double (func (x) (* x 2)))
        (def doubled (double 4))
        (println doubled)
        (set doubled (double doubled))
        (println doubled)
    "
    .to_string();
    for x in ValueIterator::new(&mut TokenIterator::new(&mut input.chars())) {
        println!("< {}", x);
        match x.compute(Rc::clone(&scope)) {
            Ok(val) => {
                if !val.is_nil() {
                    println!("> {}", val);
                }
            }
            Err(err) => println!("! {}", err),
        }
    }

    println!("-----");
    println!("{}", scope.borrow());
}
