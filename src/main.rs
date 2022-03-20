use std::collections::VecDeque;
use std::mem;
use std::rc::Rc;

mod types;
use types::*;

mod scope;
use scope::Scope;

mod funcs;

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
                    self.stack.push(RList::empty());
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

fn main() {
    let scope = Scope::new(None);
    if let Err(err) = funcs::register_to(&mut (*scope).borrow_mut()) {
        println!("failed to load embedded functions: {}", err);
        std::process::exit(-1);
    }

    println!("{}", scope.borrow());
    println!("-----");

    let input = r"
        (def double (func (x) (* x 2)))

        (def doubled (double 4))
        (println doubled)

        (set doubled (double doubled))
        (println doubled)

        (def countdown (func (x)
          (println x)
          (if x (countdown (- x 1)))))
        (countdown 5)
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
