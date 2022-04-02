use std::rc::Rc;

use crate::scope::Scope;
use crate::types::*;

mod flow;
mod func;
mod io;
mod list;
mod operator;
mod types;
mod variable;

macro_rules! binary_func {
    ($func:expr) => {
        RValue::Func(Rc::new(RFunc::Binary(Box::new($func))))
    };
}

macro_rules! register {
    ($scope:expr, $name:expr, $value:expr) => {
        $scope.define($name.into(), $value)?;
    };
}

pub fn register_to(scope: &Scope) -> Result<(), RError> {
    // variable
    register!(scope, "def", binary_func!(variable::Def));
    register!(scope, "set", binary_func!(variable::Set));

    // flow
    register!(scope, "if", binary_func!(flow::If));
    register!(scope, "while", binary_func!(flow::While));
    register!(scope, "do", binary_func!(flow::Do));
    register!(scope, "throw", binary_func!(flow::Throw));
    register!(scope, "try-catch", binary_func!(flow::TryCatch));

    // function
    register!(scope, "func", binary_func!(func::Func));

    // list
    register!(scope, "list", binary_func!(list::List));
    register!(scope, "length", binary_func!(list::Length));
    register!(scope, "get", binary_func!(list::Get));
    register!(scope, "car", binary_func!(list::Car));
    register!(scope, "cdr", binary_func!(list::Cdr));
    register!(scope, "seq", binary_func!(list::Seq));
    register!(scope, "map", binary_func!(list::Map));
    register!(scope, "fold", binary_func!(list::Fold));

    // io
    register!(scope, "import", binary_func!(io::Import));
    register!(scope, "stdout", binary_func!(io::stdout()));
    register!(scope, "stderr", binary_func!(io::stderr()));
    register!(scope, "print", binary_func!(io::PRINT));
    register!(scope, "println", binary_func!(io::PRINTLN));

    // type
    register!(scope, "number", binary_func!(types::ToNumber));
    register!(scope, "string", binary_func!(types::ToString));
    register!(scope, "literal", binary_func!(types::ToLiteral));

    register!(scope, "is-number", binary_func!(types::IS_NUMBER));
    register!(scope, "is-string", binary_func!(types::IS_STRING));
    register!(scope, "is-list", binary_func!(types::IS_LIST));
    register!(scope, "is-func", binary_func!(types::IS_FUNC));

    // operator
    register!(scope, "+", binary_func!(operator::ADD));
    register!(scope, "-", binary_func!(operator::SUB));
    register!(scope, "*", binary_func!(operator::MULTIPLY));
    register!(scope, "/", binary_func!(operator::DIVIDE));

    register!(scope, "=", binary_func!(operator::EQ));
    register!(scope, "!=", binary_func!(operator::NE));
    register!(scope, "<", binary_func!(operator::LT));
    register!(scope, ">", binary_func!(operator::GT));
    register!(scope, "<=", binary_func!(operator::LE));
    register!(scope, ">=", binary_func!(operator::GE));

    register!(scope, "not", binary_func!(operator::Not));

    Ok(())
}
