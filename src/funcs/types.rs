use crate::env::Env;
use crate::scope::Scope;
use crate::types::*;

pub struct TypeCheckOperator<'a>(&'a str, fn(&RValue) -> bool);

impl Callable for TypeCheckOperator<'_> {
    fn name(&self) -> &str {
        self.0
    }

    fn arg_rule(&self) -> ArgumentRule {
        ArgumentRule::AtLeast(1)
    }

    fn call(&self, env: &mut Env, scope: &Scope, args: RList) -> Result<RValue, RError> {
        for x in args.iter() {
            if !self.1(&x.compute(env, scope)?) {
                return Ok(0.0.into());
            }
        }
        Ok(1.0.into())
    }
}

pub const IS_NUMBER: TypeCheckOperator = TypeCheckOperator("is-number", |x| match x {
    RValue::Atom(RAtom::Number(_)) => true,
    _ => false,
});

pub const IS_STRING: TypeCheckOperator = TypeCheckOperator("is-string", |x| match x {
    RValue::Atom(RAtom::String(_)) => true,
    _ => false,
});

pub const IS_LIST: TypeCheckOperator = TypeCheckOperator("is-list", |x| match x {
    RValue::List(_) => true,
    _ => false,
});

pub const IS_FUNC: TypeCheckOperator = TypeCheckOperator("is-func", |x| match x {
    RValue::Func(_) => true,
    _ => false,
});
