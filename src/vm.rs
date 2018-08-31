use std::collections::HashMap;
use expr;

#[derive(Debug)]
pub struct VM {
    env: HashMap<expr::Id, Value>
}

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Bool(bool),
    Char(char),
    String(String),
    Tuple(Vec<Value>),
    Func(expr::Func)
}
