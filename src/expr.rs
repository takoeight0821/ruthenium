use std::collections::HashMap;

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub enum Expr {
    Var(Id),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Bool(bool),
    Char(char),
    String(String),
    Tuple(Vec<Id>),
    Apply(Id, Vec<Id>),
    If(Id, Box<Block>, Box<Block>)
}

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub struct Let {
    var: Id,
    val: Box<Expr>
}

#[derive(PartialEq, Debug, PartialOrd, Clone)]
pub struct Block {
    block: Vec<Let>
}

#[derive(PartialEq, Debug, Clone)]
pub struct Func {
    name: Id,
    params: Vec<Id>,
    body: HashMap<String, Block>
}

#[derive(PartialEq, Debug, Clone)]
pub struct Program {
    program: Vec<Func>
}

#[derive(PartialEq, Eq, Debug, PartialOrd, Ord, Clone)]
pub enum Type {
    Int {size: i8},
    Float32,
    Float64,
    Pointer(Box<Type>),
    Struct(Vec<Type>),
    Function {codom: Box<Type>, dom: Vec<Type>}
}

#[derive(PartialEq, Eq, Debug, PartialOrd, Ord, Clone)]
pub struct Id {
    name: String,
    uniq: u64,
    ty: Type
}
