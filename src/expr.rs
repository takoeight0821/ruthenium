#[derive(PartialEq, Debug, Clone)]
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
    Fn(Vec<Id>, Box<Block>),
    If(Id, Box<Block>, Box<Block>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Let {
    NonRec { name: Id, val: Expr },
    Rec { name: Id, val: Expr },
}

#[derive(PartialEq, Debug, Clone)]
pub struct Block {
    block: Vec<Let>,
    term: Expr,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Func {
    name: Id,
    params: Vec<Id>,
    body: Block,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Program {
    program: Vec<Func>,
}

#[derive(PartialEq, Eq, Debug, PartialOrd, Ord, Clone, Hash)]
pub enum Type {
    Int { size: i8 },
    Float32,
    Float64,
    Pointer(Box<Type>),
    Struct(Vec<Type>),
    Function { codom: Box<Type>, dom: Vec<Type> },
}

#[derive(PartialEq, Eq, Debug, PartialOrd, Ord, Clone, Hash)]
pub struct Id {
    name: String,
    uniq: u64,
    ty: Type,
}
