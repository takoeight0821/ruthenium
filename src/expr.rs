use std::ops::Deref;
use types::{HasType, Type};

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

impl HasType for Expr {
    fn type_(&self) -> Type {
        match self {
            Expr::Var(x) => x.type_(),
            Expr::I32(_) => Type::Int(32),
            Expr::I64(_) => Type::Int(64),
            Expr::F32(_) => Type::Float32,
            Expr::F64(_) => Type::Float64,
            Expr::Bool(_) => Type::Int(1),
            Expr::Char(_) => Type::Int(8),
            Expr::String(_) => Type::Pointer(Box::new(Type::Int(8))),
            Expr::Tuple(xs) => Type::Pointer(Box::new(Type::Struct(
                xs.iter().map(|x| x.type_().clone()).collect(),
            ))),
            Expr::Apply(f, _) => match f.type_() {
                Type::Function { codom, .. } => codom.deref().clone(),
                t => panic!("{:?} is not appliable", t),
            },
            Expr::Fn(params, body) => Type::Function {
                codom: Box::new(body.type_().clone()),
                dom: params.iter().map(|x| x.type_().clone()).collect(),
            },
            Expr::If(_, t, _) => t.type_(),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Let {
    NonRec {
        name: Id,
        val: Expr,
    },
    Rec {
        name: Id,
        params: Vec<Id>,
        body: Block,
    },
}

#[derive(PartialEq, Debug, Clone)]
pub struct Block {
    pub exprs: Vec<Let>,
    pub term: Expr,
}

impl HasType for Block {
    fn type_(&self) -> Type {
        self.term.type_()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Func {
    pub name: Id,
    pub params: Vec<Id>,
    pub body: Block,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Program {
    pub program: Vec<Func>,
}

#[derive(PartialEq, Eq, Debug, PartialOrd, Ord, Clone, Hash)]
pub struct Id {
    name: String,
    uniq: u64,
    ty: Type,
}

impl HasType for Id {
    fn type_(&self) -> Type {
        self.ty.clone()
    }
}
