use std::fmt;
use std::fmt::Display;

#[derive(PartialEq, Eq, Debug, PartialOrd, Ord, Clone, Hash)]
pub enum Type {
    Int(u8),
    Float32,
    Float64,
    String,
    Tuple(Vec<Type>),
    Function { codom: Box<Type>, dom: Vec<Type> },
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Type::*;
        write!(f, "<")?;
        match self {
            Int(x) => write!(f, "int {}", x)?,
            Float32 => write!(f, "float 32")?,
            Float64 => write!(f, "float 64")?,
            String => write!(f, "string")?,
            Tuple(xs) => xs.iter().map(|x| x.fmt(f)).collect::<fmt::Result>()?,
            Function { codom, dom } => {
                write!(f, "fn (")?;
                dom.iter()
                    .map(|x| write!(f, " {}", x))
                    .collect::<fmt::Result>()?;
                write!(f, " ) -> {}", codom)
            }?,
        }
        write!(f, ">")
    }
}

pub trait HasType {
    fn type_of(&self) -> Type;
}

impl HasType for Type {
    fn type_of(&self) -> Type {
        self.clone()
    }
}

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
    Access(Id, usize),
    Apply(Id, Vec<Id>),
    Prim(String, Type),
    If(Id, Box<Block>, Box<Block>),
}

impl HasType for Expr {
    fn type_of(&self) -> Type {
        match self {
            Expr::Var(x) => x.type_of(),
            Expr::I32(_) => Type::Int(32),
            Expr::I64(_) => Type::Int(64),
            Expr::F32(_) => Type::Float32,
            Expr::F64(_) => Type::Float64,
            Expr::Bool(_) => Type::Int(1),
            Expr::Char(_) => Type::Int(8),
            Expr::String(_) => Type::String,
            Expr::Tuple(xs) => Type::Tuple(xs.iter().map(HasType::type_of).collect()),
            Expr::Access(x, i) => match x.type_of() {
                Type::Tuple(ts) => ts[*i].clone(),
                t => panic!("{:?} is not accessable", t),
            },
            Expr::Apply(f, _) => match f.type_of() {
                Type::Function { codom, .. } => codom.type_of(),
                t => panic!("{:?} is not appliable", t),
            },
            Expr::Prim(_, t) => t.clone(),
            Expr::If(_, t, _) => t.type_of(),
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
    fn type_of(&self) -> Type {
        self.term.type_of()
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
pub struct Id(pub String, pub Type);

impl Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{} : {}]", self.0, self.1)
    }
}

impl HasType for Id {
    fn type_of(&self) -> Type {
        self.1.clone()
    }
}
