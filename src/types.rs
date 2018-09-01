#[derive(PartialEq, Eq, Debug, PartialOrd, Ord, Clone, Hash)]
pub enum Type {
    Int(i8),
    Float32,
    Float64,
    String,
    Tuple(Vec<Type>),
    Function { codom: Box<Type>, dom: Vec<Type> },
}

pub trait HasType {
    fn type_(&self) -> Type;
}

impl HasType for Type {
    fn type_(&self) -> Type {
        self.clone()
    }
}
