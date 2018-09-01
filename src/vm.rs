use expr;
use std::collections::HashMap;
use std::rc::Rc;
use types::*;

#[derive(Debug)]
pub struct VM {
    env: HashMap<expr::Id, Value>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Bool(bool),
    Char(char),
    String(Rc<String>),
    Tuple(Rc<Vec<Value>>),
    Func {
        params: Vec<expr::Id>,
        env: HashMap<expr::Id, Value>,
        body: expr::Block,
    },
}

impl HasType for Value {
    fn type_(&self) -> Type {
        match self {
            Value::I32(_) => Type::Int(32),
            Value::I64(_) => Type::Int(64),
            Value::F32(_) => Type::Float32,
            Value::F64(_) => Type::Float64,
            Value::Bool(_) => Type::Int(1),
            Value::Char(_) => Type::Int(8),
            Value::String(_) => Type::Pointer(Box::new(Type::Int(8))),
            Value::Tuple(xs) => Type::Pointer(Box::new(Type::Struct(
                xs.iter().map(|x| x.type_()).collect(),
            ))),
            Value::Func { params, body, .. } => Type::Function {
                codom: Box::new(body.type_()),
                dom: params.iter().map(|x| x.type_()).collect(),
            },
        }
    }
}

impl VM {
    pub fn new() -> Self {
        VM {
            env: HashMap::new(),
        }
    }

    pub fn eval(&mut self, program: expr::Program, entry: expr::Block) {
        self.load_functions(program.program);
        self.eval_block(entry);
    }

    fn load_functions(&mut self, funcs: Vec<expr::Func>) {
        for f in funcs {
            self.load_function(f);
        }
    }

    fn load_function(&mut self, func: expr::Func) {
        let env = self.env.clone();
        match func {
            expr::Func { name, params, body } => {
                let f = Value::Func { params, env, body };
                assert_eq!(name.type_(), f.type_());
                self.env.insert(name, f);
            }
        }
    }

    fn eval_block(&mut self, block: expr::Block) -> Value {
        for l in block.exprs.iter() {
            match l {
                expr::Let::NonRec { name, val } => {
                    let v = self.eval_expr(&val);
                    assert_eq!(name.type_(), v.type_());
                    self.env.insert(name.clone(), v);
                }
                expr::Let::Rec { name, params, body } => {
                    self.load_function(expr::Func {
                        name: name.clone(),
                        params: params.clone(),
                        body: body.clone(),
                    });
                }
            }
        }
        self.eval_expr(&block.term)
    }

    fn eval_expr(&mut self, expr: &expr::Expr) -> Value {
        use expr::Expr;
        match expr {
            Expr::Var(id) => self.env.get(id).unwrap().clone(),
            Expr::I32(i) => Value::I32(i.clone()),
            Expr::I64(i) => Value::I64(i.clone()),
            Expr::F32(f) => Value::F32(f.clone()),
            Expr::F64(f) => Value::F64(f.clone()),
            Expr::Bool(b) => Value::Bool(b.clone()),
            Expr::Char(c) => Value::Char(c.clone()),
            _ => panic!("not implemented"),
        }
    }
}
