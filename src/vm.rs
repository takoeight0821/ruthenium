use expr;
use expr::{HasType, Type};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct VM {
    env: HashMap<expr::Id, Value>,
    prims: HashMap<String, Rc<Fn(&VM, Vec<Value>) -> Value>>,
}

#[derive(PartialEq, Debug, Clone)]
#[allow(dead_code)]
pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Bool(bool),
    Char(char),
    String(Rc<String>),
    Tuple(Rc<Vec<Value>>),
    Prim(String, Type),
    Func {
        params: Vec<expr::Id>,
        env: HashMap<expr::Id, Value>,
        body: expr::Block,
    },
}

impl HasType for Value {
    fn type_of(&self) -> Type {
        match self {
            Value::I32(_) => Type::Int(32),
            Value::I64(_) => Type::Int(64),
            Value::F32(_) => Type::Float32,
            Value::F64(_) => Type::Float64,
            Value::Bool(_) => Type::Int(1),
            Value::Char(_) => Type::Int(8),
            Value::String(_) => Type::String,
            Value::Tuple(xs) => Type::Tuple(xs.iter().map(HasType::type_of).collect()),
            Value::Prim(_, t) => t.clone(),
            Value::Func { params, body, .. } => Type::Function {
                codom: Box::new(body.type_of()),
                dom: params.iter().map(HasType::type_of).collect(),
            },
        }
    }
}

#[allow(dead_code)]
impl VM {
    pub fn new() -> Self {
        let mut prims: HashMap<String, Rc<Fn(&VM, Vec<Value>) -> Value>> = HashMap::new();
        prims.insert("add_i32".to_string(), Rc::new(VM::add_i32));
        VM {
            env: HashMap::new(),
            prims: prims,
        }
    }

    fn add_i32(&self, args: Vec<Value>) -> Value {
        match args[..] {
            [Value::I32(a), Value::I32(b)] => Value::I32(a + b),
            _ => panic!("invalid args for add_i32: {:?}", args),
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
        match func {
            expr::Func { name, params, body } => {
                let f = Value::Func {
                    params: params,
                    env: self.env.clone(),
                    body: body,
                };
                assert_eq!(name.type_of(), f.type_of());
                self.env.insert(name, f);
            }
        }
    }

    fn eval_block(&mut self, block: expr::Block) -> Value {
        for l in block.exprs.iter() {
            match l {
                expr::Let::NonRec { name, val } => {
                    let v = self.eval_expr(&val);
                    assert_eq!(name.type_of(), v.type_of());
                    self.env.insert(name.clone(), v);
                }
                expr::Let::Rec { name, params, body } => {
                    match name.type_of() {
                        Type::Function { codom, dom } => {
                            assert_eq!(body.type_of(), codom.type_of());
                            assert_eq!(
                                params.iter().map(HasType::type_of).collect::<Vec<_>>(),
                                dom.iter().map(HasType::type_of).collect::<Vec<_>>()
                            );
                        }
                        t => panic!("{:?} is not function type", t),
                    };

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
            Expr::String(s) => Value::String(Rc::new(s.clone())),
            Expr::Tuple(xs) => Value::Tuple(Rc::new(
                xs.iter()
                    .map(|x| self.env.get(x).unwrap().clone())
                    .collect(),
            )),
            Expr::Access(x, i) => {
                let x = self.env.get(x).unwrap();
                match x {
                    Value::Tuple(xs) => xs.iter().nth(*i).unwrap().clone(),
                    _ => panic!("{:?} is not tuple", x),
                }
            }
            Expr::Apply(f, args) => {
                let f = self.env.get(f).unwrap();
                let args: Vec<_> = args
                    .iter()
                    .map(|x| self.env.get(x).unwrap().clone())
                    .collect();
                match f {
                    Value::Func { params, env, body } => {
                        let mut env = env.clone();
                        params.iter().zip(args.iter()).for_each(|(p, a)| {
                            env.insert(p.clone(), a.clone());
                        });
                        let mut vm = VM::new();
                        vm.env = env;
                        vm.eval_block(body.clone())
                    }
                    Value::Prim(key, _) => {
                        let f = self.prims.get(key).unwrap();
                        f(&self, args)
                    }
                    v => panic!("{:?} is not appliable", v),
                }
            }
            Expr::Prim(key, t) => Value::Prim(key.clone(), t.clone()),
            Expr::If(c, t, f) => {
                let c = self.env.get(c).unwrap().clone();
                match c {
                    Value::Bool(false) => self.eval_block(*f.clone()),
                    Value::Bool(true) => self.eval_block(*t.clone()),
                    v => panic!("{:?} is not boolean", v),
                }
            }
        }
    }
}
