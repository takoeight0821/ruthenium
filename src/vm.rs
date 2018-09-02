use expr::{Block, Expr, Func, HasType, Id, Let, Program, Type};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct VM {
    env: HashMap<Id, Value>,
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
        params: Vec<Id>,
        env: HashMap<Id, Value>,
        body: Block,
    },
}

impl HasType for Value {
    fn type_of(&self) -> Type {
        use self::Value::*;
        match self {
            I32(_) => Type::Int(32),
            I64(_) => Type::Int(64),
            F32(_) => Type::Float32,
            F64(_) => Type::Float64,
            Bool(_) => Type::Int(1),
            Char(_) => Type::Int(8),
            String(_) => Type::String,
            Tuple(xs) => Type::Tuple(xs.iter().map(HasType::type_of).collect()),
            Prim(_, t) => t.clone(),
            Func { params, body, .. } => Type::Function {
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
        prims.insert("add".to_string(), Rc::new(VM::add));
        prims.insert("sub".to_string(), Rc::new(VM::sub));
        prims.insert("mul".to_string(), Rc::new(VM::mul));
        prims.insert("div".to_string(), Rc::new(VM::div));
        prims.insert("mod".to_string(), Rc::new(VM::modulo));
        prims.insert("eq".to_string(), Rc::new(VM::eq));
        prims.insert("neq".to_string(), Rc::new(VM::neq));
        prims.insert("lt".to_string(), Rc::new(VM::lt));
        prims.insert("gt".to_string(), Rc::new(VM::gt));
        prims.insert("le".to_string(), Rc::new(VM::le));
        prims.insert("ge".to_string(), Rc::new(VM::ge));
        prims.insert("and".to_string(), Rc::new(VM::and));
        prims.insert("or".to_string(), Rc::new(VM::or));
        prims.insert("puts".to_string(), Rc::new(VM::puts));
        prims.insert("to_string".to_string(), Rc::new(VM::to_string));
        VM {
            env: HashMap::new(),
            prims: prims,
        }
    }

    fn add(&self, args: Vec<Value>) -> Value {
        match args[..] {
            [Value::I32(a), Value::I32(b)] => Value::I32(a + b),
            [Value::I64(a), Value::I64(b)] => Value::I64(a + b),
            [Value::F32(a), Value::F32(b)] => Value::F32(a + b),
            [Value::F64(a), Value::F64(b)] => Value::F64(a + b),
            _ => panic!("invalid args for add: {:?}", args),
        }
    }

    fn sub(&self, args: Vec<Value>) -> Value {
        match args[..] {
            [Value::I32(a), Value::I32(b)] => Value::I32(a - b),
            [Value::I64(a), Value::I64(b)] => Value::I64(a - b),
            [Value::F32(a), Value::F32(b)] => Value::F32(a - b),
            [Value::F64(a), Value::F64(b)] => Value::F64(a - b),
            _ => panic!("invalid args for sub: {:?}", args),
        }
    }

    fn mul(&self, args: Vec<Value>) -> Value {
        match args[..] {
            [Value::I32(a), Value::I32(b)] => Value::I32(a * b),
            [Value::I64(a), Value::I64(b)] => Value::I64(a * b),
            [Value::F32(a), Value::F32(b)] => Value::F32(a * b),
            [Value::F64(a), Value::F64(b)] => Value::F64(a * b),
            _ => panic!("invalid args for mul: {:?}", args),
        }
    }

    fn div(&self, args: Vec<Value>) -> Value {
        match args[..] {
            [Value::I32(a), Value::I32(b)] => Value::I32(a / b),
            [Value::I64(a), Value::I64(b)] => Value::I64(a / b),
            [Value::F32(a), Value::F32(b)] => Value::F32(a / b),
            [Value::F64(a), Value::F64(b)] => Value::F64(a / b),
            _ => panic!("invalid args for div: {:?}", args),
        }
    }

    fn modulo(&self, args: Vec<Value>) -> Value {
        match args[..] {
            [Value::I32(a), Value::I32(b)] => Value::I32(a % b),
            [Value::I64(a), Value::I64(b)] => Value::I64(a % b),
            _ => panic!("invalid args for mod: {:?}", args),
        }
    }

    fn eq(&self, args: Vec<Value>) -> Value {
        match args[..] {
            [Value::I32(a), Value::I32(b)] => Value::Bool(a == b),
            [Value::I64(a), Value::I64(b)] => Value::Bool(a == b),
            [Value::F32(a), Value::F32(b)] => Value::Bool(a == b),
            [Value::F64(a), Value::F64(b)] => Value::Bool(a == b),
            [Value::Bool(a), Value::Bool(b)] => Value::Bool(a == b),
            [Value::Char(a), Value::Char(b)] => Value::Bool(a == b),
            [Value::String(ref a), Value::String(ref b)] => Value::Bool(a == b),
            [Value::Tuple(ref xs), Value::Tuple(ref ys)] => Value::Bool(
                xs.iter()
                    .zip(ys.iter())
                    .map(|(x, y)| self.eq(vec![x.clone(), y.clone()]))
                    .all(|x| x == Value::Bool(true)),
            ),
            _ => panic!("invalid args for eq: {:?}", args),
        }
    }

    fn neq(&self, args: Vec<Value>) -> Value {
        match args[..] {
            [Value::I32(a), Value::I32(b)] => Value::Bool(a != b),
            [Value::I64(a), Value::I64(b)] => Value::Bool(a != b),
            [Value::F32(a), Value::F32(b)] => Value::Bool(a != b),
            [Value::F64(a), Value::F64(b)] => Value::Bool(a != b),
            [Value::Bool(a), Value::Bool(b)] => Value::Bool(a != b),
            [Value::Char(a), Value::Char(b)] => Value::Bool(a != b),
            [Value::String(ref a), Value::String(ref b)] => Value::Bool(a != b),
            [Value::Tuple(ref xs), Value::Tuple(ref ys)] => Value::Bool(
                xs.iter()
                    .zip(ys.iter())
                    .map(|(x, y)| self.neq(vec![x.clone(), y.clone()]))
                    .all(|x| x == Value::Bool(true)),
            ),
            _ => panic!("invalid args for neq: {:?}", args),
        }
    }

    fn lt(&self, args: Vec<Value>) -> Value {
        match args[..] {
            [Value::I32(a), Value::I32(b)] => Value::Bool(a < b),
            [Value::I64(a), Value::I64(b)] => Value::Bool(a < b),
            [Value::F32(a), Value::F32(b)] => Value::Bool(a < b),
            [Value::F64(a), Value::F64(b)] => Value::Bool(a < b),
            [Value::Bool(a), Value::Bool(b)] => Value::Bool(a < b),
            [Value::Char(a), Value::Char(b)] => Value::Bool(a < b),
            [Value::String(ref a), Value::String(ref b)] => Value::Bool(a < b),
            [Value::Tuple(ref xs), Value::Tuple(ref ys)] => Value::Bool(
                xs.iter()
                    .zip(ys.iter())
                    .map(|(x, y)| self.lt(vec![x.clone(), y.clone()]))
                    .all(|x| x == Value::Bool(true)),
            ),
            _ => panic!("invalid args for lt: {:?}", args),
        }
    }

    fn gt(&self, args: Vec<Value>) -> Value {
        match args[..] {
            [Value::I32(a), Value::I32(b)] => Value::Bool(a > b),
            [Value::I64(a), Value::I64(b)] => Value::Bool(a > b),
            [Value::F32(a), Value::F32(b)] => Value::Bool(a > b),
            [Value::F64(a), Value::F64(b)] => Value::Bool(a > b),
            [Value::Bool(a), Value::Bool(b)] => Value::Bool(a > b),
            [Value::Char(a), Value::Char(b)] => Value::Bool(a > b),
            [Value::String(ref a), Value::String(ref b)] => Value::Bool(a > b),
            [Value::Tuple(ref xs), Value::Tuple(ref ys)] => Value::Bool(
                xs.iter()
                    .zip(ys.iter())
                    .map(|(x, y)| self.gt(vec![x.clone(), y.clone()]))
                    .all(|x| x == Value::Bool(true)),
            ),
            _ => panic!("invalid args for gt: {:?}", args),
        }
    }

    fn le(&self, args: Vec<Value>) -> Value {
        match args[..] {
            [Value::I32(a), Value::I32(b)] => Value::Bool(a <= b),
            [Value::I64(a), Value::I64(b)] => Value::Bool(a <= b),
            [Value::F32(a), Value::F32(b)] => Value::Bool(a <= b),
            [Value::F64(a), Value::F64(b)] => Value::Bool(a <= b),
            [Value::Bool(a), Value::Bool(b)] => Value::Bool(a <= b),
            [Value::Char(a), Value::Char(b)] => Value::Bool(a <= b),
            [Value::String(ref a), Value::String(ref b)] => Value::Bool(a <= b),
            [Value::Tuple(ref xs), Value::Tuple(ref ys)] => Value::Bool(
                xs.iter()
                    .zip(ys.iter())
                    .map(|(x, y)| self.le(vec![x.clone(), y.clone()]))
                    .all(|x| x == Value::Bool(true)),
            ),
            _ => panic!("invalid args for le: {:?}", args),
        }
    }

    fn ge(&self, args: Vec<Value>) -> Value {
        match args[..] {
            [Value::I32(a), Value::I32(b)] => Value::Bool(a >= b),
            [Value::I64(a), Value::I64(b)] => Value::Bool(a >= b),
            [Value::F32(a), Value::F32(b)] => Value::Bool(a >= b),
            [Value::F64(a), Value::F64(b)] => Value::Bool(a >= b),
            [Value::Bool(a), Value::Bool(b)] => Value::Bool(a >= b),
            [Value::Char(a), Value::Char(b)] => Value::Bool(a >= b),
            [Value::String(ref a), Value::String(ref b)] => Value::Bool(a >= b),
            [Value::Tuple(ref xs), Value::Tuple(ref ys)] => Value::Bool(
                xs.iter()
                    .zip(ys.iter())
                    .map(|(x, y)| self.ge(vec![x.clone(), y.clone()]))
                    .all(|x| x == Value::Bool(true)),
            ),
            _ => panic!("invalid args for ge: {:?}", args),
        }
    }

    fn and(&self, args: Vec<Value>) -> Value {
        match args[..] {
            [Value::Bool(a), Value::Bool(b)] => Value::Bool(a & b),
            _ => panic!("invalid args for and: {:?}", args),
        }
    }

    fn or(&self, args: Vec<Value>) -> Value {
        match args[..] {
            [Value::Bool(a), Value::Bool(b)] => Value::Bool(a | b),
            _ => panic!("invalid args for or: {:?}", args),
        }
    }

    fn puts(&self, args: Vec<Value>) -> Value {
        match args[..] {
            [Value::String(ref x)] => print!("{}", x),
            _ => panic!("invalid args for puts: {:?}", args),
        }
        Value::Tuple(Rc::new(Vec::new()))
    }

    fn to_string(&self, args: Vec<Value>) -> Value {
        fn to_string_helper(x: Vec<Value>) -> String {
            match x[..] {
                [Value::I32(x)] => format!("{}", x),
                [Value::I64(x)] => format!("{}", x),
                [Value::F32(x)] => format!("{}", x),
                [Value::F64(x)] => format!("{}", x),
                [Value::Bool(x)] => format!("{}", x),
                [Value::Char(x)] => format!("{}", x),
                [Value::String(ref x)] => format!("{}", x),
                [Value::Tuple(ref xs)] => format!(
                    "{{{}}}",
                    xs.iter()
                        .cloned()
                        .map(|x| to_string_helper(vec![x]))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                _ => panic!("invalid args for to_string: {:?}", x),
            }
        }
        let s = to_string_helper(args);
        Value::String(Rc::new(s))
    }

    pub fn eval(&mut self, program: Program, entry: Block) {
        self.load_functions(program.program);
        self.eval_block(entry);
    }

    fn load_functions(&mut self, funcs: Vec<Func>) {
        for f in funcs {
            self.load_function(f);
        }
    }

    fn load_function(&mut self, func: Func) {
        match func {
            Func { name, params, body } => {
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

    fn eval_block(&mut self, block: Block) -> Value {
        for l in block.exprs.iter() {
            match l {
                Let::NonRec { name, val } => {
                    let v = self.eval_expr(&val);
                    assert_eq!(name.type_of(), v.type_of());
                    self.env.insert(name.clone(), v);
                }
                Let::Rec { name, params, body } => {
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

                    self.load_function(Func {
                        name: name.clone(),
                        params: params.clone(),
                        body: body.clone(),
                    });
                }
            }
        }
        self.eval_expr(&block.term)
    }

    pub fn eval_expr(&mut self, expr: &Expr) -> Value {
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
