use expr::{Block, Expr, Func, HasType, Id, Let, Program, Type};
use std::collections::HashMap;
use std::f32;
use std::f64;
use std::rc::Rc;

pub type PrimFunc = dyn Fn(&VM, &[Value]) -> Value;

#[derive(Clone)]
pub struct VM {
    env: HashMap<Id, Value>,
    prims: HashMap<String, Rc<PrimFunc>>,
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

impl VM {
    pub fn new() -> Self {
        let mut vm = VM {
            env: HashMap::new(),
            prims: HashMap::new(),
        };
        vm.prims.insert("add".to_string(), Rc::new(VM::add));
        vm.prims.insert("sub".to_string(), Rc::new(VM::sub));
        vm.prims.insert("mul".to_string(), Rc::new(VM::mul));
        vm.prims.insert("div".to_string(), Rc::new(VM::div));
        vm.prims.insert("mod".to_string(), Rc::new(VM::modulo));
        vm.prims.insert("eq".to_string(), Rc::new(VM::eq));
        vm.prims.insert("neq".to_string(), Rc::new(VM::neq));
        vm.prims.insert("lt".to_string(), Rc::new(VM::lt));
        vm.prims.insert("gt".to_string(), Rc::new(VM::gt));
        vm.prims.insert("le".to_string(), Rc::new(VM::le));
        vm.prims.insert("ge".to_string(), Rc::new(VM::ge));
        vm.prims.insert("and".to_string(), Rc::new(VM::and));
        vm.prims.insert("or".to_string(), Rc::new(VM::or));
        vm.prims.insert("puts".to_string(), Rc::new(VM::puts));
        vm.prims
            .insert("to_string".to_string(), Rc::new(VM::to_string));
        vm
    }

    fn add(&self, args: &[Value]) -> Value {
        match args[..] {
            [Value::I32(a), Value::I32(b)] => Value::I32(a + b),
            [Value::I64(a), Value::I64(b)] => Value::I64(a + b),
            [Value::F32(a), Value::F32(b)] => Value::F32(a + b),
            [Value::F64(a), Value::F64(b)] => Value::F64(a + b),
            _ => panic!("invalid args for add: {:?}", args),
        }
    }

    fn sub(&self, args: &[Value]) -> Value {
        match args[..] {
            [Value::I32(a), Value::I32(b)] => Value::I32(a - b),
            [Value::I64(a), Value::I64(b)] => Value::I64(a - b),
            [Value::F32(a), Value::F32(b)] => Value::F32(a - b),
            [Value::F64(a), Value::F64(b)] => Value::F64(a - b),
            _ => panic!("invalid args for sub: {:?}", args),
        }
    }

    fn mul(&self, args: &[Value]) -> Value {
        match args[..] {
            [Value::I32(a), Value::I32(b)] => Value::I32(a * b),
            [Value::I64(a), Value::I64(b)] => Value::I64(a * b),
            [Value::F32(a), Value::F32(b)] => Value::F32(a * b),
            [Value::F64(a), Value::F64(b)] => Value::F64(a * b),
            _ => panic!("invalid args for mul: {:?}", args),
        }
    }

    fn div(&self, args: &[Value]) -> Value {
        match args[..] {
            [Value::I32(a), Value::I32(b)] => Value::I32(a / b),
            [Value::I64(a), Value::I64(b)] => Value::I64(a / b),
            [Value::F32(a), Value::F32(b)] => Value::F32(a / b),
            [Value::F64(a), Value::F64(b)] => Value::F64(a / b),
            _ => panic!("invalid args for div: {:?}", args),
        }
    }

    fn modulo(&self, args: &[Value]) -> Value {
        match args[..] {
            [Value::I32(a), Value::I32(b)] => Value::I32(a % b),
            [Value::I64(a), Value::I64(b)] => Value::I64(a % b),
            _ => panic!("invalid args for mod: {:?}", args),
        }
    }

    fn eq(&self, args: &[Value]) -> Value {
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
                    .cloned()
                    .zip(ys.iter().cloned())
                    .map(|(x, y)| self.eq(&[x, y]))
                    .all(|x| x == Value::Bool(true)),
            ),
            _ => panic!("invalid args for eq: {:?}", args),
        }
    }

    fn neq(&self, args: &[Value]) -> Value {
        match args[..] {
            [Value::I32(a), Value::I32(b)] => Value::Bool(a != b),
            [Value::I64(a), Value::I64(b)] => Value::Bool(a != b),
            [Value::F32(a), Value::F32(b)] => Value::Bool((a - b).abs() < f32::EPSILON),
            [Value::F64(a), Value::F64(b)] => Value::Bool((a - b).abs() < f64::EPSILON),
            [Value::Bool(a), Value::Bool(b)] => Value::Bool(a != b),
            [Value::Char(a), Value::Char(b)] => Value::Bool(a != b),
            [Value::String(ref a), Value::String(ref b)] => Value::Bool(a != b),
            [Value::Tuple(ref xs), Value::Tuple(ref ys)] => Value::Bool(
                xs.iter()
                    .cloned()
                    .zip(ys.iter().cloned())
                    .map(|(x, y)| self.neq(&[x, y]))
                    .all(|x| x == Value::Bool(true)),
            ),
            _ => panic!("invalid args for neq: {:?}", args),
        }
    }

    fn lt(&self, args: &[Value]) -> Value {
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
                    .cloned()
                    .zip(ys.iter().cloned())
                    .map(|(x, y)| self.lt(&[x, y]))
                    .all(|x| x == Value::Bool(true)),
            ),
            _ => panic!("invalid args for lt: {:?}", args),
        }
    }

    fn gt(&self, args: &[Value]) -> Value {
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
                    .cloned()
                    .zip(ys.iter().cloned())
                    .map(|(x, y)| self.gt(&[x, y]))
                    .all(|x| x == Value::Bool(true)),
            ),
            _ => panic!("invalid args for gt: {:?}", args),
        }
    }

    fn le(&self, args: &[Value]) -> Value {
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
                    .cloned()
                    .zip(ys.iter().cloned())
                    .map(|(x, y)| self.le(&[x, y]))
                    .all(|x| x == Value::Bool(true)),
            ),
            _ => panic!("invalid args for le: {:?}", args),
        }
    }

    fn ge(&self, args: &[Value]) -> Value {
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
                    .cloned()
                    .zip(ys.iter().cloned())
                    .map(|(x, y)| self.ge(&[x, y]))
                    .all(|x| x == Value::Bool(true)),
            ),
            _ => panic!("invalid args for ge: {:?}", args),
        }
    }

    fn and(&self, args: &[Value]) -> Value {
        match args[..] {
            [Value::Bool(a), Value::Bool(b)] => Value::Bool(a & b),
            _ => panic!("invalid args for and: {:?}", args),
        }
    }

    fn or(&self, args: &[Value]) -> Value {
        match args[..] {
            [Value::Bool(a), Value::Bool(b)] => Value::Bool(a | b),
            _ => panic!("invalid args for or: {:?}", args),
        }
    }

    fn puts(&self, args: &[Value]) -> Value {
        match args[..] {
            [Value::String(ref x)] => print!("{}", x),
            _ => panic!("invalid args for puts: {:?}", args),
        }
        Value::Tuple(Rc::new(Vec::new()))
    }

    fn to_string(&self, args: &[Value]) -> Value {
        fn to_string_helper(x: &[Value]) -> String {
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
                        .map(|x| to_string_helper(&[x]))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                _ => panic!("invalid args for to_string: {:?}", x),
            }
        }
        Value::String(Rc::new(to_string_helper(args)))
    }

    pub fn eval(&mut self, program: Program, entry: Block) -> Value {
        self.load_functions(program.program);
        self.eval_block(entry)
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
                    params,
                    env: self.env.clone(),
                    body,
                };
                assert_eq!(name.type_of(), f.type_of());
                self.env.insert(name, f);
            }
        }
    }

    fn eval_block(&mut self, block: Block) -> Value {
        let env_backup = self.env.clone();
        for l in block.exprs {
            match l {
                Let::NonRec { name, val } => {
                    let v = self.eval_expr(val);
                    assert_eq!(name.type_of(), v.type_of());
                    self.env.insert(name, v);
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

                    self.load_function(Func { name, params, body });
                }
            }
        }
        let val = self.eval_expr(block.term);
        self.env = env_backup;
        val
    }

    pub fn eval_expr(&mut self, expr: Expr) -> Value {
        match expr {
            Expr::Var(id) => self.env[&id].clone(),
            Expr::I32(i) => Value::I32(i),
            Expr::I64(i) => Value::I64(i),
            Expr::F32(f) => Value::F32(f),
            Expr::F64(f) => Value::F64(f),
            Expr::Bool(b) => Value::Bool(b),
            Expr::Char(c) => Value::Char(c),
            Expr::String(s) => Value::String(Rc::new(s)),
            Expr::Tuple(xs) => {
                Value::Tuple(Rc::new(xs.iter().map(|x| self.env[x].clone()).collect()))
            }
            Expr::Access(x, i) => match &self.env[&x] {
                Value::Tuple(xs) => xs.iter().nth(i).unwrap().clone(),
                x => panic!("{:?} is not tuple", x),
            },
            Expr::Apply(f, args) => {
                let f = self.env[&f].clone();
                let args: Vec<_> = args.iter().map(|x| self.env[x].clone()).collect();
                match f {
                    Value::Func {
                        params,
                        mut env,
                        body,
                    } => {
                        assert_eq!(params.len(), args.len());
                        params.into_iter().zip(args.into_iter()).for_each(|(p, a)| {
                            assert_eq!(p.type_of(), a.type_of());
                            env.insert(p, a);
                        });
                        let mut vm = VM::new();
                        vm.env = env;
                        vm.eval_block(body)
                    }
                    Value::Prim(key, _) => {
                        let f = &self.prims[&key];
                        f(&self, &args)
                    }
                    v => panic!("{:?} is not appliable", v),
                }
            }
            Expr::Prim(key, t) => Value::Prim(key, t),
            Expr::If(c, t, f) => match self.env[&c].clone() {
                Value::Bool(false) => self.eval_block(*f),
                Value::Bool(true) => self.eval_block(*t),
                v => panic!("{:?} is not boolean", v),
            },
        }
    }
}
