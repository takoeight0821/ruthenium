use expr;
use std::collections::HashMap;
use std::rc::Rc;

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
        let env_copy = self.env.clone();
        match func {
            expr::Func { name, params, body } => {
                self.env.insert(
                    name,
                    Value::Func {
                        params: params,
                        env: env_copy,
                        body: body,
                    },
                );
            }
        }
    }

    fn eval_block(&mut self, block: expr::Block) -> Value {
        for l in block.exprs.iter() {
            match l {
                expr::Let::NonRec { name, val } => {
                    let v = self.eval_expr(&val);
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
            Expr::Var(id) => {
                self.env.get(id).unwrap().clone()
            },
            Expr::I32(i) => Value::I32(i.clone()),
            Expr::I64(i) => Value::I64(i.clone()),
            Expr::F32(f) => Value::F32(f.clone()),
            Expr::F64(f) => Value::F64(f.clone()),
            Expr::Bool(b) => Value::Bool(b.clone()),
            Expr::Char(c) => Value::Char(c.clone()),
            _ => { panic!("not implemented") }
        }
    }
}
