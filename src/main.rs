extern crate combine;
extern crate num;
mod expr;
mod parser;
mod vm;
use combine::Parser;

fn main() {
    println!("{:?}", expr::Expr::I32(32));

    let mut vm = vm::VM::new();
    let expr = expr::Expr::String("hello, world\n".to_string());
    let val = vm.eval_expr(&expr);

    println!("{:?}", val);

    let ty = parser::parse_type().parse("< int   32  >   ");
    println!("{:?}", ty);
}
