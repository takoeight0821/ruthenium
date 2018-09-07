extern crate combine;
extern crate num;
mod expr;
mod parser;
mod vm;
use combine::Parser;
use expr::HasType;

fn main() {
    let mut vm = vm::VM::new();
    let expr = parser::parse_expr().parse("(i32 42)").unwrap().0;
    println!("{:?}", expr);
    let val = vm.eval_expr(expr);
    println!("{:?} : {}", val, val.type_of());
}
