mod expr;
mod vm;
use std::io;

fn main() -> io::Result<()> {
    println!("{:?}", expr::Expr::I32(32));

    let mut vm = vm::VM::new();
    let expr = expr::Expr::String("hello, world\n".to_string());
    let val = vm.eval_expr(&expr);
    println!("{:?}", val);
    Ok(())
}
