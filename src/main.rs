mod expr;
mod types;
mod vm;
use std::io;

fn main() -> io::Result<()> {
    println!("{:?}", expr::Expr::I32(32));
    Ok(())
}
