mod expr;
mod parser;
mod vm;
use combine::Parser;
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        writeln!(std::io::stderr(), "Usage: {} FILE", args[0])?;
        std::process::exit(1);
    }

    let src = File::open(&args[1])?;
    let mut buf = BufReader::new(src);
    let mut contents = String::new();
    buf.read_to_string(&mut contents)?;

    let (ir, _) = parser::parser().parse(&*contents).unwrap();

    let mut vm = vm::VM::new();
    vm.eval(ir.0, ir.1);

    Ok(())
}
