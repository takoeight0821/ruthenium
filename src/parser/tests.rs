#[cfg(test)]
use super::*;
#[allow(unused_imports)]
use expr::Expr::*;
#[allow(unused_imports)]
use expr::*;

#[test]
fn test_expr() {
    assert_eq!(
        parse_expr().parse("[hoge<int 32>]"),
        Ok((Var(Id("hoge".to_string(), Type::Int(32),)), ""))
    );
    assert_eq!(parse_expr().parse("( i32 42 ) "), Ok((I32(42), "")));
    assert_eq!(parse_expr().parse("(i32 -42)"), Ok((I32(-42), "")));
    assert_eq!(parse_expr().parse("( f32 3.1 ) "), Ok((F32(3.1), "")));
    assert_eq!(parse_expr().parse("(f64 3.14)"), Ok((F64(3.14), "")));
    assert_eq!(parse_expr().parse("(f64 -3.14)"), Ok((F64(-3.14), "")));
    assert_eq!(parse_expr().parse("( bool true ) "), Ok((Bool(true), "")));
    assert_eq!(parse_expr().parse("(bool false)"), Ok((Bool(false), "")));

}

#[test]
fn test_id() {
    assert_eq!(
        parse_id().parse("[hoge<int 32>]"),
        Ok((Id("hoge".to_string(), Type::Int(32)), ""))
    );
    assert_eq!(
        parse_id().parse("[  hoge  <  int 32  >  ]  "),
        Ok((Id("hoge".to_string(), Type::Int(32)), ""))
    );
}

#[test]
fn test_type() {
    assert_eq!(parse_type().parse("< int 32 >"), Ok((Type::Int(32), "")));
    assert_eq!(parse_type().parse("<int 64>"), Ok((Type::Int(64), "")));
    assert_eq!(
        parse_type().parse("< float 32 > hoge"),
        Ok((Type::Float32, "hoge"))
    );
    assert_eq!(parse_type().parse("<float 64>"), Ok((Type::Float64, "")));
    assert_eq!(parse_type().parse("< string >"), Ok((Type::String, "")));
    assert_eq!(parse_type().parse("<tuple>"), Ok((Type::Tuple(vec![]), "")));
    assert_eq!(
        parse_type().parse("<tuple <string> <int 32>>"),
        Ok((Type::Tuple(vec![Type::String, Type::Int(32)]), ""))
    );
    assert_eq!(
        parse_type().parse("<tuple <string><int 32> <tuple>>"),
        Ok((
            Type::Tuple(vec![Type::String, Type::Int(32), Type::Tuple(vec![])]),
            ""
        ))
    );
    assert_eq!(
        parse_type().parse("<fn () <string>>"),
        Ok((
            Type::Function {
                codom: Box::new(Type::String),
                dom: vec![],
            },
            ""
        ))
    );
    assert_eq!(
        parse_type().parse("<fn (<int 32> <int 32>) <int 32>>"),
        Ok((
            Type::Function {
                codom: Box::new(Type::Int(32)),
                dom: vec![Type::Int(32), Type::Int(32)],
            },
            ""
        ))
    );
}
