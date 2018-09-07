#[cfg(test)]
use super::*;

#[test]
fn test_expr() {
    assert_eq!(
        parse_expr().parse("[hoge<int 32>]"),
        Ok((
            expr::Expr::Var(expr::Id {
                name: "hoge".to_string(),
                ty: expr::Type::Int(32),
            }),
            ""
        ))
    );
    assert_eq!(
        parse_expr().parse("(i32 42)"),
        Ok((expr::Expr::I32(42), ""))
    );
}

#[test]
fn test_id() {
    assert_eq!(
        parse_id().parse("[hoge<int 32>]"),
        Ok((
            expr::Id {
                name: "hoge".to_string(),
                ty: expr::Type::Int(32)
            },
            ""
        ))
    );
    assert_eq!(
        parse_id().parse("[  hoge  <  int 32  >  ]  "),
        Ok((
            expr::Id {
                name: "hoge".to_string(),
                ty: expr::Type::Int(32)
            },
            ""
        ))
    );
}

#[test]
fn test_type() {
    assert_eq!(
        parse_type().parse("< int 32 >"),
        Ok((expr::Type::Int(32), ""))
    );
    assert_eq!(
        parse_type().parse("<int 64>"),
        Ok((expr::Type::Int(64), ""))
    );
    assert_eq!(
        parse_type().parse("< float 32 > hoge"),
        Ok((expr::Type::Float32, "hoge"))
    );
    assert_eq!(
        parse_type().parse("<float 64>"),
        Ok((expr::Type::Float64, ""))
    );
    assert_eq!(
        parse_type().parse("< string >"),
        Ok((expr::Type::String, ""))
    );
    assert_eq!(
        parse_type().parse("<tuple>"),
        Ok((expr::Type::Tuple(vec![]), ""))
    );
    assert_eq!(
        parse_type().parse("<tuple <string> <int 32>>"),
        Ok((
            expr::Type::Tuple(vec![expr::Type::String, expr::Type::Int(32)]),
            ""
        ))
    );
    assert_eq!(
        parse_type().parse("<tuple <string><int 32> <tuple>>"),
        Ok((
            expr::Type::Tuple(vec![
                expr::Type::String,
                expr::Type::Int(32),
                expr::Type::Tuple(vec![])
            ]),
            ""
        ))
    );
    assert_eq!(
        parse_type().parse("<fn () <string>>"),
        Ok((
            expr::Type::Function {
                codom: Box::new(expr::Type::String),
                dom: vec![],
            },
            ""
        ))
    );
    assert_eq!(
        parse_type().parse("<fn (<int 32> <int 32>) <int 32>>"),
        Ok((
            expr::Type::Function {
                codom: Box::new(expr::Type::Int(32)),
                dom: vec![expr::Type::Int(32), expr::Type::Int(32)],
            },
            ""
        ))
    );
}
