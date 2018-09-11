#[cfg(test)]
use super::*;
#[allow(unused_imports)]
use expr::Expr::*;
#[allow(unused_imports)]
use expr::*;

#[test]
fn test_underscore() {
    assert_eq!(
        parse_id().parse("[foo_bar <int 32>]"),
        Ok((Id("foo_bar".to_string(), Type::Int(32)), ""))
    );
    assert_eq!(
        parse_expr().parse("[ #foo_bar <int 32> ]"),
        Ok((Prim("foo_bar".to_string(), Type::Int(32)), ""))
    );
}

#[test]
fn test_expr() {
    assert_eq!(
        parse_expr().parse("[hoge<int 32>]"),
        Ok((Var(Id("hoge".to_string(), Type::Int(32))), ""))
    );
    assert_eq!(parse_expr().parse("( i32 42 ) "), Ok((I32(42), "")));
    assert_eq!(parse_expr().parse("(i32 -42)"), Ok((I32(-42), "")));
    assert_eq!(parse_expr().parse("( f32 3.1 ) "), Ok((F32(3.1), "")));
    assert_eq!(parse_expr().parse("(f64 3.14)"), Ok((F64(3.14), "")));
    assert_eq!(parse_expr().parse("(f64 -3.14)"), Ok((F64(-3.14), "")));
    assert_eq!(parse_expr().parse("( bool true ) "), Ok((Bool(true), "")));
    assert_eq!(parse_expr().parse("(bool false)"), Ok((Bool(false), "")));
    assert_eq!(parse_expr().parse("( char 'c' ) "), Ok((Char('c'), "")));
    assert_eq!(
        parse_expr().parse(r#"(string "ho\"ge")"#),
        Ok((String("ho\"ge".to_string()), ""))
    );
    assert_eq!(
        parse_expr().parse("(tuple [hoge<int 32>] [fuga<float 64>])"),
        Ok((
            Tuple(vec![
                Id("hoge".to_string(), Type::Int(32)),
                Id("fuga".to_string(), Type::Float64)
            ]),
            ""
        ))
    );
    assert_eq!(
        parse_expr().parse("(access [hoge <tuple <int 32> <int 32>>] 0)"),
        Ok((
            Access(
                Id(
                    "hoge".to_string(),
                    Type::Tuple(vec![Type::Int(32), Type::Int(32)])
                ),
                0
            ),
            ""
        ))
    );
    assert_eq!(
        parse_expr().parse("(apply [f <fn (<int 32>) <int 32>>] [x <int 32>] )"),
        Ok((
            Apply(
                Id(
                    "f".to_string(),
                    Type::Function {
                        codom: Box::new(Type::Int(32)),
                        dom: vec![Type::Int(32)]
                    }
                ),
                vec![Id("x".to_string(), Type::Int(32))]
            ),
            ""
        ))
    );
    assert_eq!(
        parse_expr().parse("[ #hoge <int 32> ]"),
        Ok((Prim("hoge".to_string(), Type::Int(32)), ""))
    );
    assert_eq!(
        parse_expr()
            .parse("(if [c <int 1>] { (let [a <int 32>] (i32 42)) return [a <int 32>] } { return (i32 43) } )"),
        Ok((
            If(
                Id("c".to_string(), Type::Int(1)),
                Box::new(Block {
                    exprs: vec![Let::NonRec {
                        name: Id("a".to_string(), Type::Int(32)),
                        val: I32(42)
                    }],
                    term: Var(Id("a".to_string(), Type::Int(32)))
                }),
                Box::new(Block {
                    exprs: vec![],
                    term: I32(43)
                })
            ),
            ""
        ))
    );
}

#[test]
fn test_block() {
    assert_eq!(
        parse_block().parse("{\n(let [a <int 32>] (i32 42))\nreturn [a <int 32>]\n}"),
        Ok((
            Block {
                exprs: vec![Let::NonRec {
                    name: Id("a".to_string(), Type::Int(32)),
                    val: I32(42)
                }],
                term: Var(Id("a".to_string(), Type::Int(32)))
            },
            ""
        ))
    );
    assert_eq!(
        parse_block().parse("{ return (i32 43) }"),
        Ok((
            Block {
                exprs: vec![],
                term: I32(43)
            },
            ""
        ))
    );
}

#[test]
fn test_let() {
    assert_eq!(
        parse_let().parse("(let [a <int 32>] (i32 42))"),
        Ok((
            Let::NonRec {
                name: Id("a".to_string(), Type::Int(32)),
                val: I32(42)
            },
            ""
        ))
    )
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
