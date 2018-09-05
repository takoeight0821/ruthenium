use combine::parser::char::{char, digit, spaces, string};
use combine::*;
use expr;
use num::Num;

pub fn parse_type<I>() -> impl Parser<Input = I, Output = expr::Type>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    parse_type_()
}

parser!{
fn parse_type_[I]()(I) -> expr::Type
    where [ I: Stream<Item = char> ]
{
    let int_ty = (lex(string("int")), parse_u8()).map(|(_, i)| expr::Type::Int(i));
    let float32 = (lex(string("float")), string("32")).map(|_| expr::Type::Float32);
    let float64 = (lex(string("float")), string("64")).map(|_| expr::Type::Float64);
    let string_ty = lex(string("string")).map(|_| expr::Type::String);
    let tuple = (
        lex(string("tuple")),
        many(parse_type()),
    )
        .map(|(_, xs)| expr::Type::Tuple(xs));
    let func = (
        lex(string("fn")),
        between(lex(char('(')), lex(char(')')), many(parse_type())),
        parse_type(),
    )
        .map(|(_, dom, codom)| expr::Type::Function {
            dom: dom,
            codom: Box::new(codom),
        });

    between(lex(char('<')), lex(char('>')), choice((
        try(lex(int_ty)),
        try(lex(float32)),
        try(lex(float64)),
        try(lex(string_ty)),
        try(lex(tuple)),
        lex(func),
    )))
}
}

fn lex<P>(p: P) -> impl Parser<Input = P::Input, Output = P::Output>
where
    P: Parser,
    P::Input: Stream<Item = char>,
    <P::Input as StreamOnce>::Error: ParseError<
        <P::Input as StreamOnce>::Item,
        <P::Input as StreamOnce>::Range,
        <P::Input as StreamOnce>::Position,
    >,
{
    p.skip(spaces())
}

fn lex_natural<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    lex(many1(digit())).expected("unsigned integer")
}

fn parse_u8<I>() -> impl Parser<Input = I, Output = u8>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    lex_natural().map(|s| <u8 as Num>::from_str_radix(&s, 10).expect("u8"))
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
