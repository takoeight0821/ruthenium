use combine::parser::char::{char, digit, spaces, string};
use combine::*;
use expr;
use num::Num;

pub fn parse_type<I>() -> impl Parser<Input = I, Output = expr::Type>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let int_ty = (string("int"), spaces(), parse_u8()).map(|(_, _, i)| expr::Type::Int(i));
    let float32 = (string("float"), spaces(), string("32")).map(|_| expr::Type::Float32);
    let float64 = (string("float"), spaces(), string("64")).map(|_| expr::Type::Float64);

    lex((
        lex(char('<')),
        choice((lex(int_ty), try(lex(float32)), lex(float64))),
        lex(char('>')),
    )
        .map(|(_, t, _)| t))
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
    }
}
