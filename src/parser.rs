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
    (lex(char('<')), int_ty, lex(char('>'))).map(|(_, t, _)| t)
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
