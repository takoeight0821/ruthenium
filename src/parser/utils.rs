use combine::*;
use combine::parser::char::{char, digit, spaces};

pub fn with_parens<P>(p: P) -> impl Parser<Input = P::Input, Output = P::Output>
where
    P: Parser,
    P::Input: Stream<Item = char>,
    <P::Input as StreamOnce>::Error: ParseError<
        <P::Input as StreamOnce>::Item,
        <P::Input as StreamOnce>::Range,
        <P::Input as StreamOnce>::Position,
    >,
{
    between(lex(char('(')), lex(char(')')), p)
}

pub fn lex<P>(p: P) -> impl Parser<Input = P::Input, Output = P::Output>
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

pub fn parse_u8<I>() -> impl Parser<Input = I, Output = u8>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    lex_natural().map(|s| u8::from_str_radix(&s, 10).expect("u8"))
}

pub fn parse_i32<I>() -> impl Parser<Input = I, Output = i32>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    lex_natural().map(|s| i32::from_str_radix(&s, 10).expect("i32"))
}
