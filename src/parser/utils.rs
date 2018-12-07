use combine::error::{Consumed, ParseError};
use combine::parser::char::{alpha_num, char, digit, spaces};
use combine::parser::choice::optional;
use combine::parser::item::{any, satisfy_map};
use combine::*;
use num::Num;
use std::fmt::Debug;

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

pub fn parse_uint<I, N>() -> impl Parser<Input = I, Output = N>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
    N: Num,
    N::FromStrRadixErr: Debug,
{
    lex_natural().map(|s| Num::from_str_radix(&s, 10).unwrap())
}

fn lex_integer<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    lex((optional(char('-')), many1(digit()))).map(|(c, mut cs): (Option<char>, String)| {
        if let Some(c) = c {
            cs.insert(0, c)
        }
        cs
    })
}

pub fn parse_int<I, N>() -> impl Parser<Input = I, Output = N>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
    N: Num,
    N::FromStrRadixErr: Debug,
{
    lex_integer().map(|s| Num::from_str_radix(&s, 10).unwrap())
}

fn lex_float<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    lex((
        optional(char('-')),
        many1(digit()),
        char('.'),
        many1(digit()),
    ))
    .map(|(sign, mut cs1, _, cs2): (_, String, _, String)| {
        if let Some(c) = sign {
            cs1.insert(0, c)
        }
        cs1.push('.');
        cs1 + &*cs2
    })
}

pub fn parse_float<I, N>() -> impl Parser<Input = I, Output = N>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
    N: Num,
    N::FromStrRadixErr: Debug,
{
    lex_float().map(|s| Num::from_str_radix(&s, 10).unwrap())
}

#[test]
fn test_parse_float() {
    assert_eq!(parse_float().parse("-123.45"), Ok((-123.45, "")));
}

pub fn satisfy_char<I>() -> impl Parser<Input = I, Output = char>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    parser(|input: &mut I| {
        let (c, comsumed) = any().parse_stream(input)?;
        let mut back_slash_char = satisfy_map(|c| {
            Some(match c {
                '\'' => '\'',
                '"' => '"',
                '\\' => '\\',
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                _ => return None,
            })
        });
        match c {
            '\\' => comsumed.combine(|_| back_slash_char.parse_stream(input)),
            '"' => Err(Consumed::Empty(I::Error::empty(input.position()).into())),
            _ => Ok((c, comsumed)),
        }
    })
}

#[test]
fn test_satisfy_char() {
    assert_eq!(satisfy_char().parse("h"), Ok(('h', "")));
    assert_eq!(satisfy_char().parse(r"\'"), Ok(('\'', "")));
    assert_eq!(satisfy_char().parse(r#"\""#), Ok(('\"', "")));
    assert_eq!(satisfy_char().parse(r"\n"), Ok(('\n', "")));
    assert_eq!(satisfy_char().parse("字"), Ok(('字', "")));
}

pub fn parse_ident<I>() -> impl Parser<Input = I, Output = String>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    many(alpha_num().or(satisfy(|c| c == '_')))
}
