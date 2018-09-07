mod tests;
mod utils;
use combine::parser::char::{alpha_num, char, lower, string};
use combine::*;
use expr;
use parser::utils::*;

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
    let int_ty = (lex(string("int")), parse_uint()).map(|(_, i)| expr::Type::Int(i));
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

fn parse_id<I>() -> impl Parser<Input = I, Output = expr::Id>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(
        lex(char('[')),
        lex(char(']')),
        lex((lower(), lex(many(alpha_num())), parse_type())),
    ).map(|(c, mut cs, ty): (char, String, expr::Type)| {
        cs.insert(0, c);
        expr::Id(cs, ty)
    })
}

fn parse_expr<I>() -> impl Parser<Input = I, Output = expr::Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    use expr::Expr::*;
    let var = parse_id().map(|id| Var(id));
    let int32 = with_parens((lex(string("i32")), parse_uint())).map(|(_, x)| I32(x));
    choice((try(var), try(int32)))
}
