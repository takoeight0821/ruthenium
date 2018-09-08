mod tests;
mod utils;
use combine::parser::char::{alpha_num, char, lower, spaces, string};
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
    let int_ty = (lex(string("int")), parse_int()).map(|(_, i)| expr::Type::Int(i));
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

pub fn parse_id<I>() -> impl Parser<Input = I, Output = expr::Id>
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

fn parse_prim<I>() -> impl Parser<Input = I, Output = (String, expr::Type)>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(
        lex(char('[')),
        lex(char(']')),
        lex((char('#'), lex(many(alpha_num())), parse_type())),
    ).map(|(_, name, ty)| (name, ty))
}

pub fn parse_expr<I>() -> impl Parser<Input = I, Output = expr::Expr>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    use expr::Expr::*;
    let var = parse_id().map(|id| Var(id));
    let int32 = with_parens((lex(string("i32")), parse_int())).map(|(_, x)| I32(x));
    let int64 = with_parens((lex(string("i64")), parse_int())).map(|(_, x)| I64(x));
    let float32 = with_parens((lex(string("f32")), parse_float())).map(|(_, x)| F32(x));
    let float64 = with_parens((lex(string("f64")), parse_float())).map(|(_, x)| F64(x));
    let true_lit = with_parens((lex(string("bool")), lex(string("true")))).map(|_| Bool(true));
    let false_lit = with_parens((lex(string("bool")), lex(string("false")))).map(|_| Bool(false));
    let char_lit = with_parens((
        lex(string("char")),
        lex(between(char('\''), char('\''), satisfy_char())),
    )).map(|(_, c)| Char(c));
    let string_lit = with_parens((
        lex(string("string")),
        lex(between(char('\"'), char('\"'), many(satisfy_char()))),
    )).map(|(_, cs)| String(cs));
    let tuple = with_parens((lex(string("tuple")), many(parse_id()))).map(|(_, xs)| Tuple(xs));
    let access = with_parens((lex(string("access")), parse_id(), parse_uint()))
        .map(|(_, id, index)| Access(id, index));
    let apply = with_parens((lex(string("apply")), parse_id(), many(parse_id())))
        .map(|(_, f, args)| Apply(f, args));
    let prim = parse_prim().map(|(name, ty)| Prim(name, ty));
    let if_then_else = with_parens((lex(string("if")), parse_id(), parse_block(), parse_block()))
        .map(|(_, c, t, f)| If(c, Box::new(t), Box::new(f)));

    choice((
        try(var),
        try(int32),
        try(int64),
        try(float32),
        try(float64),
        try(true_lit),
        try(false_lit),
        try(char_lit),
        try(string_lit),
        try(tuple),
        try(access),
        try(apply),
        try(prim),
        if_then_else,
    ))
}

parser!{
    fn parse_let_[I]()(I) -> expr::Let
    where [ I: Stream<Item = char> ]
    {
        let nonrec = with_parens((lex(string("let")), parse_id(), parse_expr()))
            .map(|(_, name, val)| expr::Let::NonRec { name, val });
        let rec = with_parens((
            lex(string("letrec")),
            parse_id(),
            many(parse_id()),
            parse_block(),
        )).map(|(_, name, params, body)| expr::Let::Rec { name, params, body });
        choice((try(nonrec), rec))
    }
}

pub fn parse_let<I>() -> impl Parser<Input = I, Output = expr::Let>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    parse_let_()
}

parser!{
    fn parse_block_[I]()(I) -> expr::Block
    where [ I: Stream<Item = char> ]
    {
        lex(between(
            lex(char('{')),
            lex(char('}')),
            (many(parse_let()), lex(string("return")), parse_expr()),
        )).map(|(exprs, _, term)| expr::Block { exprs, term })
    }
}

pub fn parse_block<I>() -> impl Parser<Input = I, Output = expr::Block>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    parse_block_()
}
