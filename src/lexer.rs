use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::character::complete::{alpha1, char, space0};
use nom::combinator::{map, recognize};
use nom::multi::many0;
use nom::sequence::{pair, preceded, terminated};
use nom::{AsChar, IResult};

use crate::token::Token;

pub type Result<'a, O> = IResult<&'a str, O>;

macro_rules! map_val {
    ($c:expr, $t:expr) => {
        map($c, |_| $t)
    };
}

#[inline]
fn keyword(input: &str) -> Result<Token> {
    alt((
        map_val!(tag("__loc__"), Token::__Loc__),
        map_val!(tag("as"), Token::As),
        map_val!(tag("break"), Token::Break),
        map_val!(tag("catch"), Token::Catch),
        map_val!(tag("def"), Token::Def),
        map_val!(tag("if"), Token::If),
        map_val!(tag("then"), Token::Then),
        map_val!(tag("elif"), Token::ElseIf),
        map_val!(tag("else"), Token::Else),
        map_val!(tag("end"), Token::End),
        map_val!(tag("and"), Token::And),
        map_val!(tag("or"), Token::Or),
        map_val!(tag("foreach"), Token::Foreach),
        map_val!(tag("import"), Token::Import),
        map_val!(tag("include"), Token::Include),
        map_val!(tag("label"), Token::Label),
        map_val!(tag("module"), Token::Module),
        map_val!(tag("reduce"), Token::Reduce),
        map_val!(tag("try"), Token::Try),
    ))(input)
}

#[inline]
fn ident(input: &str) -> Result<Token> {
    let first = alt((alpha1, tag("_")));
    let rest = take_while(|c: char| c.is_alphanum() || c == '_');
    map(recognize(pair(first, rest)), Token::Ident)(input)
}

#[inline]
fn operations(input: &str) -> Result<Token> {
    alt((
        map_val!(char('+'), Token::Add),
        map_val!(char('-'), Token::Sub),
        map_val!(char('*'), Token::Mul),
        map_val!(char('/'), Token::Div),
        map_val!(char('%'), Token::Mod),
        map_val!(char('='), Token::Equal),
        map_val!(tag("!="), Token::NotEqual),
        map_val!(char('>'), Token::GreaterThan),
        map_val!(char('<'), Token::LessThan),
        map_val!(tag(">="), Token::GreaterEqual),
        map_val!(tag("<="), Token::LessEqual),
        map_val!(tag("//"), Token::Alt),
        map_val!(char('='), Token::Assign),
        alt((
            map_val!(tag("|="), Token::SetPipe),
            map_val!(tag("+="), Token::SetPlus),
            map_val!(tag("-="), Token::SetMinus),
            map_val!(tag("*="), Token::SetMult),
            map_val!(tag("/="), Token::SetDiv),
            map_val!(tag("%="), Token::SetMod),
        )),
        map_val!(tag(".."), Token::Recurse),
    ))(input)
}

#[inline]
fn punctations(input: &str) -> Result<Token> {
    alt((
        map_val!(char('('), Token::OpenParen),
        map_val!(char(')'), Token::CloseParen),
        map_val!(char('{'), Token::OpenBrace),
        map_val!(char('}'), Token::CloseBrace),
        map_val!(char('['), Token::OpenBracket),
        map_val!(char(']'), Token::CloseBracket),
        map_val!(char('.'), Token::Dot),
        map_val!(char(','), Token::Comma),
        map_val!(char(':'), Token::Colon),
        map_val!(char('|'), Token::Pipe),
        map_val!(char('?'), Token::QuestionMark),
    ))(input)
}

#[inline]
fn token(input: &str) -> Result<Token> {
    alt((keyword, ident, punctations, operations))(input)
}

pub fn parse(input: &str) -> Result<Vec<Token>> {
    let p = preceded(space0, token);
    terminated(many0(p), space0)(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token::*;
    use nom::{error::ErrorKind, Err};

    #[test]
    fn test_ident() {
        let expected = Ok(("", Ident("foo")));
        assert_eq!(expected, ident("foo"));

        let expected = Ok(("", Ident("_foo")));
        assert_eq!(expected, ident("_foo"));

        let expected = Ok(("", Ident("_foo1_2")));
        assert_eq!(expected, ident("_foo1_2"));

        let expected = Err(Err::Error(("1_foo", ErrorKind::Tag)));
        assert_eq!(expected, ident("1_foo"));
    }

    #[test]
    fn lexer1() {
        let (input, tokens) = parse(r#" ( . ) "#).unwrap();
        let expected = vec![OpenParen, Dot, CloseParen];

        assert_eq!(tokens, expected);
        assert_eq!(input, "");

        let (input, tokens) = parse(r#"(.)"#).unwrap();

        assert_eq!(tokens, expected);
        assert_eq!(input, "");
    }
}
