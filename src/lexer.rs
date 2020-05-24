use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::character::complete::{alpha1, char, space0};
use nom::combinator::{map, recognize};
use nom::multi::many0;
use nom::number::complete::double;
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

// string::parse {{{
mod string {
    use super::*;
    use nom::bytes::complete::{is_not, take_while_m_n};
    use nom::character::complete::multispace1;
    use nom::combinator::{map_opt, map_res, verify};
    use nom::sequence::delimited;

    #[inline]
    fn unicode(input: &str) -> Result<char> {
        let hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());

        let delimited_hex = preceded(char('u'), delimited(char('{'), hex, char('}')));

        map_opt(
            map_res(delimited_hex, |hex| u32::from_str_radix(hex, 16)),
            std::char::from_u32,
        )(input)
    }

    #[inline]
    fn escaped_char(input: &str) -> Result<char> {
        preceded(
            char('\\'),
            alt((
                unicode,
                map_val!(char('\n'), '\n'),
                map_val!(char('\r'), '\r'),
                map_val!(char('\t'), '\t'),
                map_val!(char('b'), '\u{08}'),
                map_val!(char('f'), '\u{0C}'),
                map_val!(char('\\'), '\\'),
                map_val!(char('/'), '/'),
                map_val!(char('"'), '"'),
            )),
        )(input)
    }

    #[inline]
    fn escaped_ws(input: &str) -> Result<&str> {
        preceded(char('\\'), multispace1)(input)
    }

    #[inline]
    fn literal(input: &str) -> Result<&str> {
        let not_quote_slash = is_not("\"\\");
        verify(not_quote_slash, |s: &str| !s.is_empty())(input)
    }

    #[inline]
    fn fragment(input: &str) -> Result<()> {
        alt((
            map_val!(literal, ()),
            map_val!(escaped_char, ()),
            map_val!(escaped_ws, ()),
        ))(input)
    }

    pub fn parse(input: &str) -> Result<&str> {
        let build_string = recognize(many0(fragment));
        delimited(char('"'), build_string, char('"'))(input)
    }

    #[cfg(test)]
    mod tests {
        use super::parse;

        #[test]
        fn simple() {
            let result = parse(r#""foo""#);
            let expected = Ok(("", "foo"));
            assert_eq!(expected, result);

            let result = parse(r#""foo bar""#);
            let expected = Ok(("", "foo bar"));
            assert_eq!(expected, result);

            let result = parse("foo").is_err();
            assert!(result);
        }

        #[test]
        fn escaped() {
            let (input, s) = parse("\"foo\\\"\"").unwrap();
            assert_eq!(s, "foo\\\"");
            assert_eq!(input, "");
        }
    }
}
// }}}

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
fn literal(input: &str) -> Result<Token> {
    alt((
        map(string::parse, Token::String),
        map(double, Token::Number),
        map_val!(tag("true"), Token::Boolean(true)),
        map_val!(tag("false"), Token::Boolean(false)),
        map_val!(tag("null"), Token::Null),
    ))(input)
}

#[inline]
fn token(input: &str) -> Result<Token> {
    alt((keyword, ident, literal, punctations, operations))(input)
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

        let (input, tokens) = parse(r#"(."foo")"#).unwrap();
        let expected = vec![OpenParen, Dot, String("foo"), CloseParen];

        assert_eq!(tokens, expected);
        assert_eq!(input, "");

        let (input, tokens) = parse(".[1:-2]").unwrap();
        let expected = vec![
            Dot,
            OpenBracket,
            Number(1.0),
            Colon,
            Number(-2.0),
            CloseBracket,
        ];

        assert_eq!(tokens, expected);
        assert_eq!(input, "");
    }
}
