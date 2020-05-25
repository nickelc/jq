use std::ops::RangeFrom;

use nom::branch::alt;
use nom::bytes::complete::take;
use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded, terminated};
use nom::{error::ErrorKind, Err, IResult};

use crate::ast::{Index, Literal, Number, Suffix, SuffixIndex, Term, TermType};
use crate::token::{Token, Tokens};

pub type Result<'a, O> = IResult<Tokens<'a>, O>;

fn token<'a, I>(t: Token<'a>) -> impl Fn(I) -> IResult<I, Token<'a>>
where
    I: nom::Slice<RangeFrom<usize>> + nom::InputIter<Item = &'a Token<'a>>,
{
    move |i: I| match (i).iter_elements().next().map(|tt| {
        let b = *tt == t;
        (&t, b)
    }) {
        Some((t, true)) => Ok((i.slice(1..), t.clone())),
        _ => Err(nom::Err::Error((i, nom::error::ErrorKind::Char))),
    }
}

fn ident(input: Tokens) -> Result<&str> {
    let (i, ret) = take(1usize)(input)?;
    match ret[0] {
        Token::Ident(s) => Ok((i, s)),
        _ => Err(Err::Error((i, ErrorKind::Tag))),
    }
}

fn string(input: Tokens) -> Result<&str> {
    let (i, ret) = take(1usize)(input)?;
    match ret[0] {
        Token::String(s) => Ok((i, s)),
        _ => Err(Err::Error((i, ErrorKind::Tag))),
    }
}

fn number(input: Tokens) -> Result<Number> {
    let (i, ret) = take(1usize)(input)?;
    match ret[0] {
        Token::Number(n) => Ok((i, n)),
        _ => Err(Err::Error((i, ErrorKind::Digit))),
    }
}

fn literal(input: Tokens) -> Result<Literal> {
    let (i, ret) = take(1usize)(input)?;
    match ret[0] {
        Token::Boolean(b) => Ok((i, Literal::Boolean(b))),
        Token::Number(n) => Ok((i, Literal::Number(n))),
        Token::String(s) => Ok((i, Literal::String(s))),
        Token::Null => Ok((i, Literal::Null)),
        _ => Err(Err::Error((i, ErrorKind::Tag))),
    }
}

fn range(input: Tokens) -> Result<(Option<Number>, Option<Number>)> {
    alt((
        pair(
            terminated(map(number, Some), token(Token::Colon)),
            opt(number),
        ),
        preceded(token(Token::Colon), map(number, |n| (None, Some(n)))),
    ))(input)
}

fn index(input: Tokens) -> Result<Index> {
    preceded(
        token(Token::Dot),
        alt((
            map(ident, Index::Name),
            map(string, Index::Name),
            delimited(
                token(Token::OpenBracket),
                alt((
                    map(string, Index::Name),
                    map(range, |(start, end)| Index::Slice(start, end)),
                    map(number, Index::Number),
                )),
                token(Token::CloseBracket),
            ),
        )),
    )(input)
}

fn suffix_index(input: Tokens) -> Result<SuffixIndex> {
    delimited(
        token(Token::OpenBracket),
        alt((
            map(range, |(start, end)| SuffixIndex::Slice(start, end)),
            map(number, SuffixIndex::Number),
        )),
        token(Token::CloseBracket),
    )(input)
}

fn suffix(input: Tokens) -> Result<Suffix> {
    alt((
        map(suffix_index, Suffix::Index),
        map(
            pair(token(Token::OpenBracket), token(Token::CloseBracket)),
            |_| Suffix::Iter,
        ),
        map(token(Token::QuestionMark), |_| Suffix::Optional),
    ))(input)
}

fn term(input: Tokens) -> Result<Term> {
    let kind = alt((
        map(index, TermType::Index),
        map(token(Token::Dot), |_| TermType::Identity),
        map(token(Token::Recurse), |_| TermType::Recurse),
        map(literal, TermType::Literal),
    ));
    let suffixes = many0(suffix);
    map(pair(kind, suffixes), |(kind, suffixes)| Term {
        kind,
        suffixes,
    })(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token::*;
    use nom::sequence::{delimited, tuple};
    use nom::InputLength;

    #[test]
    fn test_token() {
        let input = [Dot, OpenBracket, CloseBracket, Dot];
        let input = Tokens::new(&input);

        let mut parse = tuple((token(Dot), token(OpenBracket), token(CloseBracket)));
        let (input, result) = parse(input).unwrap();

        let expected = (Dot, OpenBracket, CloseBracket);
        assert_eq!(expected, result);
        assert_eq!(1, input.input_len());
        assert_eq!(Dot, input[0]);

        let input = [OpenParen, Dot, CloseParen];
        let input = Tokens::new(&input);

        let mut parse = delimited(token(OpenParen), token(Dot), token(CloseParen));
        let (input, result) = parse(input).unwrap();

        let expected = Dot;
        assert_eq!(result, expected);
        assert_eq!(0, input.input_len());
    }

    // test `fn index(i) -> Index` {{{
    #[test]
    fn index_name() {
        let input = [Dot, Ident("foo")];
        let input = Tokens::new(&input);

        let expected = Index::Name("foo");
        let (_, result) = index(input).unwrap();
        assert_eq!(expected, result);

        let input = [Dot, String("foo bar")];
        let input = Tokens::new(&input);

        let expected = Index::Name("foo bar");
        let (_, result) = index(input).unwrap();
        assert_eq!(expected, result);

        let input = [Dot, OpenBracket, String("foo"), CloseBracket];
        let input = Tokens::new(&input);

        let expected = Index::Name("foo");
        let (_, result) = index(input).unwrap();
        assert_eq!(expected, result);
    }

    #[test]
    fn index_number() {
        let input = [Dot, OpenBracket, Number(123.4), CloseBracket];
        let input = Tokens::new(&input);

        let expected = Index::Number(123.4);
        let (_, result) = index(input).unwrap();
        assert_eq!(expected, result);
    }

    #[test]
    fn index_slice() {
        let input = [Dot, OpenBracket, Number(123.4), Colon, CloseBracket];
        let input = Tokens::new(&input);

        let expected = Index::Slice(Some(123.4), None);
        let (_, result) = index(input).unwrap();
        assert_eq!(expected, result);

        let input = [Dot, OpenBracket, Colon, Number(123.4), CloseBracket];
        let input = Tokens::new(&input);

        let expected = Index::Slice(None, Some(123.4));
        let (_, result) = index(input).unwrap();
        assert_eq!(expected, result);

        let input = [
            Dot,
            OpenBracket,
            Number(1.2),
            Colon,
            Number(3.4),
            CloseBracket,
        ];
        let input = Tokens::new(&input);

        let expected = Index::Slice(Some(1.2), Some(3.4));
        let (_, result) = index(input).unwrap();
        assert_eq!(expected, result);

        let input = [Dot, OpenBracket, Colon, CloseBracket];
        let input = Tokens::new(&input);

        assert!(index(input).is_err());
    }
    // }}}

    // test `fn term(i) -> Term` {{{
    #[test]
    fn term_index() {
        let input = [Dot, Ident("foo")];
        let input = Tokens::new(&input);

        let expected = Term {
            kind: TermType::Index(Index::Name("foo")),
            suffixes: vec![],
        };
        let (_, result) = term(input).unwrap();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_term() {
        let input = [Dot];
        let input = Tokens::new(&input);

        let expected = Term {
            kind: TermType::Identity,
            suffixes: vec![],
        };
        let (input, result) = term(input).unwrap();
        assert_eq!(expected, result);
        assert_eq!(0, input.input_len());

        let input = [Recurse];
        let input = Tokens::new(&input);

        let expected = Term {
            kind: TermType::Recurse,
            suffixes: vec![],
        };
        let (input, result) = term(input).unwrap();
        assert_eq!(expected, result);
        assert_eq!(0, input.input_len());
    }

    #[test]
    fn term_literal() {
        let input = [Boolean(true)];
        let input = Tokens::new(&input);

        let expected = Term {
            kind: TermType::Literal(Literal::Boolean(true)),
            suffixes: vec![],
        };
        let (input, result) = term(input).unwrap();
        assert_eq!(expected, result);
        assert_eq!(0, input.input_len());

        let input = [Null];
        let input = Tokens::new(&input);

        let expected = Term {
            kind: TermType::Literal(Literal::Null),
            suffixes: vec![],
        };
        let (input, result) = term(input).unwrap();
        assert_eq!(expected, result);
        assert_eq!(0, input.input_len());
    }

    #[test]
    fn term_with_suffix() {
        let input = [Dot, Ident("foo"), OpenBracket, CloseBracket, QuestionMark];
        let input = Tokens::new(&input);

        let expected = Term {
            kind: TermType::Index(Index::Name("foo")),
            suffixes: vec![Suffix::Iter, Suffix::Optional],
        };
        let (input, result) = term(input).unwrap();
        assert_eq!(expected, result);
        assert_eq!(0, input.input_len());

        let input = [
            Dot,
            Ident("foo"),
            OpenBracket,
            CloseBracket,
            OpenBracket,
            Number(12.3),
            CloseBracket,
            QuestionMark,
        ];
        let input = Tokens::new(&input);

        let expected = Term {
            kind: TermType::Index(Index::Name("foo")),
            suffixes: vec![
                Suffix::Iter,
                Suffix::Index(SuffixIndex::Number(12.3)),
                Suffix::Optional,
            ],
        };
        let (input, result) = term(input).unwrap();
        assert_eq!(expected, result);
        assert_eq!(0, input.input_len());
    }
    // }}}
}

// vim: fdm=marker
