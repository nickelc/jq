use std::ops::RangeFrom;

use nom::branch::alt;
use nom::bytes::complete::take;
use nom::combinator::{map, not, opt, peek};
use nom::multi::{many0, separated_list1};
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated};
use nom::{error::ErrorKind, Err, IResult};

use crate::ast::{
    Array, Comma, Index, Literal, Number, Object, ObjectKey, ObjectKeyOnly, ObjectKeyValue,
    ObjectValue, Query, Suffix, SuffixIndex, Term, TermType,
};
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

fn keyword(input: Tokens) -> Result<&str> {
    let (i, token) = take(1usize)(input)?;
    let token = match &token[0] {
        Token::__Loc__ => "__loc__",
        Token::As => "as",
        Token::Break => "break",
        Token::Catch => "catch",
        Token::Def => "def",
        Token::If => "if",
        Token::Then => "then",
        Token::Else => "else",
        Token::ElseIf => "elif",
        Token::End => "end",
        Token::And => "and",
        Token::Or => "or",
        Token::Foreach => "foreach",
        Token::Import => "import",
        Token::Include => "include",
        Token::Label => "label",
        Token::Module => "module",
        Token::Reduce => "reduce",
        Token::Try => "try",
        _ => return Err(Err::Error((i, ErrorKind::Tag))),
    };
    Ok((i, token))
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

fn array(input: Tokens) -> Result<Array> {
    delimited(
        token(Token::OpenBracket),
        map(query, |query| Array { query }),
        token(Token::CloseBracket),
    )(input)
}

fn object_key(input: Tokens) -> Result<ObjectKey> {
    alt((
        map(ident, ObjectKey::Name),
        map(string, ObjectKey::String),
        map(keyword, ObjectKey::Name),
        map(
            delimited(token(Token::OpenParen), query, token(Token::CloseParen)),
            ObjectKey::Query,
        ),
    ))(input)
}

fn object_key_only(input: Tokens) -> Result<ObjectKeyOnly> {
    alt((
        map(ident, ObjectKeyOnly::Name),
        map(string, ObjectKeyOnly::String),
    ))(input)
}

fn object_value(input: Tokens) -> Result<ObjectValue> {
    alt((
        map(preceded(token(Token::Dot), ident), ObjectValue::Name),
        map(string, ObjectValue::String),
        map(term, ObjectValue::Term),
    ))(input)
}

fn object(input: Tokens) -> Result<Object> {
    let key_value = map(
        separated_pair(object_key, token(Token::Colon), object_value),
        |(key, value)| ObjectKeyValue::KeyValue(key, value),
    );
    let key_only = map(object_key_only, ObjectKeyValue::KeyOnly);
    let elems = terminated(
        separated_list1(token(Token::Comma), alt((key_value, key_only))),
        opt(token(Token::Comma)),
    );
    delimited(
        token(Token::OpenBrace),
        map(opt(elems), |fields| Object {
            fields: fields.unwrap_or_default(),
        }),
        token(Token::CloseBrace),
    )(input)
}

fn term(input: Tokens) -> Result<Term> {
    let kind = alt((
        map(index, TermType::Index),
        map(token(Token::Dot), |_| TermType::Identity),
        map(token(Token::Recurse), |_| TermType::Recurse),
        map(literal, TermType::Literal),
        map(array, TermType::Array),
        map(object, TermType::Object),
    ));
    let suffixes = many0(suffix);
    map(pair(kind, suffixes), |(kind, suffixes)| Term {
        kind,
        suffixes,
    })(input)
}

fn query(input: Tokens) -> Result<Query> {
    let terms = terminated(
        separated_list1(token(Token::Comma), term),
        not(peek(token(Token::Comma))),
    );
    let comma = map(terms, |terms| Comma { terms });

    let commas = terminated(
        separated_list1(token(Token::Pipe), comma),
        not(peek(token(Token::Pipe))),
    );
    map(commas, |commas| Query { commas })(input)
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

    #[test]
    fn test_array() {
        use crate::ast::Comma as Fork;

        let input = [OpenBracket, Dot, Ident("a"), CloseBracket];
        let input = Tokens::new(&input);

        let expected = Array {
            query: Query {
                commas: vec![Fork {
                    terms: vec![Term {
                        kind: TermType::Index(Index::Name("a")),
                        suffixes: vec![],
                    }],
                }],
            },
        };
        let (_, result) = array(input).unwrap();
        assert_eq!(expected, result);
    }

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
    fn term_array() {
        use crate::ast::Comma as Fork;

        let input = [OpenBracket, Dot, Ident("a"), CloseBracket];
        let input = Tokens::new(&input);

        let expected = Term {
            kind: TermType::Array(Array {
                query: Query {
                    commas: vec![Fork {
                        terms: vec![Term {
                            kind: TermType::Index(Index::Name("a")),
                            suffixes: vec![],
                        }],
                    }],
                },
            }),
            suffixes: vec![],
        };
        let (_, result) = term(input).unwrap();
        assert_eq!(expected, result);
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

    // test `fn query(i) -> Query` {{{
    #[test]
    fn query_pipe_comma() {
        use crate::ast::Comma as Fork;

        // `.a , .b | .c`
        let input = [
            Dot,
            Ident("a"),
            Comma,
            Dot,
            Ident("b"),
            Pipe,
            Dot,
            Ident("c"),
        ];
        let input = Tokens::new(&input);

        let (_, result) = query(input).unwrap();
        let expected = Query {
            commas: vec![
                Fork {
                    terms: vec![
                        Term {
                            kind: TermType::Index(Index::Name("a")),
                            suffixes: vec![],
                        },
                        Term {
                            kind: TermType::Index(Index::Name("b")),
                            suffixes: vec![],
                        },
                    ],
                },
                Fork {
                    terms: vec![Term {
                        kind: TermType::Index(Index::Name("c")),
                        suffixes: vec![],
                    }],
                },
            ],
        };
        assert_eq!(expected, result);

        // `.a | .b , .c`
        let input = [
            Dot,
            Ident("a"),
            Pipe,
            Dot,
            Ident("b"),
            Comma,
            Dot,
            Ident("c"),
        ];
        let input = Tokens::new(&input);

        let (_, result) = query(input).unwrap();
        let expected = Query {
            commas: vec![
                Fork {
                    terms: vec![Term {
                        kind: TermType::Index(Index::Name("a")),
                        suffixes: vec![],
                    }],
                },
                Fork {
                    terms: vec![
                        Term {
                            kind: TermType::Index(Index::Name("b")),
                            suffixes: vec![],
                        },
                        Term {
                            kind: TermType::Index(Index::Name("c")),
                            suffixes: vec![],
                        },
                    ],
                },
            ],
        };
        assert_eq!(expected, result);
    }

    #[test]
    fn query_with_trailing_token() {
        // `.a | .b |`
        let input = [Dot, Ident("a"), Comma, Dot, Ident("b"), Pipe];
        let input = Tokens::new(&input);

        let result = query(input);

        assert!(result.is_err());
        if let Err(Err::Error((rest, _))) = result {
            assert_eq!(Pipe, rest[0]);
        }

        // `.a , | .b`
        let input = [Dot, Ident("a"), Comma, Pipe, Dot, Ident("b")];
        let input = Tokens::new(&input);

        let result = query(input);

        assert!(result.is_err());
        if let Err(Err::Error((rest, _))) = result {
            assert_eq!(Comma, rest[0]);
        }

        // `.a | .b .`
        let input = [Dot, Ident("a"), Pipe, Dot, Ident("b"), Dot];
        let input = Tokens::new(&input);

        let (rest, _) = query(input).unwrap();

        assert_eq!(Dot, rest[0]);
    }
    // }}}

    // test `fn object(i) -> Object` {{{
    #[test]
    fn object_empty() {
        let input = [OpenBrace, CloseBrace];
        let input = Tokens::new(&input);

        let (input, result) = object(input).unwrap();
        let expected = Object { fields: vec![] };

        assert_eq!(expected, result);
        assert_eq!(0, input.input_len());
    }

    #[test]
    fn object_comma_only() {
        let input = [OpenBrace, Comma, CloseBrace];
        let input = Tokens::new(&input);

        let result = object(input);
        assert!(result.is_err());
        if let Err(Err::Error((input, _))) = result {
            assert_eq!(Comma, input[0]);
        }
    }

    #[test]
    fn object_string_fields() {
        let input = [
            OpenBrace,
            String("a"),
            Colon,
            String("b"),
            Comma,
            String("c"),
            Colon,
            String("d"),
            CloseBrace,
        ];
        let input = Tokens::new(&input);

        let (input, result) = object(input).unwrap();
        let expected = Object {
            fields: vec![
                ObjectKeyValue::KeyValue(ObjectKey::String("a"), ObjectValue::String("b")),
                ObjectKeyValue::KeyValue(ObjectKey::String("c"), ObjectValue::String("d")),
            ],
        };

        assert_eq!(expected, result);
        assert_eq!(0, input.input_len());
    }

    #[test]
    fn object_trailing_comma() {
        let input = [
            OpenBrace,
            String("a"),
            Colon,
            String("b"),
            Comma,
            String("c"),
            Colon,
            String("d"),
            Comma,
            CloseBrace,
        ];
        let input = Tokens::new(&input);

        let (input, result) = object(input).unwrap();
        let expected = Object {
            fields: vec![
                ObjectKeyValue::KeyValue(ObjectKey::String("a"), ObjectValue::String("b")),
                ObjectKeyValue::KeyValue(ObjectKey::String("c"), ObjectValue::String("d")),
            ],
        };

        assert_eq!(expected, result);
        assert_eq!(0, input.input_len());
    }

    #[test]
    fn object_with_key_only() {
        let input = [
            OpenBrace,
            Ident("a"),
            Comma,
            String("b"),
            Comma,
            CloseBrace,
        ];
        let input = Tokens::new(&input);

        let (input, result) = object(input).unwrap();
        let expected = Object {
            fields: vec![
                ObjectKeyValue::KeyOnly(ObjectKeyOnly::Name("a")),
                ObjectKeyValue::KeyOnly(ObjectKeyOnly::String("b")),
            ],
        };

        assert_eq!(expected, result);
        assert_eq!(0, input.input_len());
    }

    #[test]
    fn object_complex() {
        use crate::ast::Comma as Fork;

        let input = [
            OpenBrace,
            OpenParen,
            Dot,
            OpenBracket,
            CloseBracket,
            CloseParen,
            Colon,
            Dot,
            Ident("a"),
            Comma,
            Foreach,
            Colon,
            Dot,
            Ident("b"),
            Comma,
            CloseBrace,
        ];
        let input = Tokens::new(&input);

        let (input, result) = object(input).unwrap();
        let expected = Object {
            fields: vec![
                ObjectKeyValue::KeyValue(
                    ObjectKey::Query(Query {
                        commas: vec![Fork {
                            terms: vec![Term {
                                kind: TermType::Identity,
                                suffixes: vec![Suffix::Iter],
                            }],
                        }],
                    }),
                    ObjectValue::Name("a"),
                ),
                ObjectKeyValue::KeyValue(ObjectKey::Name("foreach"), ObjectValue::Name("b")),
            ],
        };

        assert_eq!(expected, result);
        assert_eq!(0, input.input_len());
    }
    // }}}
}

// vim: fdm=marker
