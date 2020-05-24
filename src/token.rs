#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Ident(&'a str),
    String(&'a str),
    Number(f64),
    Boolean(bool),
    Null,
    // Keywords
    __Loc__,
    As,
    Break,
    Catch,
    Def,
    If,
    Then,
    Else,
    ElseIf,
    End,
    And,
    Or,
    Foreach,
    Import,
    Include,
    Label,
    Module,
    Reduce,
    Try,
    // Operators
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterEqual,
    LessEqual,
    Alt,
    Assign,
    SetPipe,
    SetPlus,
    SetMinus,
    SetMult,
    SetDiv,
    SetMod,
    Recurse,
    // Punctations
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    Dot,
    Comma,
    Colon,
    Pipe,
    QuestionMark,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tokens<'a> {
    tokens: &'a [Token<'a>],
    start: usize,
    end: usize,
}

use std::ops::Index;

impl<'a> Index<usize> for Tokens<'a> {
    type Output = Token<'a>;

    fn index(&self, idx: usize) -> &Self::Output {
        &self.tokens[idx]
    }
}

use nom::InputLength;
use nom::InputTake;

impl<'a> InputLength for Token<'a> {
    fn input_len(&self) -> usize {
        1
    }
}

impl<'a> InputLength for Tokens<'a> {
    fn input_len(&self) -> usize {
        self.tokens.len()
    }
}

impl<'a> InputTake for Tokens<'a> {
    fn take(&self, count: usize) -> Self {
        Tokens {
            tokens: &self.tokens[0..count],
            start: 0,
            end: count,
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.tokens.split_at(count);
        let prefix = Tokens {
            tokens: prefix,
            start: 0,
            end: prefix.len(),
        };
        let suffix = Tokens {
            tokens: suffix,
            start: 0,
            end: suffix.len(),
        };
        (suffix, prefix)
    }
}

use nom::InputIter;
use std::iter::Enumerate;
use std::slice::Iter;

impl<'a> InputIter for Tokens<'a> {
    type Item = &'a Token<'a>;
    type Iter = Enumerate<Iter<'a, Token<'a>>>;
    type IterElem = Iter<'a, Token<'a>>;

    fn iter_indices(&self) -> Self::Iter {
        self.tokens.iter().enumerate()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.tokens.iter()
    }

    fn position<P>(&self, pred: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tokens.iter().position(pred)
    }

    fn slice_index(&self, count: usize) -> Option<usize> {
        if self.tokens.len() >= count {
            Some(count)
        } else {
            None
        }
    }
}

use nom::Slice;
use std::ops::{Range, RangeFrom, RangeFull, RangeTo};

impl<'a> Slice<Range<usize>> for Tokens<'a> {
    fn slice(&self, range: Range<usize>) -> Self {
        let start = self.start + range.start;
        let end = self.start + range.end;
        let slice = &self.tokens[range];
        Tokens {
            tokens: slice,
            start,
            end,
        }
    }
}

impl<'a> Slice<RangeTo<usize>> for Tokens<'a> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice(0..range.end)
    }
}

impl<'a> Slice<RangeFrom<usize>> for Tokens<'a> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..self.end - self.start)
    }
}

impl<'a> Slice<RangeFull> for Tokens<'a> {
    fn slice(&self, _: RangeFull) -> Self {
        Tokens {
            tokens: &self.tokens,
            start: self.start,
            end: self.end,
        }
    }
}
