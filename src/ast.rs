pub type Number = f64;

#[derive(Debug, Clone, PartialEq)]
pub struct Query<'a> {
    pub commas: Vec<Comma<'a>>, // `@@ ("|" @@)*`
}

#[derive(Debug, Clone, PartialEq)]
pub struct Comma<'a> {
    pub terms: Vec<Term<'a>>, // `@@ ("," @@)*`
}

#[derive(Debug, Clone, PartialEq)]
pub struct Term<'a> {
    pub kind: TermType<'a>,
    pub suffixes: Vec<Suffix>, // `@@*`
}

#[derive(Debug, Clone, PartialEq)]
pub enum TermType<'a> {
    Index(Index<'a>),
    Identity, // `.`
    Recurse,  // `..`
    // Array,
    // Object,
    Literal(Literal<'a>), // `null | bool | string`
    Query(Query<'a>),     // `( Query )`
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal<'a> {
    String(&'a str),
    Number(Number),
    Boolean(bool),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Index<'a> {
    Name(&'a str),                         // `"." ( @Ident | @String )`
    Number(Number),                        // `"[" @Number "]"`
    Slice(Option<Number>, Option<Number>), // `"[" ( @Number ( ":" @Number)? | ":" @Number ) "]"`
}

#[derive(Debug, Clone, PartialEq)]
pub enum Suffix {
    Index(SuffixIndex),
    Iter,     // `"[" "]"`
    Optional, // `"?"`
}

#[derive(Debug, Clone, PartialEq)]
pub enum SuffixIndex {
    Number(Number),                        // `"[" @Number "]"`
    Slice(Option<Number>, Option<Number>), // `"[" ( @Number ( ":" @Number)? | ":" @Number ) "]"`
}
