use std::fmt;

use quote::ToTokens;
use syn::parse::{Parse, ParseBuffer};

////////////////////////////////////////////////////////////////////////////////

pub struct Path(pub syn::Path);

pub struct Lit(pub syn::Lit);

pub struct Type(pub syn::Type);

pub struct Ident(pub syn::Ident);

pub struct Expr(pub syn::Expr);

pub struct LitStr(pub syn::LitStr);

impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.to_token_stream().fmt(f)
    }
}

impl fmt::Debug for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.to_token_stream().fmt(f)
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.to_token_stream().fmt(f)
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.to_string().fmt(f)
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        "<expr>".fmt(f)
    }
}

impl fmt::Debug for LitStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.value().fmt(f)
    }
}

impl Parse for Path {
    fn parse(input: &ParseBuffer) -> syn::Result<Self> {
        Ok(Path(input.parse::<syn::Path>()?))
    }
}

impl Parse for Lit {
    fn parse(input: &ParseBuffer) -> syn::Result<Self> {
        Ok(Lit(input.parse::<syn::Lit>()?))
    }
}

impl Parse for Type {
    fn parse(input: &ParseBuffer) -> syn::Result<Self> {
        Ok(Type(input.parse::<syn::Type>()?))
    }
}

impl Parse for Ident {
    fn parse(input: &ParseBuffer) -> syn::Result<Self> {
        Ok(Ident(input.parse::<syn::Ident>()?))
    }
}

impl Parse for Expr {
    fn parse(input: &ParseBuffer) -> syn::Result<Self> {
        Ok(Expr(input.parse::<syn::Expr>()?))
    }
}

impl Parse for LitStr {
    fn parse(input: &ParseBuffer) -> syn::Result<Self> {
        Ok(LitStr(input.parse::<syn::LitStr>()?))
    }
}

////////////////////////////////////////////////////////////////////////////////

/// The `enum Token { ... }` declaration
#[derive(Debug)]
pub struct EnumToken {
    pub type_name: Path,
    pub conversions: Vec<Conversion>,
}

#[derive(Debug)]
pub struct Conversion {
    pub from: String,
    pub to: Pattern,
}

#[derive(Debug)]
pub struct TypeSynonym {
    pub name: Ident,
    pub ty: Type,
}

/// A pattern used in token definitions
#[derive(Debug)]
pub enum Pattern {
    // `X::Y(<pat1>,...,<patN>)`
    Enum(Path, Vec<Pattern>),
    // `X::Y { f1: <pat1>, ..., fN: <patN>, .. }`. The `..` part is optional (the `bool` field).
    Struct(Path, Vec<FieldPattern>, bool),
    // `(<pat1>, ..., <patN>)`
    Tuple(Vec<Pattern>),
    Path(Path),
    Underscore,
    DotDot,
    Lit(Lit),
    Choose(Type),
}

#[derive(Debug)]
pub struct FieldPattern {
    pub field_name: syn::Ident,
    pub pattern: Pattern,
}

#[derive(Debug)]
pub enum Visibility {
    Pub,
    Priv,
}

#[derive(Debug)]
pub struct NonTerminal {
    pub visibility: Visibility,
    pub name: Ident,
    // pub macro_args: Vec<Ident>,
    pub type_decl: Option<Type>,
    pub alternatives: Vec<Alternative>,
}

#[derive(Debug)]
pub struct Alternative {
    pub symbols: Vec<Symbol>,
    pub action: Action,
}

#[derive(Debug)]
pub enum Symbol {
    Terminal(LitStr),
    // TODO: more symbols here
}

#[derive(Debug)]
pub enum Action {
    User(Expr),
    Fallible(Expr),
}

impl Parse for FieldPattern {
    fn parse(input: &ParseBuffer) -> syn::Result<Self> {
        let field_name = input.parse::<syn::Ident>()?;
        input.parse::<syn::token::Colon>()?;
        let pattern = input.parse::<Pattern>()?;
        Ok(FieldPattern {
            field_name,
            pattern,
        })
    }
}

impl Parse for Pattern {
    fn parse(input: &ParseBuffer) -> syn::Result<Self> {
        if input.peek(syn::token::Dot2) {
            input.parse::<syn::token::Dot2>()?;
            Ok(Pattern::DotDot)
        } else if input.peek(syn::Lit) {
            Ok(Pattern::Lit(Lit(input.parse::<syn::Lit>()?)))
        } else if input.peek(syn::token::Underscore) {
            input.parse::<syn::token::Underscore>()?;
            Ok(Pattern::Underscore)
        } else if input.peek(syn::token::Paren) {
            let contents;
            syn::parenthesized!(contents in input);
            let pats: syn::punctuated::Punctuated<Pattern, syn::token::Comma> =
                syn::punctuated::Punctuated::parse_terminated(&contents)?;
            let mut fields: Vec<Pattern> = vec![];
            for pair in pats.into_pairs() {
                fields.push(pair.into_value());
            }
            Ok(Pattern::Tuple(fields))
        } else if input.peek(syn::Ident) {
            let path = input.parse::<syn::ExprPath>()?;
            // enum, struct, or just path
            if input.peek(syn::token::Paren) {
                let contents;
                syn::parenthesized!(contents in input);
                let pats: syn::punctuated::Punctuated<Pattern, syn::token::Comma> =
                    syn::punctuated::Punctuated::parse_terminated(&contents)?;
                let mut fields: Vec<Pattern> = vec![];
                for pair in pats.into_pairs() {
                    fields.push(pair.into_value());
                }
                Ok(Pattern::Enum(Path(path.path), fields))
            } else if input.peek(syn::token::Brace) {
                let contents;
                syn::braced!(contents in input);

                let mut field_patterns: Vec<FieldPattern> = vec![];
                let mut trailing_dots = false;
                while !contents.is_empty() {
                    if contents.peek(syn::token::Dot2) {
                        contents.parse::<syn::token::Dot2>()?;
                        trailing_dots = true;
                        // TODO: turn this into a helpful error
                        assert!(contents.is_empty());
                        break;
                    } else {
                        field_patterns.push(FieldPattern::parse(&contents)?);
                        if contents.peek(syn::token::Comma) {
                            contents.parse::<syn::token::Comma>()?;
                        } else {
                            // TODO: turn this into a helpful error
                            assert!(contents.is_empty() || contents.peek(syn::token::Dot2));
                        }
                    }
                }
                Ok(Pattern::Struct(
                    Path(path.path),
                    field_patterns,
                    trailing_dots,
                ))
            } else {
                Ok(Pattern::Path(Path(path.path)))
            }
        } else if input.peek(syn::token::Lt) {
            input.parse::<syn::token::Lt>()?;
            let ty = input.parse::<syn::Type>()?;
            input.parse::<syn::token::Gt>()?;
            Ok(Pattern::Choose(Type(ty)))
        } else {
            // TODO: error message
            panic!()
        }
    }
}

impl Parse for Conversion {
    fn parse(input: &ParseBuffer) -> syn::Result<Self> {
        let from = input.parse::<syn::LitStr>()?;
        input.parse::<syn::token::FatArrow>()?;
        let to = input.parse::<Pattern>()?;
        Ok(Conversion {
            from: from.value(),
            to,
        })
    }
}

impl Parse for EnumToken {
    fn parse(input: &ParseBuffer) -> syn::Result<Self> {
        input.parse::<syn::token::Enum>()?;
        let type_name = input.parse::<Path>()?;
        let contents;
        syn::braced!(contents in input);
        let conversions: syn::punctuated::Punctuated<Conversion, syn::token::Comma> =
            syn::punctuated::Punctuated::parse_terminated(&contents)?;

        Ok(EnumToken {
            type_name,
            conversions: conversions
                .into_pairs()
                .map(|pair| pair.into_value())
                .collect(),
        })
    }
}

impl Parse for TypeSynonym {
    fn parse(input: &ParseBuffer) -> syn::Result<Self> {
        input.parse::<syn::token::Type>()?;
        let name = input.parse::<Ident>()?;
        input.parse::<syn::token::Eq>()?;
        let ty = input.parse::<Type>()?;
        input.parse::<syn::token::Semi>()?;
        Ok(TypeSynonym { name, ty })
    }
}

impl Parse for Visibility {
    fn parse(input: &ParseBuffer) -> syn::Result<Self> {
        if input.peek(syn::token::Pub) {
            input.parse::<syn::token::Pub>()?;
            Ok(Visibility::Pub)
        } else {
            Ok(Visibility::Priv)
        }
    }
}

impl Parse for NonTerminal {
    fn parse(input: &ParseBuffer) -> syn::Result<Self> {
        let visibility = input.parse::<Visibility>()?;
        let name = input.parse::<Ident>()?;
        let type_decl: Option<Type> = {
            if input.peek(syn::token::Colon) {
                input.parse::<syn::token::Colon>()?;
                Some(input.parse::<Type>()?)
            } else {
                None
            }
        };
        input.parse::<syn::token::Eq>()?;
        let mut alternatives: Vec<Alternative> = vec![];
        if input.peek(syn::token::Brace) {
            let contents;
            syn::braced!(contents in input);
            while !contents.is_empty() {
                alternatives.push(contents.parse::<Alternative>()?);
                let _ = contents.parse::<syn::token::Comma>();
            }
            input.parse::<syn::token::Semi>()?;
        } else {
            alternatives.push(input.parse::<Alternative>()?);
            input.parse::<syn::token::Comma>()?;
        }
        Ok(NonTerminal {
            visibility,
            name,
            type_decl,
            alternatives,
        })
    }
}

impl Parse for Alternative {
    fn parse(input: &ParseBuffer) -> syn::Result<Self> {
        let mut symbols: Vec<Symbol> = vec![];
        while !input.peek(syn::token::FatArrow) {
            symbols.push(input.parse::<Symbol>()?);
        }
        let action = input.parse::<Action>()?;
        Ok(Alternative { symbols, action })
    }
}

impl Parse for Symbol {
    fn parse(input: &ParseBuffer) -> syn::Result<Self> {
        Ok(Symbol::Terminal(input.parse::<LitStr>()?))
    }
}

impl Parse for Action {
    fn parse(input: &ParseBuffer) -> syn::Result<Self> {
        input.parse::<syn::token::FatArrow>()?;
        if input.peek(syn::token::Question) {
            input.parse::<syn::token::Question>()?;
            Ok(Action::Fallible(input.parse::<Expr>()?))
        } else {
            Ok(Action::User(input.parse::<Expr>()?))
        }
    }
}

#[test]
fn parse_pattern() {
    assert!(matches!(
        syn::parse_str::<Pattern>("X(A, B)").unwrap(),
        Pattern::Enum(_, _)
    ));
    assert!(matches!(
        syn::parse_str::<Pattern>("X::Y(A, B)").unwrap(),
        Pattern::Enum(_, _)
    ));
    assert!(matches!(
        syn::parse_str::<Pattern>("X::Y { a: A, b: B, .. }").unwrap(),
        Pattern::Struct(_, _, true)
    ));
    assert!(matches!(
        syn::parse_str::<Pattern>("X::Y { a: A, b: B }").unwrap(),
        Pattern::Struct(_, _, false)
    ));
    assert!(matches!(
        syn::parse_str::<Pattern>("(A, B)").unwrap(),
        Pattern::Tuple(_)
    ));
    assert!(matches!(
        syn::parse_str::<Pattern>("X::Y").unwrap(),
        Pattern::Path(_)
    ));
    assert!(matches!(
        syn::parse_str::<Pattern>("X").unwrap(),
        Pattern::Path(_)
    ));
    assert!(matches!(
        syn::parse_str::<Pattern>("_").unwrap(),
        Pattern::Underscore
    ));
    assert!(matches!(
        syn::parse_str::<Pattern>("..").unwrap(),
        Pattern::DotDot
    ));
    assert!(matches!(
        syn::parse_str::<Pattern>("123").unwrap(),
        Pattern::Lit(_)
    ));
    assert!(matches!(
        syn::parse_str::<Pattern>("'a'").unwrap(),
        Pattern::Lit(_)
    ));
    assert!(matches!(
        syn::parse_str::<Pattern>("\"a\"").unwrap(),
        Pattern::Lit(_)
    ));
    assert!(matches!(
        syn::parse_str::<Pattern>("<A>").unwrap(),
        Pattern::Choose(_)
    ));
}

#[test]
fn parse_enum_token() {
    let EnumToken { conversions, .. } = syn::parse_str::<EnumToken>(
        r#"
            enum Token {
            }
        "#,
    )
    .unwrap();
    assert_eq!(conversions.len(), 0);

    let EnumToken { conversions, .. } = syn::parse_str::<EnumToken>(
        r#"
            enum Token {
                "id" => Token::Id(<&'input str>),
            }
        "#,
    )
    .unwrap();
    assert_eq!(conversions.len(), 1);
}

#[test]
fn parse_type_synonym() {
    syn::parse_str::<TypeSynonym>("type X = Y;").unwrap();
}

#[test]
fn parse_nonterminal() {
    syn::parse_str::<NonTerminal>("pub Test: A = { };").unwrap();
    syn::parse_str::<NonTerminal>(r#"Test = "a" "b" => (),"#).unwrap();
    let non_terminal = syn::parse_str::<NonTerminal>(
        r#"
            Test = {
                "a" "b" => {
                    1
                },
                "c" => {
                    2
                },
            };
        "#,
    )
    .unwrap();
    assert_eq!(non_terminal.alternatives.len(), 2);
}
