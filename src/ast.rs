use std::fmt;

use quote::ToTokens;
use syn::parse::{Parse, ParseBuffer};

#[derive(Clone)]
pub struct Path(pub syn::Path);

impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.to_token_stream().fmt(f)
    }
}

#[derive(Clone)]
pub struct Lit(pub syn::Lit);

impl fmt::Debug for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.to_token_stream().fmt(f)
    }
}

#[derive(Clone)]
pub struct Type(pub syn::Type);

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.to_token_stream().fmt(f)
    }
}

/// A pattern used in token definitions
#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct FieldPattern {
    pub field_name: syn::Ident,
    pub pattern: Pattern,
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
