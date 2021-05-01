use crate::ast::{Conversion, FieldPattern, Ident, Lit, Path, Pattern, TokenEnum};
use crate::grammar::{Grammar, NonTerminal, NonTerminalIdx, Production, Symbol};

use fxhash::FxHashMap;
use proc_macro2::TokenStream;
use quote::quote;

pub struct SemanticAction {
    type_: syn::Type,
    code: syn::Expr,
}

// Terminals in the grammar are the qualified enum variants (e.g. `TokenKind::T0`)
pub fn generate_ll1_parser<A>(
    grammar: &Grammar<syn::Ident, A>,
    tokens: &TokenEnum,
    token_kind_type_name: syn::Ident,
    token_kind_type_decl: TokenStream,
    token_kind_map: FxHashMap<String, syn::Ident>,
) -> TokenStream {
    let (token_kind_fn_name, token_kind_fn_decl) =
        token_kind_fn(&tokens.type_name.0, &token_kind_type_name, tokens);

    quote!(
        #token_kind_type_decl

        #token_kind_fn_decl
    )
}

/// Generates an `enum #{token}Kind { T0, T1, ... }` type with a variant for each token described
/// in the `enum Token { ... }`.
///
/// Return values are:
///
/// - Name of the enum type for token kinds
/// - The definition of the enum
/// - A map from token names (as written by the user in `enum Token { ... }`) to the their token
///   kind enum variants
///
pub fn token_kind_type(
    tokens: &TokenEnum,
) -> (syn::Ident, TokenStream, FxHashMap<String, syn::Ident>) {
    let TokenEnum {
        type_name: Ident(type_name),
        conversions,
    } = tokens;

    let mut map: FxHashMap<String, syn::Ident> = Default::default();

    let type_name = syn::Ident::new(&(type_name.to_string() + "Kind"), type_name.span());
    let enum_alts: Vec<syn::Ident> = conversions
        .iter()
        .enumerate()
        .map(|(i, conv)| {
            let ident = syn::Ident::new(&format!("T{}", i), conv.span);
            map.insert(conv.from.clone(), ident.clone());
            ident
        })
        .collect();

    let code = quote!(enum #type_name { #(#enum_alts,)* });

    (type_name, code, map)
}

/// Generates a `fn token_kind(& #token_type) -> #token_kind_type` that returns kind of a token.
fn token_kind_fn(
    user_token_type_name: &syn::Ident,
    token_kind_type_name: &syn::Ident,
    tokens: &TokenEnum,
) -> (syn::Ident, TokenStream) {
    let TokenEnum {
        type_name: Ident(type_name),
        conversions,
    } = tokens;

    let fn_name = syn::Ident::new(&(type_name.to_string() + "_kind"), type_name.span());
    let arg_name = syn::Ident::new("token", type_name.span());

    let match_alts: Vec<TokenStream> = conversions
        .iter()
        .enumerate()
        .map(|(i, Conversion { to, span, .. })| {
            let pattern_code = pattern_ignore(to);
            let alt_name = syn::Ident::new(&format!("T{}", i), *span);
            quote!(#pattern_code => #token_kind_type_name::#alt_name)
        })
        .collect();

    let code = quote!(
        fn #fn_name(#arg_name: &#user_token_type_name) -> #token_kind_type_name {
            match #arg_name {
                #(#match_alts,)*
            }
        }
    );

    (fn_name, code)
}

/// Given a `Pattern`, generate the pattern syntax for it, using `_` for the "choose" argument.
fn pattern_ignore(pattern: &Pattern) -> TokenStream {
    match pattern {
        Pattern::Enum(Path(path), pats) => {
            let pats: Vec<TokenStream> = pats.iter().map(pattern_ignore).collect();
            quote!(#path(#(#pats,)*))
        }

        Pattern::Struct(Path(path), fields, dot) => {
            let mut pats: Vec<TokenStream> = fields
                .iter()
                .map(
                    |FieldPattern {
                         field_name,
                         pattern,
                     }| {
                        let pat = pattern_ignore(pattern);
                        quote!(#field_name: #pat,)
                    },
                )
                .collect();

            if *dot {
                pats.push(quote!(..));
            }

            quote!(#path { #(#pats)* })
        }

        Pattern::Tuple(pats) => {
            let pats: Vec<TokenStream> = pats.iter().map(pattern_ignore).collect();
            quote!((#(#pats,)*))
        }

        Pattern::Path(Path(path)) => quote!(#path),

        Pattern::Underscore => quote!(_),

        Pattern::DotDot => quote!(..),

        Pattern::Lit(Lit(lit)) => quote!(#lit),

        Pattern::Choose(_) => quote!(_),
    }
}
