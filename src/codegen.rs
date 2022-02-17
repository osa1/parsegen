use crate::ast::{Conversion, FieldPattern, Pattern, TokenEnum};

use proc_macro2::TokenStream;
use quote::quote;

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
pub fn generate_token_kind_type(tokens: &TokenEnum) -> (syn::Ident, TokenStream) {
    let TokenEnum {
        type_name,
        type_lifetimes: _,
        conversions,
    } = tokens;

    let token_kind_name = syn::Ident::new(&(type_name.to_string() + "Kind"), type_name.span());

    let enum_alts: Vec<syn::Ident> = conversions
        .iter()
        .enumerate()
        .map(|(i, conv)| syn::Ident::new(&format!("T{}", i), conv.span))
        .collect();

    let code = quote!(
        #[derive(Clone, Copy, PartialEq, Eq, Debug)]
        enum #token_kind_name {
            #(#enum_alts,)*
        }
    );

    (token_kind_name, code)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SemanticActionIdx(u16);

/// Generates a `fn token_kind(& #token_type) -> #token_kind_type` that returns kind of a token.
pub fn token_kind_fn(
    token_kind_type_name: &syn::Ident,
    tokens: &TokenEnum,
) -> (syn::Ident, TokenStream) {
    let TokenEnum {
        type_name,
        type_lifetimes,
        conversions,
    } = tokens;

    let fn_name = syn::Ident::new(
        &(type_name.to_string().to_lowercase() + "_kind"),
        type_name.span(),
    );
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
        fn #fn_name<#(#type_lifetimes),*>(
            #arg_name: &#type_name<#(#type_lifetimes),*>
        ) -> #token_kind_type_name
        {
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
        Pattern::Enum(path, pats) => {
            let pats: Vec<TokenStream> = pats.iter().map(pattern_ignore).collect();
            quote!(#path(#(#pats,)*))
        }

        Pattern::Struct(path, fields, dot) => {
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

        Pattern::Path(path) => quote!(#path),

        Pattern::Underscore => quote!(_),

        Pattern::DotDot => quote!(..),

        Pattern::Lit(lit) => quote!(#lit),

        Pattern::Choose(_) => quote!(_),
    }
}
