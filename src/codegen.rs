use crate::ast::{Conversion, FieldPattern, Ident, Lit, Path, Pattern, TokenEnum, Type};
use crate::first::generate_first_table;
use crate::follow::generate_follow_table;
use crate::grammar::{Grammar, NonTerminal, NonTerminalIdx, Production, Symbol};
use crate::terminal::{TerminalReprArena, TerminalReprIdx};

use std::iter::FromIterator;

use fxhash::FxHashMap;
use proc_macro2::{Span, TokenStream};
use quote::quote;

pub struct SemanticAction {
    type_: syn::Type,
    code: syn::Expr,
}

// Terminals in the grammar are the qualified enum variants (e.g. `TokenKind::T0`)
pub fn generate_ll1_parser<A>(
    grammar: &Grammar<TerminalReprIdx, A>,
    tokens: &TokenEnum,
    token_kind_type_name: syn::Ident,
    token_kind_type_decl: TokenStream,
    terminal_arena: &TerminalReprArena,
) -> TokenStream {
    let (token_kind_fn_name, token_kind_fn_decl) =
        token_kind_fn(&tokens.type_name.0, &token_kind_type_name, tokens);

    let (action_result_type_name, action_result_type_decl, _) =
        action_result_type(grammar, &tokens.conversions);

    let (token_value_fn_name, token_value_fn_decl) = token_value_fn(
        &tokens.conversions,
        &tokens.type_name.0,
        &action_result_type_name,
    );

    let first_table = generate_first_table(grammar);
    let follow_table = generate_follow_table(grammar, &first_table);

    quote!(
        #token_kind_type_decl
        #token_kind_fn_decl
        #action_result_type_decl
        #token_value_fn_decl
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
pub fn token_kind_type(tokens: &TokenEnum) -> (syn::Ident, TokenStream, TerminalReprArena) {
    let TokenEnum {
        type_name: Ident(type_name),
        conversions,
    } = tokens;

    let type_name = syn::Ident::new(&(type_name.to_string() + "Kind"), type_name.span());

    let mut arena = TerminalReprArena::new(type_name.clone());

    let enum_alts: Vec<syn::Ident> = conversions
        .iter()
        .enumerate()
        .map(|(i, conv)| {
            let ident = syn::Ident::new(&format!("T{}", i), conv.span);
            arena.new_terminal(conv.from.clone(), ident.clone());
            ident
        })
        .collect();

    let code = quote!(enum #type_name { #(#enum_alts,)* });

    (type_name, code, arena)
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
        fn #fn_name(#arg_name: &#user_token_type_name) -> #token_kind_type_name {
            match #arg_name {
                #(#match_alts,)*
            }
        }
    );

    (fn_name, code)
}

/// Generates a `fn token_value(& #token_type) -> ActionResult` function that returns the value of
/// a matched token.
fn token_value_fn(
    tokens: &[Conversion],
    user_token_type_name: &syn::Ident,
    action_result_type_name: &syn::Ident,
) -> (syn::Ident, TokenStream) {
    let fn_name = syn::Ident::new("token_value", Span::call_site());

    let mut variants: Vec<TokenStream> = vec![];
    for (i, Conversion { span, from, to }) in tokens.iter().enumerate() {
        let (pattern_code, pattern_idents) = generate_pattern_syn_with_idents(to);
        let variant_id = syn::Ident::new(&format!("Token{}", i), Span::call_site());
        variants.push(quote!(
            #pattern_code => {
                // TODO: This clone needs to go
                #action_result_type_name::#variant_id((#(#pattern_idents.clone()),*))
            }
        ));
    }

    let code = quote!(
        fn #fn_name(token: & #user_token_type_name) -> #action_result_type_name {
            match token {
                #(#variants)*
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

/// Declares an `ActionResult` enum type with a variant for each non-terminal in the grammar and
/// each token with value. Used in the value stack for terminals and non-terminals.
fn action_result_type<T, A>(
    grammar: &Grammar<T, A>,
    tokens: &[Conversion],
) -> (syn::Ident, TokenStream, FxHashMap<String, syn::Ident>) {
    let action_result_type_name = syn::Ident::new("ActionResult", Span::call_site());

    let mut variants: Vec<TokenStream> = vec![];
    let mut map: FxHashMap<String, syn::Ident> = Default::default();

    // Generate variants for tokens
    // TODO: Do we need variants for tokens without values? If not, remove those. If yes, then use
    // a single variant for all value-less tokens.
    for (i, Conversion { to, .. }) in tokens.iter().enumerate() {
        let variant_id = syn::Ident::new(&format!("Token{}", i), Span::call_site());
        let pattern_types = pattern_types(to);
        variants.push(quote!(#variant_id(#(#pattern_types,)*)));
    }

    // Generate variants for non-terminals
    for NonTerminal {
        non_terminal,
        return_ty: Type(pattern_ty),
        ..
    } in grammar.non_terminals()
    {
        let variant_id =
            syn::Ident::new(&format!("NonTerminal{}", non_terminal), Span::call_site());
        variants.push(quote!(#variant_id(#pattern_ty)));
        map.insert(non_terminal.clone(), variant_id);
    }

    let code = quote!(
        enum #action_result_type_name {
            #(#variants,)*
        }
    );

    (action_result_type_name, code, map)
}

fn pattern_types(pat: &Pattern) -> Vec<&syn::Type> {
    let mut ret = vec![];
    pattern_types_(pat, &mut ret);
    ret
}

fn pattern_types_<'a, 'b>(pat: &'a Pattern, acc: &'b mut Vec<&'a syn::Type>) {
    match pat {
        Pattern::Choose(Type(ty)) => {
            acc.push(ty);
        }

        Pattern::Enum(_, pats) | Pattern::Tuple(pats) => {
            for pat in pats {
                pattern_types_(pat, acc);
            }
        }

        Pattern::Struct(_, pats, _) => {
            for FieldPattern { pattern, .. } in pats {
                pattern_types_(pattern, acc);
            }
        }

        Pattern::Path(_) | Pattern::Underscore | Pattern::DotDot | Pattern::Lit(_) => {}
    }
}

/// Given a pattern, generate the code for it and return variables generated for the `Choose` nodes
fn generate_pattern_syn_with_idents(pat: &Pattern) -> (TokenStream, Vec<syn::Ident>) {
    let mut idents = vec![];
    let code = generate_pattern_syn_with_idents_(pat, &mut idents);
    (code, idents)
}

fn generate_pattern_syn_with_idents_(pat: &Pattern, idents: &mut Vec<syn::Ident>) -> TokenStream {
    match pat {
        Pattern::Choose(_) => {
            let ident = syn::Ident::new(&format!("f{}", idents.len()), Span::call_site());
            let code = quote!(#ident);
            idents.push(ident);
            code
        }

        Pattern::Enum(Path(path), pats) => {
            let pats: Vec<TokenStream> = pats
                .iter()
                .map(|pat| generate_pattern_syn_with_idents_(pat, idents))
                .collect();

            quote!(#path(#(#pats),*))
        }

        Pattern::Struct(Path(path), fields, dots) => {
            let mut pats: Vec<TokenStream> = fields
                .iter()
                .map(
                    |FieldPattern {
                         field_name,
                         pattern,
                     }| {
                        let pattern = generate_pattern_syn_with_idents_(pattern, idents);
                        quote!(#field_name: #pattern,)
                    },
                )
                .collect();

            if *dots {
                pats.push(quote!(..));
            }

            quote!(#path { #(#pats)* })
        }

        Pattern::Tuple(pats) => {
            let pats: Vec<TokenStream> = pats
                .iter()
                .map(|pat| generate_pattern_syn_with_idents_(pat, idents))
                .collect();

            quote!((#(#pats),*))
        }

        Pattern::Path(Path(path)) => quote!(#path),

        Pattern::Underscore => quote!(_),

        Pattern::DotDot => quote!(..),

        Pattern::Lit(Lit(lit)) => quote!(#lit),
    }
}
