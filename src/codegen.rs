use crate::ast::{Conversion, FieldPattern, Name, Pattern, TokenEnum};
use crate::grammar::{Grammar, NonTerminal, Production, Symbol, SymbolKind};

use fxhash::FxHashMap;
use proc_macro2::{Span, TokenStream};
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

impl SemanticActionIdx {
    pub fn as_u16(&self) -> u16 {
        self.0
    }
}

/// Generates semantic action functions, semantic action table (array of semantic action functions)
/// and replaces semantic actions in the grammar with their indices in the array.
pub fn generate_semantic_action_table(
    grammar: Grammar<syn::Expr>,
    non_terminal_action_variant_name: &[usize],
    token_lifetimes: &[syn::Lifetime],
) -> (Vec<TokenStream>, Grammar<SemanticActionIdx>) {
    let Grammar {
        non_terminals,
        terminals,
    } = grammar;

    // Action function declarations and the array
    let mut decls: Vec<TokenStream> = vec![];
    let mut fn_names: Vec<syn::Ident> = vec![];

    let non_terminals: Vec<NonTerminal<SemanticActionIdx>> = non_terminals
        .into_iter()
        .enumerate()
        .map(
            |(
                nt_i,
                NonTerminal {
                    non_terminal,
                    productions,
                    return_ty,
                    public,
                },
            )| {
                let productions: Vec<Production<SemanticActionIdx>> = productions
                    .into_iter()
                    .enumerate()
                    .map(|(p_i, Production { symbols, action })| {
                        // Statements to pop the values off the value stack and bind them, for the
                        // pruduction's RHS
                        let mut pop_code: Vec<TokenStream> = vec![];

                        for Symbol { binder, kind } in symbols.iter().rev() {
                            match binder {
                                None => {
                                    pop_code.push(quote!(value_stack.pop()));
                                }
                                Some(Name {
                                    mutable,
                                    name,
                                }) => {
                                    let mut_ = if *mutable { quote!(mut) } else { quote!() };
                                    let extract_method = match kind {
                                        SymbolKind::NonTerminal(nt_idx) => syn::Ident::new(
                                            &format!("non_terminal_{}", non_terminal_action_variant_name[nt_idx.as_usize()]),
                                            Span::call_site()
                                        ),
                                        SymbolKind::Terminal(terminal_idx) => syn::Ident::new(
                                            &format!("token_{}", terminal_idx.as_usize()),
                                            Span::call_site(),
                                        ),
                                    };
                                    pop_code.push(quote!(
                                        let #mut_ #name = value_stack.pop().unwrap().#extract_method()
                                    ));
                                }
                            }
                        }

                        let fn_name = syn::Ident::new(
                            &format!("nt{}p{}_action", nt_i, p_i),
                            Span::call_site(),
                        );
                        let fn_idx = decls.len();

                        let non_terminal_variant = syn::Ident::new(
                            &format!("NonTerminal{}", non_terminal_action_variant_name[nt_i]),
                            Span::call_site()
                        );

                        decls.push(quote!(
                            fn #fn_name<#(#token_lifetimes),*>(
                                value_stack: &mut Vec<SemanticActionResult<#(#token_lifetimes),*>>
                            ) {
                                #(#pop_code;)*
                                value_stack.push(SemanticActionResult::#non_terminal_variant(#action));
                            }
                        ));

                        fn_names.push(fn_name);

                        Production {
                            symbols,
                            action: SemanticActionIdx(u16::try_from(fn_idx).unwrap()),
                        }
                    })
                    .collect();

                NonTerminal {
                    non_terminal,
                    productions,
                    return_ty,
                    public,
                }
            },
        )
        .collect();

    let n_fns = fn_names.len();
    decls.push(quote!(
        static SEMANTIC_ACTIONS: [fn(&mut Vec<SemanticActionResult>); #n_fns] = [
            #(#fn_names),*
        ];
    ));

    (
        decls,
        Grammar {
            non_terminals,
            terminals,
        },
    )
}

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

/// Generates a `fn token_value(& #token_type) -> ActionResult` function that returns the value of
/// a matched token.
pub fn token_value_fn(
    tokens: &[Conversion],
    user_token_type_name: &syn::Ident,
    user_token_type_lifetimes: &[syn::Lifetime],
    action_result_type_name: &syn::Ident,
) -> (syn::Ident, TokenStream) {
    let fn_name = syn::Ident::new("token_value", Span::call_site());

    let mut variants: Vec<TokenStream> = vec![];
    for (i, Conversion { to, .. }) in tokens.iter().enumerate() {
        let (pattern_code, pattern_idents) = generate_pattern_syn_with_idents(to);
        let variant_id = syn::Ident::new(&format!("Token{}", i), Span::call_site());
        variants.push(quote!(
            #pattern_code => {
                // TODO: This clone needs to go
                #action_result_type_name::#variant_id(#(#pattern_idents.clone()),*)
            }
        ));
    }

    let code = quote!(
        fn #fn_name<#(#user_token_type_lifetimes),*>(
            token: &#user_token_type_name<#(#user_token_type_lifetimes),*>
        ) -> #action_result_type_name<#(#user_token_type_lifetimes),*>
        {
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

/// Declares an `ActionResult` enum type with a variant for each non-terminal in the grammar and
/// each token with value. Used in the value stack for terminals and non-terminals.
///
/// The `Vec` maps `NonTerminalIdx`s to their value extraction method names.
pub fn semantic_action_result_type<A>(
    grammar: &Grammar<A>,
    tokens: &[Conversion],
    token_lifetimes: &[syn::Lifetime],
) -> (syn::Ident, TokenStream, Vec<usize>) {
    let semantic_action_result_type_name =
        syn::Ident::new("SemanticActionResult", Span::call_site());

    let mut variants: Vec<TokenStream> = vec![];

    // Inherent methods for extracting fields of variants
    let mut extraction_fns: Vec<TokenStream> = vec![];

    // Generate variants for tokens
    // TODO: Do we need variants for tokens without values? If not, remove those. If yes, then use
    // a single variant for all value-less tokens.
    for (i, Conversion { to, .. }) in tokens.iter().enumerate() {
        let variant_id = syn::Ident::new(&format!("Token{}", i), Span::call_site());
        let pattern_types = pattern_types(to);
        variants.push(quote!(#variant_id(#(#pattern_types,)*)));

        let method_id = syn::Ident::new(&format!("token_{}", i), Span::call_site());
        let field_ids: Vec<syn::Ident> = pattern_types
            .iter()
            .enumerate()
            .map(|(i, _)| syn::Ident::new(&format!("f{}", i), Span::call_site()))
            .collect();

        extraction_fns.push(quote!(
            fn #method_id(self) -> (#(#pattern_types),*) {
                match self {
                    Self::#variant_id(#(#field_ids),*) => (#(#field_ids),*),
                    _ => unreachable!(),
                }
            }
        ));
    }

    // Generate variants for non-terminals. Non-terminals with same type use the same enum variant.
    let mut non_terminal_action_variant_names: Vec<usize> = vec![];

    let mut non_terminal_ty_indices: FxHashMap<syn::Type, usize> = Default::default();
    let mut i: usize = 0;

    for non_terminal in grammar.non_terminals().iter() {
        match non_terminal_ty_indices
            .get(&non_terminal.return_ty)
            .copied()
        {
            Some(i) => {
                non_terminal_action_variant_names.push(i);
            }
            None => {
                let variant_id = syn::Ident::new(&format!("NonTerminal{}", i), Span::call_site());

                let pattern_ty = &non_terminal.return_ty;
                variants.push(quote!(#variant_id(#pattern_ty)));

                let method_id = syn::Ident::new(&format!("non_terminal_{}", i), Span::call_site());
                extraction_fns.push(quote!(
                    fn #method_id(self) -> #pattern_ty {
                        match self {
                            Self::#variant_id(f) => f,
                            _ => unreachable!("{:?}", self),
                        }
                    }
                ));

                non_terminal_action_variant_names.push(i);

                non_terminal_ty_indices.insert(non_terminal.return_ty.clone(), i);
                i += 1;
            }
        }
    }

    let code = quote!(
        #[derive(Debug)]
        enum #semantic_action_result_type_name<#(#token_lifetimes),*> {
            #(#variants,)*
        }

        impl<#(#token_lifetimes),*> #semantic_action_result_type_name<#(#token_lifetimes),*> {
            #(#extraction_fns)*
        }
    );

    (
        semantic_action_result_type_name,
        code,
        non_terminal_action_variant_names,
    )
}

fn pattern_types(pat: &Pattern) -> Vec<&syn::Type> {
    let mut ret = vec![];
    pattern_types_(pat, &mut ret);
    ret
}

fn pattern_types_<'a, 'b>(pat: &'a Pattern, acc: &'b mut Vec<&'a syn::Type>) {
    match pat {
        Pattern::Choose(ty) => {
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

        Pattern::Enum(path, pats) => {
            let pats: Vec<TokenStream> = pats
                .iter()
                .map(|pat| generate_pattern_syn_with_idents_(pat, idents))
                .collect();

            quote!(#path(#(#pats),*))
        }

        Pattern::Struct(path, fields, dots) => {
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

        Pattern::Path(path) => quote!(#path),

        Pattern::Underscore => quote!(_),

        Pattern::DotDot => quote!(..),

        Pattern::Lit(lit) => quote!(#lit),
    }
}
