use crate::ast::{Conversion, FieldPattern, Ident, Lit, Name, Path, Pattern, TokenEnum, Type};
use crate::first::generate_first_table;
use crate::follow::generate_follow_table;
use crate::grammar::{Grammar, NonTerminal, Production, Symbol, SymbolKind};
use crate::parse_table::{generate_parse_table, ParseTable};
use crate::terminal::{TerminalReprArena, TerminalReprIdx};

use fxhash::FxHashMap;
use proc_macro2::{Span, TokenStream};
use quote::quote;

// Terminals in the grammar are the qualified enum variants (e.g. `TokenKind::T0`)
pub fn generate_ll1_parser(
    grammar: Grammar<TerminalReprIdx, syn::Expr>,
    tokens: &TokenEnum,
    token_kind_type_name: syn::Ident,
    token_kind_type_decl: TokenStream,
    terminal_arena: &TerminalReprArena,
) -> TokenStream {
    let (_token_kind_fn_name, token_kind_fn_decl) =
        token_kind_fn(&tokens.type_name.0, &token_kind_type_name, tokens);

    let (action_result_type_name, action_result_type_decl, non_terminal_action_variant_name) =
        action_result_type(&grammar, &tokens.conversions);

    let (_token_value_fn_name, token_value_fn_decl) = token_value_fn(
        &tokens.conversions,
        &tokens.type_name.0,
        &action_result_type_name,
    );

    let first_table = generate_first_table(&grammar);
    let follow_table = generate_follow_table(&grammar, &first_table);
    let parse_table = generate_parse_table(&grammar, &first_table, &follow_table);

    // Generate semantic action table, replace semantic actions in the grammar with their indices
    // in the table
    let (semantic_action_table, grammar) =
        generate_semantic_action_table(grammar, &non_terminal_action_variant_name);

    let production_array = generate_production_array(&grammar, terminal_arena);

    let _ = generate_parse_table_code(&grammar, &parse_table, terminal_arena);

    let parse_fn = generate_parse_fn(&tokens.type_name.0);

    quote!(
        #token_kind_type_decl
        #token_kind_fn_decl
        #action_result_type_decl
        #token_value_fn_decl
        #(#semantic_action_table)*
        #production_array

        type ParseError = (); // TODO

        enum Action {
            MatchNonTerminal(usize),
            MatchTerminal(#token_kind_type_name),
            RunSemanticAction(usize),
        }

        #parse_fn
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

/// Generates the parser
fn generate_parse_fn(token_type: &syn::Ident) -> TokenStream {
    quote!(
        fn parse(
            input: &mut dyn Iterator<Item=(usize, #token_type, usize)>
        ) -> Result<ActionResult, ParseError>
        {
            let mut action_stack: Vec<Action> = vec![Action::MatchNonTerminal(0)];
            let mut value_stack: Vec<ActionResult> = vec![];

            'next_token: for (token_index, token) in input.enumerate() {
                loop {
                    match action_stack.pop().unwrap() {
                        Action::MatchNonTerminal(nt_idx) => {
                            // let prod_idx = PARSE_TABLE[nt_idx][token_kind(token) as usize];
                            todo!()
                        }
                        Action::MatchTerminal(_) => {
                            todo!()
                        }
                        Action::RunSemanticAction(idx) => {
                            (ACTIONS[idx])(&mut value_stack);
                        }
                    }
                }
            }

            todo!()
        }
    )
}

/// Generates semantic action functions, semantic action table (array of semantic action functions)
/// and replaces semantic actions in the grammar with their indices in the array.
fn generate_semantic_action_table(
    grammar: Grammar<TerminalReprIdx, syn::Expr>,
    non_terminal_action_variant_name: &FxHashMap<String, syn::Ident>,
) -> (Vec<TokenStream>, Grammar<TerminalReprIdx, usize>) {
    let Grammar {
        init,
        non_terminals,
    } = grammar;

    // Action function declarations and the array
    let mut decls: Vec<TokenStream> = vec![];
    let mut fn_names: Vec<syn::Ident> = vec![];

    let non_terminals: Vec<NonTerminal<TerminalReprIdx, usize>> = non_terminals
        .into_iter()
        .enumerate()
        .map(
            |(
                nt_i,
                NonTerminal {
                    non_terminal,
                    productions,
                    return_ty,
                },
            )| {
                let productions: Vec<Production<TerminalReprIdx, usize>> = productions
                    .into_iter()
                    .enumerate()
                    .map(|(p_i, Production { symbols, action })| {
                        // Statements to pop the values off the value stack and bind them, for the
                        // pruduction's RHS
                        let mut pop_code: Vec<TokenStream> = vec![];

                        for Symbol { binder, kind: _ } in symbols.iter().rev() {
                            match binder {
                                None => {
                                    pop_code.push(quote!(value_stack.pop()));
                                }
                                Some(Name {
                                    mutable,
                                    name: Ident(name),
                                }) => {
                                    let mut_ = if *mutable { quote!(mut) } else { quote!() };
                                    pop_code.push(quote!(
                                        let #mut_ #name = value_stack.pop().unwrap()
                                    ));
                                }
                            }
                        }

                        let fn_name = syn::Ident::new(
                            &format!("nt{}p{}_action", nt_i, p_i),
                            Span::call_site(),
                        );
                        let fn_idx = decls.len();

                        let non_terminal_variant =
                            non_terminal_action_variant_name.get(&non_terminal).unwrap();

                        decls.push(quote!(
                            fn #fn_name(value_stack: &mut Vec<ActionResult>) {
                                #(#pop_code;)*
                                value_stack.push(ActionResult::#non_terminal_variant(#action));
                            }
                        ));

                        fn_names.push(fn_name);

                        Production {
                            symbols,
                            action: fn_idx,
                        }
                    })
                    .collect();

                NonTerminal {
                    non_terminal,
                    productions,
                    return_ty,
                }
            },
        )
        .collect();

    let n_fns = fn_names.len();
    decls.push(quote!(
        static ACTIONS: [fn(&mut Vec<ActionResult>); #n_fns] = [ #(#fn_names),* ];
    ));

    (
        decls,
        Grammar {
            init,
            non_terminals,
        },
    )
}

/// Generates `PARSE_TABLE: [[usize]]` that maps (non_terminal_idx, terminal_idx) to
/// production_idx.
fn generate_parse_table_code(
    grammar: &Grammar<TerminalReprIdx, usize>,
    parse_table: &ParseTable,
    terminals: &TerminalReprArena,
) -> TokenStream {
    let mut non_terminal_array_elems: Vec<TokenStream> = vec![];

    for ((non_terminal_idx, terminal_idx), production_idx) in &parse_table.table {
        // TODO
    }

    let n_non_terminals = non_terminal_array_elems.len();
    let n_terminals = terminals.len_terminals();
    quote!(
        static PARSE_TABLE: [[usize; #n_terminals]; #n_non_terminals] = [
            #(#non_terminal_array_elems),*
        ];
    )
}

/// Generates a `PRODUCTIONS: [[Action]]` array, indexed by production index.
///
/// TODO: Production indices are normally local to non-terminals. Perhaps refactor it?
fn generate_production_array(
    grammar: &Grammar<TerminalReprIdx, usize>,
    terminals: &TerminalReprArena,
) -> TokenStream {
    let mut action_array_names: Vec<syn::Ident> = vec![];
    let mut action_arrays: Vec<TokenStream> = vec![];

    for (p_idx, (_, _, production)) in grammar.production_indices().enumerate() {
        let array_name = syn::Ident::new(&format!("PROD_{}", p_idx), Span::call_site());
        let action_idx = production.action;
        let mut array_elems: Vec<TokenStream> =
            vec![quote!(Action::RunSemanticAction(#action_idx))];

        for symbol in &production.symbols {
            match symbol.kind {
                SymbolKind::NonTerminal(non_terminal_idx) => {
                    let idx = non_terminal_idx.as_usize();
                    array_elems.push(quote!(Action::MatchNonTerminal(#idx)));
                }
                SymbolKind::Terminal(terminal_idx) => {
                    let terminal_path = terminals.get_enum_path(terminal_idx);
                    array_elems.push(quote!(Action::MatchNonTerminal(#terminal_path)));
                }
            }
        }

        let n_actions = array_elems.len();
        action_arrays.push(quote!(
            static #array_name: [Action; #n_actions] = [#(#array_elems),*];
        ));

        action_array_names.push(array_name);
    }

    let n_elems = action_arrays.len();
    quote!(
        #(#action_arrays)*
        static PRODUCTIONS: [&'static [Action]; #n_elems] = [#(#action_array_names),*];
    )
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
