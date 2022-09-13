use crate::ast;
use crate::collections::Map;
use crate::grammar::{BoundSymbol, Grammar, NonTerminalIdx, Symbol, TerminalIdx};

use std::iter::FromIterator;

use proc_macro2::Span;

pub fn lower(
    non_terminals: Vec<ast::NonTerminal>,
    terminals: &[ast::Conversion],
) -> Grammar<syn::Expr> {
    let mut grammar = Grammar::new();

    let mut nt_indices: Map<String, NonTerminalIdx> = Default::default();

    let t_indices = {
        let mut t_indices: Map<String, TerminalIdx> = Default::default();
        for conv in terminals {
            let idx = grammar.new_terminal(conv.from.clone());
            let old = t_indices.insert(conv.from.clone(), idx);
            if old.is_some() {
                panic!("Terminal {:?} defined multiple times", conv.from);
            }
        }
        t_indices
    };

    fn non_terminal_ty(nt: &ast::NonTerminal) -> syn::Type {
        match &nt.type_decl {
            Some(ty) => ty.clone(),
            None => syn::Type::Tuple(syn::TypeTuple {
                paren_token: syn::token::Paren {
                    span: Span::call_site(),
                },
                elems: syn::punctuated::Punctuated::new(),
            }),
        }
    }

    for nt in &non_terminals {
        if nt.visibility.is_pub() {
            // For a `pub` non-terminal:
            //
            // - Add a "1" suffix to the original non-terminal
            //
            // - Create a new non-terminal with the original name:
            //
            //       NonTerminal0: ... = { <x:NonTerminal1> => x }
            //
            //   This new terminal will be used as an entry point to the parser.
            let nt1_name = nt.name.to_string() + "1";
            let nt1_idx = grammar.add_non_terminal(nt1_name, non_terminal_ty(nt), false);
            // Original name mapped to the new name with "1" suffix
            nt_indices.insert(nt.name.to_string(), nt1_idx);

            // Create the entry point
            let nt_idx = grammar.add_non_terminal(nt.name.to_string(), non_terminal_ty(nt), true);
            // Add the production here as we already defined non-terminal on the RHS
            let binder_ident = syn::Ident::new("x", Span::call_site());
            grammar.add_production(
                nt_idx,
                vec![BoundSymbol {
                    binder: Some(ast::Name {
                        mutable: false,
                        name: binder_ident.clone(),
                    }),
                    symbol: Symbol::NonTerminal(nt1_idx),
                }],
                syn::Expr::Path(syn::ExprPath {
                    attrs: vec![],
                    qself: None,
                    path: syn::Path {
                        leading_colon: None,
                        segments: syn::punctuated::Punctuated::from_iter(vec![syn::PathSegment {
                            ident: binder_ident,
                            arguments: syn::PathArguments::None,
                        }]),
                    },
                }),
            );
        } else {
            let nt_name = nt.name.to_string();
            let nt_idx = grammar.add_non_terminal(nt_name.clone(), non_terminal_ty(nt), false);
            nt_indices.insert(nt_name, nt_idx);
        }
    }

    for ast::NonTerminal {
        name, productions, ..
    } in non_terminals
    {
        for prod in productions {
            let mut symbols: Vec<BoundSymbol> = vec![];
            for sym in prod.symbols {
                add_symbol(
                    &mut grammar,
                    &nt_indices,
                    &t_indices,
                    &mut symbols,
                    None,
                    sym,
                );
            }
            let nt_idx = nt_indices.get(&name.to_string()).unwrap();

            let action = match prod.action {
                None => syn::Expr::Tuple(syn::ExprTuple {
                    attrs: vec![],
                    paren_token: syn::token::Paren {
                        span: Span::call_site(),
                    },
                    elems: syn::punctuated::Punctuated::new(),
                }),
                Some(ast::Action::User(expr)) => expr,
                Some(ast::Action::Fallible(_)) => todo!("Fallible actions not supported yet"),
            };

            grammar.add_production(*nt_idx, symbols, action);
        }
    }

    grammar
}

fn add_symbol<A>(
    grammar: &mut Grammar<A>,
    nt_indices: &Map<String, NonTerminalIdx>,
    t_indices: &Map<String, TerminalIdx>,
    symbols: &mut Vec<BoundSymbol>,
    binder: Option<ast::Name>,
    symbol: ast::Symbol,
) {
    match symbol {
        ast::Symbol::NonTerminal(nt) => {
            let nt_name = nt.to_string();
            let nt_idx = match nt_indices.get(&nt_name) {
                None => panic!("Non-terminal not defined: {}", nt_name),
                Some(nt_idx) => nt_idx,
            };
            symbols.push(BoundSymbol {
                binder,
                symbol: Symbol::NonTerminal(*nt_idx),
            });
        }
        ast::Symbol::Terminal(str) => {
            let str = str.value();
            let idx = t_indices
                .get(&str)
                .unwrap_or_else(|| panic!("Unknown non-terminal: {:?}", str));
            symbols.push(BoundSymbol {
                binder,
                symbol: Symbol::Terminal(*idx),
            });
        }
        ast::Symbol::Repeat(_) => {
            todo!("Repeat symbol not supported yet");
        }
        ast::Symbol::Name(binder, sym) => {
            add_symbol(grammar, nt_indices, t_indices, symbols, Some(binder), *sym)
        }
    }
}
