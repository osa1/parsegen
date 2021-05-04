use crate::ast;
use crate::grammar::{Grammar, NonTerminalIdx, Symbol, SymbolKind};
use crate::terminal::{TerminalReprArena, TerminalReprIdx};

use fxhash::FxHashMap;

pub fn lower(
    non_terminals: Vec<ast::NonTerminal>,
    arena: &TerminalReprArena,
) -> Grammar<TerminalReprIdx, syn::Expr> {
    let mut grammar = Grammar::new();

    let mut nt_indices: FxHashMap<String, NonTerminalIdx> = Default::default();

    for nt in &non_terminals {
        let nt_name = nt.name.to_string();
        let nt_idx = grammar.add_non_terminal(nt_name.clone(), nt.type_decl.clone());
        nt_indices.insert(nt_name, nt_idx);
    }

    for ast::NonTerminal {
        name, productions, ..
    } in non_terminals
    {
        // TODO: Not sure why we need this?
        if productions.is_empty() {
            // let nt_idx = nt_indices.get(&name.0.to_string()).unwrap();
            // grammar.add_production(*nt_idx, vec![], ());
            todo!("Empty productions not supported yet");
        }

        for prod in productions {
            let mut symbols: Vec<Symbol<TerminalReprIdx>> = vec![];
            for sym in prod.symbols {
                add_symbol(arena, &nt_indices, &mut symbols, None, sym);
            }
            let nt_idx = nt_indices.get(&name.to_string()).unwrap();

            let action = match prod.action {
                ast::Action::User(expr) => expr,
                ast::Action::Fallible(_) => todo!("Fallible actions not supported yet"),
            };

            grammar.add_production(*nt_idx, symbols, action);
        }
    }

    grammar
}

fn add_symbol(
    arena: &TerminalReprArena,
    nt_indices: &FxHashMap<String, NonTerminalIdx>,
    symbols: &mut Vec<Symbol<TerminalReprIdx>>,
    binder: Option<ast::Name>,
    symbol: ast::Symbol,
) {
    match symbol {
        ast::Symbol::NonTerminal(nt) => {
            let nt_name = nt.to_string();
            // TODO: This line fails when a non-terminal is used before defined, during development
            let nt_idx = nt_indices.get(&nt_name).unwrap();
            symbols.push(Symbol {
                binder,
                kind: SymbolKind::NonTerminal(*nt_idx),
            });
        }
        ast::Symbol::Terminal(str) => {
            symbols.push(Symbol {
                binder,
                kind: SymbolKind::Terminal(arena.get_name_idx(&str.value())),
            });
        }
        ast::Symbol::Repeat(_) => {
            todo!("Repeat symbol not supported yet");
        }
        ast::Symbol::Name(binder, sym) => {
            add_symbol(arena, nt_indices, symbols, Some(binder), *sym)
        }
    }
}
