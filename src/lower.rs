use crate::ast;
use crate::grammar::{Grammar, NonTerminalIdx, Symbol};

use fxhash::FxHashMap;

pub fn lower(
    non_terminals: Vec<ast::NonTerminal>,
    conversions: &FxHashMap<String, syn::Ident>,
) -> Grammar<syn::Ident, ()> {
    let mut grammar = Grammar::new();

    let mut nt_indices: FxHashMap<String, NonTerminalIdx> = Default::default();

    for nt in &non_terminals {
        let nt_name = nt.name.0.to_string();
        let nt_idx = grammar.add_non_terminal(nt_name.clone());
        nt_indices.insert(nt_name, nt_idx);
    }

    for ast::NonTerminal {
        name, productions, ..
    } in non_terminals
    {
        // TODO: Not sure why we need this?
        if productions.is_empty() {
            let nt_idx = nt_indices.get(&name.0.to_string()).unwrap();
            grammar.add_production(*nt_idx, vec![], ());
        }

        for prod in productions {
            let mut symbols: Vec<Symbol<syn::Ident>> = vec![];
            for sym in prod.symbols {
                add_symbol(conversions, &nt_indices, &mut symbols, sym);
            }
            let nt_idx = nt_indices.get(&name.0.to_string()).unwrap();
            grammar.add_production(*nt_idx, symbols, ());
        }
    }

    grammar
}

fn add_symbol(
    conversions: &FxHashMap<String, syn::Ident>,
    nt_indices: &FxHashMap<String, NonTerminalIdx>,
    symbols: &mut Vec<Symbol<syn::Ident>>,
    symbol: ast::Symbol,
) {
    match symbol {
        ast::Symbol::NonTerminal(nt) => {
            let nt_name = nt.0.to_string();
            let nt_idx = nt_indices.get(&nt_name).unwrap();
            symbols.push(Symbol::NonTerminal(*nt_idx));
        }
        ast::Symbol::Terminal(ast::LitStr(str)) => {
            symbols.push(Symbol::Terminal(
                conversions.get(&str.value()).unwrap().clone(),
            ));
        }
        ast::Symbol::Repeat(_) => {
            todo!("Repeat symbol not supported yet");
        }
        ast::Symbol::Name(_, sym) => add_symbol(conversions, nt_indices, symbols, *sym),
    }
}
