use crate::ast;
use crate::grammar::{Grammar, NonTerminalIdx, Symbol};

use fxhash::FxHashMap;

pub fn lower(parser: ast::Parser) -> Grammar<char, ()> {
    let mut grammar = Grammar::new();

    let mut nt_indices: FxHashMap<String, NonTerminalIdx> = Default::default();

    let nts: Vec<ast::NonTerminal> = parser
        .items
        .into_iter()
        .filter_map(|item| match item {
            ast::GrammarItem::TypeSynonym(_) | ast::GrammarItem::TokenEnum(_) => None,
            ast::GrammarItem::NonTerminal(nt) => Some(nt),
        })
        .collect();

    for nt in &nts {
        let nt_name = nt.name.0.to_string();
        let nt_idx = grammar.add_non_terminal(nt_name.clone());
        nt_indices.insert(nt_name, nt_idx);
    }

    for ast::NonTerminal {
        name, productions, ..
    } in nts
    {
        for prod in productions {
            let mut symbols: Vec<Symbol<char>> = vec![];
            for sym in prod.symbols {
                add_symbol(&nt_indices, &mut symbols, sym);
            }
            let nt_idx = nt_indices.get(&name.0.to_string()).unwrap();
            grammar.add_production(*nt_idx, symbols, ());
        }
    }

    grammar
}

fn add_symbol(
    nt_indices: &FxHashMap<String, NonTerminalIdx>,
    symbols: &mut Vec<Symbol<char>>,
    symbol: ast::Symbol,
) {
    match symbol {
        ast::Symbol::NonTerminal(nt) => {
            let nt_name = nt.0.to_string();
            let nt_idx = nt_indices.get(&nt_name).unwrap();
            symbols.push(Symbol::NonTerminal(*nt_idx));
        }
        ast::Symbol::Terminal(str) => {
            // For now we support chars as terminals, but I'm too lazy to refactor the
            // AST
            let str = str.0.value();
            let char = str.chars().next().unwrap();
            symbols.push(Symbol::Terminal(char));
        }
        ast::Symbol::Repeat(_) => {
            todo!("Repeat symbol not supported yet");
        }
        ast::Symbol::Name(_, sym) => add_symbol(nt_indices, symbols, *sym),
    }
}
