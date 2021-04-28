use crate::earley::{EarleyItem, EarleyItemIdx, EarleySet};
use crate::grammar::{Grammar, Symbol};

use std::fmt::Write;

use fxhash::{FxHashMap, FxHashSet};

pub fn generate_graphviz<A>(
    grammar: &Grammar<char, A>,
    states: &[EarleySet],
    item_child: &FxHashMap<EarleyItemIdx, FxHashSet<EarleyItemIdx>>,
) -> String {
    // Maps `EarleyItemIdx`s to graphviz labels
    let item_labels: FxHashMap<EarleyItemIdx, String> = {
        let mut item_labels: FxHashMap<EarleyItemIdx, String> = Default::default();
        for (set_idx, EarleySet { items }) in states.iter().enumerate() {
            for (item_idx, EarleyItem { idx, .. }) in items.iter().enumerate() {
                item_labels.insert(*idx, format!("state{}:f{}", set_idx, item_idx));
            }
        }
        item_labels
    };

    let mut dot = String::new();
    dot.push_str("digraph States {\n");
    dot.push_str("    node [shape=record, fontname=\"Jetbrains Mono Medium\"];\n");

    // Generate nodes
    for (set_idx, EarleySet { items }) in states.iter().enumerate() {
        let _ = write!(&mut dot, "    state{} [label=\"S{}", set_idx, set_idx);

        let mut items: Vec<EarleyItem> = items.iter().copied().collect();
        items.sort_by_key(|item| item.idx);

        for (
            item_idx,
            EarleyItem {
                idx: _,
                non_terminal,
                production,
                position,
                set_idx: item_set_idx,
            },
        ) in items.into_iter().enumerate()
        {
            let _ = write!(&mut dot, " | ");
            let _ = write!(
                &mut dot,
                "<f{}> [{} -&gt; ",
                item_idx,
                grammar.get_non_terminal(non_terminal).name()
            );

            let production = grammar.get_production(non_terminal, production);
            let production_symbols = production.symbols();
            for (symbol_idx, symbol) in production_symbols.iter().enumerate() {
                if symbol_idx == position as usize {
                    let _ = write!(&mut dot, "\\|");
                }
                match symbol {
                    Symbol::NonTerminal(nt_idx) => {
                        let nt = grammar.get_non_terminal(*nt_idx);
                        let _ = write!(&mut dot, "{}", nt.name());
                    }
                    Symbol::Terminal(char) => {
                        let _ = write!(&mut dot, "{:?}", char);
                    }
                }
                if symbol_idx != production_symbols.len() - 1 {
                    let _ = write!(&mut dot, " ");
                }
            }
            if position as usize == production_symbols.len() {
                let _ = write!(&mut dot, "\\|");
            }
            let _ = write!(&mut dot, ",{}]", item_set_idx);
        }

        let _ = write!(&mut dot, "\"];\n");
    }

    // Generate edges
    for EarleySet { items } in states {
        for EarleyItem { idx: item_idx, .. } in items {
            let item_label = item_labels.get(item_idx).unwrap();
            if let Some(child_items) = item_child.get(item_idx) {
                for child in child_items {
                    let _ = write!(
                        &mut dot,
                        "    {} -> {};\n",
                        item_label,
                        item_labels.get(child).unwrap()
                    );
                }
            }
        }
    }

    dot.push_str("}\n");

    dot
}
