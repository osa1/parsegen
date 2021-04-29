use crate::earley::{EarleyItem, EarleyItemIdx, EarleySet};
use crate::grammar::{Grammar, Symbol};
use crate::simulate::EdgeKind;

use std::fmt::Write;

use fxhash::FxHashMap;

pub fn generate_graphviz<A>(
    grammar: &Grammar<char, A>,
    states: &[EarleySet],
    item_child: &FxHashMap<EarleyItemIdx, Vec<(EarleyItemIdx, EdgeKind)>>,
) -> String {
    // Maps `EarleyItemIdx`s to graphviz labels
    let item_labels: FxHashMap<EarleyItemIdx, String> = {
        let mut item_labels: FxHashMap<EarleyItemIdx, String> = Default::default();
        for (set_idx, EarleySet { items }) in states.iter().enumerate() {
            let mut items: Vec<EarleyItem> = items.iter().copied().collect();
            items.sort_by_key(|item| item.idx);

            for (item_idx, EarleyItem { idx, .. }) in items.iter().enumerate() {
                item_labels.insert(*idx, format!("set{}_item{}", set_idx, item_idx));
            }
        }
        item_labels
    };

    let mut dot = String::new();
    dot.push_str("digraph States {\n");
    dot.push_str("    node [shape=record, fontname=\"Jetbrains Mono Medium\"];\n");

    // Generate nodes
    for (set_idx, EarleySet { items }) in states.iter().enumerate() {
        // let _ = write!(&mut dot, "    state{} [label=\"S{}", set_idx, set_idx);

        let _ = write!(&mut dot, "    subgraph clusterS{} {{\n", set_idx);
        let _ = write!(&mut dot, "        label=\"S{}\";\n", set_idx);

        let mut items: Vec<EarleyItem> = items.iter().copied().collect();
        items.sort_by_key(|item| item.idx);

        for (
            item_idx,
            EarleyItem {
                idx,
                non_terminal,
                production,
                position,
                set_idx: item_set_idx,
            },
        ) in items.iter().copied().enumerate()
        {
            // let _ = write!(&mut dot, " | ");
            // let _ = write!(
            //     &mut dot,
            //     "<f{}> [{} -&gt; ",
            //     item_idx,
            //     grammar.get_non_terminal(non_terminal).name()
            // );

            let _ = write!(
                &mut dot,
                "        set{}_item{} [label=\"({}) [{} -&gt; ",
                set_idx,
                item_idx,
                idx.0,
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
            // let _ = write!(&mut dot, ",{}]", item_set_idx);
            let _ = write!(&mut dot, ",{}]\"];\n", item_set_idx);
        }

        // let _ = write!(&mut dot, "\"];\n");

        let _ = write!(&mut dot, "    }}\n");
    }

    // Generate edges
    for EarleySet { items } in states {
        for EarleyItem { idx: item_idx, .. } in items {
            let item_label = item_labels.get(item_idx).unwrap();
            if let Some(child_items) = item_child.get(item_idx) {
                for (child, kind) in child_items {
                    match kind {
                        EdgeKind::Predict => {
                            let _ = write!(
                                &mut dot,
                                "    {} -> {} [color=grey];\n",
                                item_label,
                                item_labels.get(child).unwrap(),
                            );
                        }
                        EdgeKind::Complete1 => {
                            let _ = write!(
                                &mut dot,
                                "    {} -> {} [color=red];\n",
                                item_label,
                                item_labels.get(child).unwrap(),
                            );
                        }
                        EdgeKind::Complete2 => {
                            let _ = write!(
                                &mut dot,
                                "    {} -> {} [color=blue];\n",
                                item_label,
                                item_labels.get(child).unwrap(),
                            );
                        }
                        EdgeKind::Scan(char) => {
                            let _ = write!(
                                &mut dot,
                                "    {} -> {} [label=\"{:?}\"];\n",
                                item_label,
                                item_labels.get(child).unwrap(),
                                char,
                            );
                        }
                    }
                }
            }
        }
    }

    dot.push_str("}\n");

    dot
}
