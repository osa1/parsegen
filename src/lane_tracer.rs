//! Implements lane tracing

use crate::first::{FirstSet, FirstTable};
use crate::grammar::{Grammar, Symbol};
use crate::item::Item;
use crate::lane_table::{ItemIdx, LaneTable};
use crate::lr0::LR0State;
use crate::lr_common::StateIdx;
use crate::state_graph::StateGraph;

// Lookahead of a shift item is the lookahead of the reduce item we will derive from the shift
// item, after some number of shifts.

// The item will be a reduce item, as we only need lookaheads for reduce items.
pub fn lane_trace<A>(
    states: &[LR0State],
    first_table: &FirstTable,
    lane_table: &mut LaneTable,
    state_graph: &mut StateGraph,
    item_idx: ItemIdx,
    state_idx: StateIdx,
    item: &Item<()>,
    grammar: &Grammar<A>,
) {
    // let item_production = grammar.get_production(item.non_terminal_idx, item.production_idx);
    match item.next_symbol(grammar) {
        Some(_) => {
            // Shift item
            panic!() // should be a reduce item
        }

        None => {
            // Reduce item: find the shift item that generated this item. It will look like
            // `[X -> ... | Y ...]`, symbols after `Y` will potentially give us the lookahead.
            //
            // TODO: Should we cache lookaheads of visited items? If we come across an item with
            // lookaheads already computed we can use those in some cases. For example, when `Y` is
            // the last symbol above.
            let shift_symbol = item.revert().and_then(|item| item.next_symbol(grammar));
            match shift_symbol {
                None => todo!(), // Not sure when this happens
                Some(shift_symbol) => {
                    for pred_state in state_graph.predecessors(state_idx) {
                        let state_items = &states[pred_state.0].items;
                        for item in state_items {
                            if item.next_symbol(grammar) != Some(shift_symbol) {
                                continue;
                            }

                            let item_production =
                                grammar.get_production(item.non_terminal_idx, item.production_idx);

                            // Find the first set of the symbols after the current non-terminal
                            // is shifted
                            // TODO: Initial item should be checked below to add end-of-input
                            // as a lookahead
                            let first = {
                                let mut first = FirstSet::default();
                                for symbol in &item_production.symbols()[item.cursor + 1..] {
                                    match &symbol.symbol {
                                        Symbol::Terminal(t) => {
                                            first.add(*t);
                                            break;
                                        }
                                        Symbol::NonTerminal(nt) => {
                                            let nt_first = first_table.get_first(*nt);
                                            for t in nt_first.terminals() {
                                                first.add(*t);
                                            }
                                            if !nt_first.has_empty() {
                                                break;
                                            }
                                        }
                                    }
                                }
                                first
                            };

                            for terminal in first.terminals() {
                                lane_table.add_lookahead(state_idx, item_idx, *terminal);
                            }
                        }
                    }
                }
            }
        }
    }
}
