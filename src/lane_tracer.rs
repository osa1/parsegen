//! Implements lane tracing

use crate::collections::Set;
use crate::first::{FirstSet, FirstTable};
use crate::grammar::{Grammar, NonTerminalIdx, ProductionIdx, Symbol, SymbolDisplay};
use crate::item::{Item, ItemDisplay};
use crate::lane_table::LaneTable;
use crate::lr0::{LR0Item, LR0State};
use crate::lr_common::StateIdx;
use crate::state_graph::StateGraph;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConflictIdx(pub u32);

pub fn lane_trace<A>(
    //
    // Constant arguments
    //
    states: &[LR0State],
    first_table: &FirstTable,
    state_graph: &mut StateGraph,
    lane_table: &mut LaneTable,
    visited: &mut Set<(StateIdx, LR0Item)>,
    grammar: &Grammar<A>,
    conflict_idx: ConflictIdx,
    //
    // Rest get updated as we recurse
    //
    state_idx: StateIdx,
    item: LR0Item,
) {
    if !visited.insert((state_idx, item)) {
        return;
    }

    println!(
        "Lane trace conflict_idx = {}, state = {}, item = {}",
        conflict_idx.0,
        state_idx.0,
        ItemDisplay {
            item: &item,
            grammar
        }
    );

    fn is_initial_item(item: &LR0Item) -> bool {
        item.non_terminal_idx == NonTerminalIdx(1) && item.production_idx == ProductionIdx(0)
    }

    if is_initial_item(&item) {
        lane_table.add_lookahead(state_idx, conflict_idx, None);
        return;
    }

    if item.cursor > 0 {
        let unshifted_item = item.unshift();
        let shifted_symbol = unshifted_item.next_symbol(grammar).unwrap();
        let predecessors: Vec<StateIdx> = state_graph.predecessors(state_idx).copied().collect();
        for predecessor in predecessors {
            let predecessor_state = &states[predecessor.as_usize()];
            if predecessor_state.goto.get(shifted_symbol) != Some(&state_idx) {
                continue;
            }
            lane_trace(
                states,
                first_table,
                state_graph,
                lane_table,
                visited,
                grammar,
                conflict_idx,
                predecessor,
                unshifted_item,
            );
        }
        return;
    }

    // Cursor is at the beginning of the item: item was an expansion of a non-terminal from another
    // item in the same set.
    for pred_item in states[state_idx.as_usize()]
        .items
        .iter()
        .filter(|pred_item| {
            pred_item.next_symbol(grammar) == Some(&Symbol::NonTerminal(item.non_terminal_idx))
        })
    {
        println!(
            "  Checking item {}",
            ItemDisplay {
                item: pred_item,
                grammar
            }
        );

        // Find the first set after the non-terminal shifted. If the rest of the production can
        // generate empty string, then the item's lookahead is added to the conflict item's
        // lookahead, so we recursive to find the item's lookahead.
        let first = {
            let production = pred_item.get_production(grammar);
            let mut first: FirstSet = Default::default();
            let mut can_generate_empty = true;
            for symbol in &production.symbols()[pred_item.cursor + 1..] {
                println!("symbol = {}", SymbolDisplay::new(&symbol.symbol, grammar),);
                match &symbol.symbol {
                    Symbol::Terminal(t) => {
                        can_generate_empty = false;
                        first.add(*t);
                        break;
                    }
                    Symbol::NonTerminal(nt) => {
                        let nt_first = first_table.get_first(*nt);
                        for t in nt_first.terminals() {
                            first.add(*t);
                        }
                        if !nt_first.has_empty() {
                            can_generate_empty = false;
                            break;
                        }
                    }
                }
            }
            for t in first.terminals() {
                println!("  Lookahead found: {}", grammar.get_terminal(*t));
                lane_table.add_lookahead(state_idx, conflict_idx, Some(*t));
            }
            if can_generate_empty {
                lane_trace(
                    states,
                    first_table,
                    state_graph,
                    lane_table,
                    visited,
                    grammar,
                    conflict_idx,
                    state_idx,
                    *pred_item,
                );
            }
        };
    }
}
