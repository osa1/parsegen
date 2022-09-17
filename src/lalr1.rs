use crate::collections::{Map, Set};
use crate::grammar::{Grammar, Symbol, SymbolDisplay, TerminalIdx};
use crate::item::{Item, ItemDisplay};
use crate::lr0::LR0State;
use crate::lr_common::StateIdx;
use crate::state::LRState;

use std::fmt;

pub type LALR1Item = Item<Set<Option<TerminalIdx>>>;

pub struct LALR1State {
    pub items: Vec<LALR1Item>,
    pub goto: Map<Symbol, StateIdx>,
}

pub fn lr0_states_to_lalr1(states: &[LR0State]) -> Vec<LALR1State> {
    states
        .iter()
        .map(|LRState { items, goto }| LALR1State {
            items: items
                .iter()
                .map(|item| Item {
                    non_terminal_idx: item.non_terminal_idx,
                    production_idx: item.production_idx,
                    cursor: item.cursor,
                    lookahead: Default::default(),
                })
                .collect(),
            goto: goto.clone(),
        })
        .collect()
}

pub fn find_conflicts<A>(state: &LALR1State, grammar: &Grammar<A>) -> Set<usize> {
    let all_terminals: Set<Option<TerminalIdx>> =
        grammar.terminal_indices().map(Some).chain(None).collect();

    let mut shifts: Map<Option<TerminalIdx>, Set<usize>> = Default::default();
    let mut reduces: Map<Option<TerminalIdx>, Set<usize>> = Default::default();

    for (item_idx, item) in state.items.iter().enumerate() {
        if let Some(shift_lookahead) = item.next_terminal(grammar) {
            shifts
                .entry(Some(shift_lookahead))
                .or_default()
                .insert(item_idx);
            continue;
        }

        if item.is_reduce_item(grammar) {
            // Empty lookahead means "any token": lane tracing will never generate an empty set,
            // and we initialize conflicting LALR(1) items with empty set to mean "any token".
            let item_lookaheads = if item.lookahead.is_empty() {
                &all_terminals
            } else {
                &item.lookahead
            };
            for lookahead in item_lookaheads {
                reduces.entry(*lookahead).or_default().insert(item_idx);
            }
        }
    }

    let mut conflicting_item_indices: Set<usize> = Default::default();

    for (reduce_lookahead, reduce_items) in &reduces {
        // Collect reduce-reduce conflicts
        if reduce_items.len() > 1 {
            conflicting_item_indices.extend(reduce_items.iter());
        }

        // Collect shift-reduce conflicts
        if let Some(shift_items) = shifts.get(reduce_lookahead) {
            if !shift_items.is_empty() {
                conflicting_item_indices.extend(reduce_items.iter());
                conflicting_item_indices.extend(shift_items.iter());
            }
        }
    }

    conflicting_item_indices
}

pub struct LALR1StateDisplay<'a, 'b, 'c, A> {
    pub state: &'a LALR1State,
    pub grammar: &'b Grammar<A>,
    pub conflicts: &'c Set<usize>,
}

impl<'a, 'b, 'c, A> fmt::Display for LALR1StateDisplay<'a, 'b, 'c, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (item_idx, item) in self.state.items.iter().enumerate() {
            writeln!(
                f,
                "  {} {}",
                ItemDisplay {
                    item,
                    grammar: self.grammar
                },
                if self.conflicts.contains(&item_idx) {
                    " (*)"
                } else {
                    ""
                }
            )?;
        }

        for (symbol, next) in &self.state.goto {
            writeln!(
                f,
                "  GOTO {} -> {}",
                SymbolDisplay::new(symbol, self.grammar),
                next.0
            )?;
        }

        Ok(())
    }
}
