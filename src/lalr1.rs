use crate::collections::{Map, Set};
use crate::grammar::{Grammar, Symbol, SymbolDisplay, TerminalIdx};
use crate::item::{Item, ItemDisplay};
use crate::lr0::LR0State;
use crate::lr_common::StateIdx;
use crate::state::LRState;

use std::fmt;

pub type LALR1Item = Item<Set<Option<TerminalIdx>>>;

pub struct LALR1State {
    pub items: Vec<Item<Set<Option<TerminalIdx>>>>,
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

pub struct LALR1StateDisplay<'a, 'b, A> {
    pub state: &'a LALR1State,
    pub grammar: &'b Grammar<A>,
}

impl<'a, 'b, A> fmt::Display for LALR1StateDisplay<'a, 'b, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for item in &self.state.items {
            writeln!(
                f,
                "  {}",
                ItemDisplay {
                    item,
                    grammar: self.grammar
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
