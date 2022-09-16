use crate::collections::Map;
use crate::grammar::{Grammar, Symbol, SymbolDisplay};
use crate::item::{Item, ItemDisplay, LookaheadDisplay};
use crate::lr_common::StateIdx;

use std::collections::BTreeSet;
use std::fmt;

#[derive(Debug)]
pub struct LRState<LA> {
    pub items: BTreeSet<Item<LA>>,
    pub goto: Map<Symbol, StateIdx>,
}

pub struct LRStateDisplay<'a, 'b, LA: LookaheadDisplay, A> {
    pub state: &'a LRState<LA>,
    pub grammar: &'b Grammar<A>,
}

impl<'a, 'b, LA: LookaheadDisplay, A> fmt::Display for LRStateDisplay<'a, 'b, LA, A> {
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
