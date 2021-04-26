//! Definitions of Earley items and sets

use crate::grammar::{Grammar, NonTerminalIdx, ProductionIdx, Symbol};

use std::fmt;

use fxhash::FxHashSet;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct EarleyItem {
    /// The non-terminal for the production of this item
    pub non_terminal: NonTerminalIdx,

    /// The production
    pub production: ProductionIdx,

    /// Current position in the production
    pub position: u32,

    /// Index of the Earley set that this item was created from (via the "scanner" rule)
    pub set_idx: u32,
}

#[derive(Debug, Default)]
pub struct EarleySet {
    pub items: FxHashSet<EarleyItem>,
}

/// Display an Earley item following the syntax in the "Practical Earley Parsing" paper.
pub fn display_earley_item<T, A>(
    f: &mut fmt::Formatter<'_>,
    item: EarleyItem,
    grammar: &Grammar<T, A>,
    display_symbol: fn(&mut fmt::Formatter<'_>, &Grammar<T, A>, &Symbol<T>) -> fmt::Result,
) -> fmt::Result {
    write!(
        f,
        "[{} -> ",
        grammar.get_non_terminal(item.non_terminal).name()
    )?;

    let production = grammar.get_production(item.production);
    let production_symbols = production.symbols();
    for (symbol_idx, symbol) in production_symbols.iter().enumerate() {
        if symbol_idx == item.position as usize {
            write!(f, "|")?;
        }
        display_symbol(f, grammar, symbol)?;
        if symbol_idx != production_symbols.len() - 1 {
            write!(f, " ")?;
        }
    }
    if item.position as usize == production_symbols.len() {
        write!(f, "|")?;
    }
    write!(f, ",{}]", item.set_idx)
}

/// Display an Earley set following the syntax in the "Practical Earley Parsing" paper.
pub fn display_earley_set<T, A>(
    f: &mut fmt::Formatter<'_>,
    set: EarleySet,
    grammar: &Grammar<T, A>,
    display_symbol: fn(&mut fmt::Formatter<'_>, &Grammar<T, A>, &Symbol<T>) -> fmt::Result,
) -> fmt::Result {
    write!(f, "{{")?;
    for (item_idx, item) in set.items.iter().copied().enumerate() {
        display_earley_item(f, item, grammar, display_symbol)?;
        if item_idx != set.items.len() - 1 {
            write!(f, ", ")?;
        }
    }
    write!(f, "}}")
}
