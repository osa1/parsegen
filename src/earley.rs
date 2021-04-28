//! Definitions of Earley items and sets

use crate::grammar::{Grammar, NonTerminalIdx, ProductionIdx, Symbol};

use std::fmt;
use std::hash;

use fxhash::FxHashSet;

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord, Hash)]
pub struct EarleyItemIdx(pub u32);

#[derive(Debug, Clone, Copy)]
pub struct EarleyItem {
    /// An unique index for this set. Not used for `Eq` or `Hash` implementations of `EarleyItem`.
    /// Used to allow mapping an `EarleyItem` to its parent/child items.
    pub idx: EarleyItemIdx,

    /// The non-terminal for the production of this item
    pub non_terminal: NonTerminalIdx,

    /// The production
    pub production: ProductionIdx,

    /// Current position in the production
    pub position: u32,

    /// Index of the Earley set that this item was created from (via the "scanner" rule)
    pub set_idx: u32,
}

impl PartialEq for EarleyItem {
    fn eq(&self, other: &Self) -> bool {
        self.non_terminal == other.non_terminal
            && self.production == other.production
            && self.position == other.position
            && self.set_idx == other.set_idx
    }
}

impl Eq for EarleyItem {}

impl hash::Hash for EarleyItem {
    fn hash<H: hash::Hasher>(&self, state: &mut H) -> () {
        match self {
            EarleyItem {
                idx: _,
                non_terminal,
                production,
                position,
                set_idx,
            } => {
                hash::Hash::hash(non_terminal, state);
                hash::Hash::hash(production, state);
                hash::Hash::hash(position, state);
                hash::Hash::hash(set_idx, state)
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct EarleySet {
    pub items: FxHashSet<EarleyItem>,
}

impl EarleySet {
    /// Try to get an existing item, without allocating an `EarleyItemIdx` for the lookup
    pub fn get_idx(
        &self,
        non_terminal: NonTerminalIdx,
        production: ProductionIdx,
        position: u32,
        set_idx: u32,
    ) -> Option<EarleyItemIdx> {
        self.items
            .get(&EarleyItem {
                idx: EarleyItemIdx(0), // doesn't matter
                non_terminal,
                production,
                position,
                set_idx,
            })
            .map(|item| item.idx)
    }

    pub fn insert(&mut self, item: EarleyItem) {
        let not_exists = self.items.insert(item);
        assert!(not_exists);
    }

    /// Extends current set with another. Panics if there's a duplicate.
    pub fn merge(&mut self, other: EarleySet) {
        for item in other.items {
            let not_exists = self.items.insert(item);
            assert!(not_exists);
        }
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

/// Display an Earley item following the syntax in the "Practical Earley Parsing" paper.
pub fn display_earley_item<A>(
    f: &mut fmt::Formatter<'_>,
    item: EarleyItem,
    grammar: &Grammar<char, A>,
) -> fmt::Result {
    write!(
        f,
        "[{} -> ",
        grammar.get_non_terminal(item.non_terminal).name()
    )?;

    let production = grammar.get_production(item.non_terminal, item.production);
    let production_symbols = production.symbols();
    for (symbol_idx, symbol) in production_symbols.iter().enumerate() {
        if symbol_idx == item.position as usize {
            write!(f, "|")?;
        }
        match symbol {
            Symbol::NonTerminal(nt_idx) => {
                let nt = grammar.get_non_terminal(*nt_idx);
                <str as fmt::Display>::fmt(nt.name(), f)?;
            }
            Symbol::Terminal(char) => {
                <char as fmt::Debug>::fmt(char, f)?;
            }
        }
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
pub fn display_earley_set<A>(
    f: &mut fmt::Formatter<'_>,
    set: &EarleySet,
    grammar: &Grammar<char, A>,
) -> fmt::Result {
    write!(f, "{{")?;
    for (item_idx, item) in set.items.iter().copied().enumerate() {
        display_earley_item(f, item, grammar)?;
        if item_idx != set.items.len() - 1 {
            write!(f, ", ")?;
        }
    }
    write!(f, "}}")
}

pub struct EarleySetDisplay<'set, 'grammar, A> {
    pub set: &'set EarleySet,
    pub grammar: &'grammar Grammar<char, A>,
}

impl<'set, 'grammar, A> fmt::Display for EarleySetDisplay<'set, 'grammar, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_earley_set(f, self.set, self.grammar)
    }
}
