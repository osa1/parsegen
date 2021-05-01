//! Implementation of "first" sets

use crate::grammar::{Grammar, NonTerminalIdx, SymbolKind};
use crate::terminal::TerminalReprIdx;

use std::collections::hash_map::Entry;

use fxhash::{FxHashMap, FxHashSet};

/// Maps non-terminals to their first sets
#[derive(Debug)]
pub struct FirstTable(FxHashMap<NonTerminalIdx, FirstSet>);

impl Default for FirstTable {
    fn default() -> Self {
        FirstTable(Default::default())
    }
}

#[derive(Debug)]
pub struct FirstSet {
    empty: bool,
    terminals: FxHashSet<TerminalReprIdx>,
}

impl FirstSet {
    pub fn terminals(&self) -> &FxHashSet<TerminalReprIdx> {
        &self.terminals
    }

    pub fn has_empty(&self) -> bool {
        self.empty
    }
}

impl Default for FirstSet {
    fn default() -> Self {
        FirstSet {
            empty: false,
            terminals: Default::default(),
        }
    }
}

impl FirstTable {
    /// Returns whether the value is added
    fn add_first(&mut self, non_terminal_idx: NonTerminalIdx, terminal: TerminalReprIdx) -> bool {
        self.0
            .entry(non_terminal_idx)
            .or_default()
            .terminals
            .insert(terminal)
    }

    /// Returns whether the value is changed
    fn set_empty(&mut self, non_terminal_idx: NonTerminalIdx) -> bool {
        match self.0.entry(non_terminal_idx) {
            Entry::Occupied(mut entry) => {
                let old_value = entry.get().empty;
                entry.get_mut().empty = true;
                old_value != true
            }
            Entry::Vacant(entry) => {
                entry.insert(FirstSet {
                    empty: true,
                    terminals: Default::default(),
                });
                true
            }
        }
    }

    pub fn get_first(&self, non_terminal_idx: NonTerminalIdx) -> Option<&FirstSet> {
        self.0.get(&non_terminal_idx)
    }
}

pub fn generate_first_table<A>(grammar: &Grammar<TerminalReprIdx, A>) -> FirstTable {
    let mut table: FirstTable = Default::default();

    let mut updated = true;
    while updated {
        updated = false;
        for (non_terminal_idx, non_terminal) in grammar.non_terminal_indices() {
            'production_loop: for production in &non_terminal.productions {
                if production.symbols.is_empty() {
                    updated |= table.set_empty(non_terminal_idx);
                }
                for symbol in &production.symbols {
                    match &symbol.kind {
                        SymbolKind::NonTerminal(nt) => match table.get_first(*nt) {
                            Some(FirstSet { empty, terminals }) => {
                                if *empty {
                                    // Continue to the next symbol in the production
                                    continue;
                                }
                                // TODO: clone below to avoid borrowck issues
                                for terminal in terminals.clone() {
                                    updated |= table.add_first(non_terminal_idx, terminal.clone());
                                }
                                continue 'production_loop;
                            }
                            None => {
                                // Non-terminal not in the table yet, we will revisit this later
                                continue 'production_loop;
                            }
                        },
                        SymbolKind::Terminal(terminal) => {
                            updated |= table.add_first(non_terminal_idx, terminal.clone());
                            continue 'production_loop;
                        }
                    }
                }
                // If we reached here then all symbols in the production are empty, so the
                // non-terminal can be empty
                table.set_empty(non_terminal_idx);
            }
        }
    }

    table
}
