//! Implementation of "follow" sets

use crate::first::FirstTable;
use crate::grammar::{Grammar, NonTerminalIdx, SymbolKind};
use crate::terminal::TerminalReprIdx;

use std::collections::hash_map::Entry;

use fxhash::{FxHashMap, FxHashSet};

/// Maps non-terminals fo their follow sets
#[derive(Debug)]
pub struct FollowTable(FxHashMap<NonTerminalIdx, FollowSet>);

impl Default for FollowTable {
    fn default() -> Self {
        FollowTable(Default::default())
    }
}

#[derive(Debug, Clone)]
pub struct FollowSet {
    end: bool,
    terminals: FxHashSet<TerminalReprIdx>,
}

impl Default for FollowSet {
    fn default() -> Self {
        FollowSet {
            end: false,
            terminals: Default::default(),
        }
    }
}

impl FollowSet {
    pub fn terminals(&self) -> &FxHashSet<TerminalReprIdx> {
        &self.terminals
    }

    pub fn has_end(&self) -> bool {
        self.end
    }
}

impl FollowTable {
    /// Returns whether the value is added
    fn add_follow(&mut self, non_terminal_idx: NonTerminalIdx, terminal: TerminalReprIdx) -> bool {
        self.0
            .entry(non_terminal_idx)
            .or_default()
            .terminals
            .insert(terminal)
    }

    /// Returns whether the value is changed
    fn set_end(&mut self, non_terminal_idx: NonTerminalIdx) -> bool {
        match self.0.entry(non_terminal_idx) {
            Entry::Occupied(mut entry) => {
                let old_value = entry.get().end;
                entry.get_mut().end = true;
                old_value != true
            }
            Entry::Vacant(entry) => {
                entry.insert(FollowSet {
                    end: true,
                    terminals: Default::default(),
                });
                true
            }
        }
    }

    pub fn get_follow(&self, non_terminal_idx: NonTerminalIdx) -> Option<&FollowSet> {
        self.0.get(&non_terminal_idx)
    }
}

pub fn generate_follow_table<A>(
    grammar: &Grammar<TerminalReprIdx, A>,
    first_table: &FirstTable,
) -> FollowTable {
    let mut table: FollowTable = Default::default();

    table.set_end(NonTerminalIdx(0));

    let mut updated = true;
    while updated {
        updated = false;

        // For each non-terminal
        for (non_terminal_idx, _) in grammar.non_terminal_indices() {
            // For each production
            for (non_terminal_idx_, _, production) in grammar.production_indices() {
                // See if the non-terminal is in the RHS, and if it is, what's on the right
                let mut symbol_iter = production.symbols().iter().map(|s| s.kind.clone());
                let mut current_symbol: Option<SymbolKind<TerminalReprIdx>> = symbol_iter.next();
                'symbol_loop_0: while let Some(symbol) = current_symbol.take() {
                    if symbol != SymbolKind::NonTerminal(non_terminal_idx) {
                        current_symbol = symbol_iter.next();
                        continue;
                    }
                    let mut next_symbol = symbol_iter.next();
                    // Skip empty symbols
                    'symbol_loop_1: while let Some(next_symbol_) = next_symbol {
                        match next_symbol_ {
                            SymbolKind::Terminal(next_terminal) => {
                                updated |=
                                    table.add_follow(non_terminal_idx, next_terminal.clone());
                                // There may be more of the non-terminal we're looking for in
                                // the RHS, so continue
                                current_symbol = symbol_iter.next();
                                continue 'symbol_loop_0;
                            }
                            SymbolKind::NonTerminal(next_non_terminal) => {
                                let next_first_set =
                                    first_table.get_first(next_non_terminal).unwrap();
                                for next_first in next_first_set.terminals().iter() {
                                    updated |=
                                        table.add_follow(non_terminal_idx, next_first.clone());
                                }
                                if next_first_set.has_empty() {
                                    next_symbol = symbol_iter.next();
                                    continue 'symbol_loop_1;
                                } else {
                                    // Same as the terminal case, we may have more of the
                                    // non-terminal we're looking for in the RHS, so continue
                                    current_symbol =
                                        Some(SymbolKind::NonTerminal(next_non_terminal));
                                    continue 'symbol_loop_0;
                                }
                            }
                        }
                    }
                    // If we've reached here then the our non-terminal appears the end of the
                    // RHS, so follow set should be the follow set of the current production's
                    // non-terminal
                    if let Some(nt_follows) = table.get_follow(non_terminal_idx_).cloned() {
                        if nt_follows.end {
                            updated |= table.set_end(non_terminal_idx);
                        }
                        for follow in nt_follows.terminals {
                            updated |= table.add_follow(non_terminal_idx, follow);
                        }
                    }
                }
            }
        }
    }

    table
}
