//! Implementation of "follow" sets

use crate::bitset::{BitSet, FromBitIdx, ToBitIdx};
use crate::first::FirstTable;
use crate::grammar::{Grammar, NonTerminalIdx, SymbolKind};

/// Maps non-terminals fo their follow sets
#[derive(Debug)]
pub struct FollowTable<T: ToBitIdx>(Vec<FollowSet<T>>);

#[derive(Debug, Clone)]
pub struct FollowSet<T: ToBitIdx> {
    end: bool,
    terminals: BitSet<T>,
}

impl<T: ToBitIdx> FollowSet<T> {
    pub fn new(n_terminals: usize) -> FollowSet<T> {
        FollowSet {
            end: false,
            terminals: BitSet::new(n_terminals),
        }
    }

    pub fn has_end(&self) -> bool {
        self.end
    }
}

impl<T: ToBitIdx + FromBitIdx> FollowSet<T> {
    pub fn terminals<'a>(&'a self) -> impl Iterator<Item = T> + 'a {
        self.terminals.elems()
    }
}

impl<T: ToBitIdx> FollowTable<T> {
    fn new(n_non_terminals: usize, n_terminals: usize) -> FollowTable<T> {
        let mut sets = Vec::with_capacity(n_non_terminals);
        for _ in 0..n_non_terminals {
            sets.push(FollowSet::new(n_terminals));
        }
        FollowTable(sets)
    }

    /// Returns whether the value is added
    fn add_follow(&mut self, non_terminal_idx: NonTerminalIdx, terminal: &T) -> bool {
        self.0[non_terminal_idx.as_usize()].terminals.set(terminal)
    }

    /// Returns whether the value is changed
    fn set_end(&mut self, non_terminal_idx: NonTerminalIdx) -> bool {
        let end = &mut self.0[non_terminal_idx.as_usize()].end;
        let old_value = *end;
        *end = true;
        old_value != true
    }

    pub fn get_follow(&self, non_terminal_idx: NonTerminalIdx) -> &FollowSet<T> {
        &self.0[non_terminal_idx.as_usize()]
    }
}

pub fn generate_follow_table<T: ToBitIdx + FromBitIdx + Clone + Eq, A>(
    grammar: &Grammar<T, A>,
    first_table: &FirstTable<T>,
    n_terminals: usize,
) -> FollowTable<T> {
    let n_non_terminals = grammar.non_terminals().len();
    let mut table = FollowTable::new(grammar.non_terminals().len(), n_terminals);

    if n_non_terminals == 0 {
        return table;
    }

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
                let mut current_symbol: Option<SymbolKind<T>> = symbol_iter.next();
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
                                    table.add_follow(non_terminal_idx, &next_terminal);
                                // There may be more of the non-terminal we're looking for in
                                // the RHS, so continue
                                current_symbol = symbol_iter.next();
                                continue 'symbol_loop_0;
                            }
                            SymbolKind::NonTerminal(next_non_terminal) => {
                                let next_first_set = first_table.get_first(next_non_terminal);
                                for next_first in next_first_set.terminals() {
                                    updated |= table.add_follow(non_terminal_idx, &next_first);
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
                    let nt_follows = table.get_follow(non_terminal_idx_).clone();
                    if nt_follows.end {
                        updated |= table.set_end(non_terminal_idx);
                    }
                    for follow in nt_follows.terminals() {
                        updated |= table.add_follow(non_terminal_idx, &follow);
                    }
                }
            }
        }
    }

    table
}
