//! Implementation of "first" sets

use crate::bitset::{BitSet, FromBitIdx, ToBitIdx};
use crate::grammar::{Grammar, NonTerminalIdx, SymbolKind};

/// Maps non-terminals to their first sets
#[derive(Debug)]
pub struct FirstTable<T: ToBitIdx>(Vec<FirstSet<T>>);

#[derive(Debug, Clone)]
pub struct FirstSet<T: ToBitIdx> {
    empty: bool,
    terminals: BitSet<T>,
}

impl<T: ToBitIdx> FirstSet<T> {
    pub fn new(n_terminals: usize) -> Self {
        FirstSet {
            empty: false,
            terminals: BitSet::new(n_terminals),
        }
    }

    pub fn add(&mut self, terminal: &T) {
        self.terminals.set(terminal);
    }

    pub fn set_empty(&mut self) {
        self.empty = true;
    }

    pub fn has_empty(&self) -> bool {
        self.empty
    }

    pub fn n_terminals(&self) -> usize {
        self.terminals.len()
    }
}

impl<T: ToBitIdx + FromBitIdx> FirstSet<T> {
    pub fn terminals<'a>(&'a self) -> impl Iterator<Item = T> + 'a {
        self.terminals.elems()
    }
}

impl<T: ToBitIdx> FirstTable<T> {
    fn new(n_non_terminals: usize, n_terminals: usize) -> FirstTable<T> {
        let mut sets = Vec::with_capacity(n_non_terminals);
        for _ in 0..n_non_terminals {
            sets.push(FirstSet {
                empty: false,
                terminals: BitSet::new(n_terminals),
            });
        }
        FirstTable(sets)
    }

    /// Returns whether the value is added
    fn add_first(&mut self, non_terminal_idx: NonTerminalIdx, terminal: &T) -> bool {
        self.0[non_terminal_idx.as_usize()].terminals.set(terminal)
    }

    /// Returns whether the value is changed
    fn set_empty(&mut self, non_terminal_idx: NonTerminalIdx) -> bool {
        let empty = &mut self.0[non_terminal_idx.as_usize()].empty;
        let old_value = *empty;
        *empty = true;
        old_value != true
    }

    pub fn get_first(&self, non_terminal_idx: NonTerminalIdx) -> &FirstSet<T> {
        &self.0[non_terminal_idx.as_usize()]
    }
}

pub fn generate_first_table<T: ToBitIdx + FromBitIdx, A>(
    grammar: &Grammar<T, A>,
    n_terminals: usize,
) -> FirstTable<T> {
    let mut table: FirstTable<T> = FirstTable::new(grammar.non_terminals().len(), n_terminals);

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
                        SymbolKind::NonTerminal(nt) => {
                            let FirstSet { empty, terminals } = table.get_first(*nt);
                            if *empty {
                                // Continue to the next symbol in the production
                                continue;
                            }
                            // TODO: clone below to avoid borrowck issues
                            for terminal in terminals.clone().elems() {
                                updated |= table.add_first(non_terminal_idx, &terminal);
                            }
                            continue 'production_loop;
                        }
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

use std::fmt;

pub struct FirstSetDisplay<'a, T: ToBitIdx + fmt::Debug> {
    pub set: &'a FirstSet<T>,
}

impl<'a, T: ToBitIdx + FromBitIdx + fmt::Debug> fmt::Display for FirstSetDisplay<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let n_terminals = self.set.n_terminals();
        write!(f, "{{")?;
        for (t_idx, t) in self.set.terminals().enumerate() {
            write!(f, "{:?}", t)?;
            if t_idx != n_terminals - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")?;

        if self.set.empty {
            write!(f, " (+ empty)")?;
        }

        Ok(())
    }
}
