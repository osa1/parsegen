//! Implementation of "first" sets

use crate::grammar::{Grammar, NonTerminalIdx, Symbol, TerminalIdx};

use fxhash::FxHashSet;

/// Maps non-terminals to their first sets
#[derive(Debug)]
pub struct FirstTable(Vec<FirstSet>);

#[derive(Debug, Clone, Default)]
pub struct FirstSet {
    empty: bool,
    terminals: FxHashSet<TerminalIdx>,
}

impl FirstSet {
    pub fn add(&mut self, terminal: TerminalIdx) {
        self.terminals.insert(terminal);
    }

    pub fn set_empty(&mut self) {
        self.empty = true;
    }

    pub fn terminals(&self) -> &FxHashSet<TerminalIdx> {
        &self.terminals
    }

    pub fn has_empty(&self) -> bool {
        self.empty
    }
}

impl FirstTable {
    fn new(n_non_terminals: usize) -> FirstTable {
        let mut sets = Vec::with_capacity(n_non_terminals);
        for _ in 0..n_non_terminals {
            sets.push(FirstSet::default());
        }
        FirstTable(sets)
    }

    /// Returns whether the value is added
    fn add_first(&mut self, non_terminal_idx: NonTerminalIdx, terminal: TerminalIdx) -> bool {
        self.0[non_terminal_idx.as_usize()]
            .terminals
            .insert(terminal)
    }

    /// Returns whether the value is changed
    fn set_empty(&mut self, non_terminal_idx: NonTerminalIdx) -> bool {
        let empty = &mut self.0[non_terminal_idx.as_usize()].empty;
        let old_value = *empty;
        *empty = true;
        !old_value
    }

    pub fn get_first(&self, non_terminal_idx: NonTerminalIdx) -> &FirstSet {
        &self.0[non_terminal_idx.as_usize()]
    }
}

pub fn generate_first_table<A>(grammar: &Grammar<A>) -> FirstTable {
    let mut table: FirstTable = FirstTable::new(grammar.non_terminals().len());

    let mut updated = true;
    while updated {
        updated = false;
        for (non_terminal_idx, non_terminal) in grammar.non_terminal_indices() {
            'production_loop: for production in &non_terminal.productions {
                if production.symbols.is_empty() {
                    updated |= table.set_empty(non_terminal_idx);
                }
                for symbol in &production.symbols {
                    match &symbol.symbol {
                        Symbol::NonTerminal(nt) => {
                            let FirstSet { empty, terminals } = table.get_first(*nt);
                            if *empty {
                                // Continue to the next symbol in the production
                                continue;
                            }
                            // TODO: clone below to avoid borrowck issues
                            for terminal in terminals.clone() {
                                updated |= table.add_first(non_terminal_idx, terminal);
                            }
                            continue 'production_loop;
                        }
                        Symbol::Terminal(terminal) => {
                            updated |= table.add_first(non_terminal_idx, *terminal);
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

pub struct FirstTableDisplay<'a, 'b, A> {
    pub table: &'b FirstTable,
    pub grammar: &'a Grammar<A>,
}

pub struct FirstSetDisplay<'a, 'b, A> {
    pub set: &'a FirstSet,
    pub grammar: &'b Grammar<A>,
}

impl<'a, 'b, A> fmt::Display for FirstTableDisplay<'a, 'b, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{{")?;
        for (nt_idx, set) in self.table.0.iter().enumerate() {
            let non_terminal = &self
                .grammar
                .get_non_terminal(NonTerminalIdx::from_usize(nt_idx))
                .non_terminal;
            write!(
                f,
                "    {}: {}",
                non_terminal,
                FirstSetDisplay {
                    set,
                    grammar: self.grammar,
                }
            )?;
            writeln!(f)?;
        }
        writeln!(f, "}}")
    }
}

impl<'a, 'b, A> fmt::Display for FirstSetDisplay<'a, 'b, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        for (t_idx, t) in self.set.terminals.iter().enumerate() {
            write!(f, "{:?}", self.grammar.get_terminal(*t))?;
            if t_idx != self.set.terminals.len() - 1 {
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
