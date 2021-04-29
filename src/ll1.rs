use crate::grammar::{Grammar, NonTerminalIdx, Symbol};

use std::collections::hash_map::Entry;

use fxhash::{FxHashMap, FxHashSet};

#[derive(Debug, Default)]
struct FirstSet {
    empty: bool,
    terminals: FxHashSet<char>,
}

/// Maps non-terminals to their first sets.
#[derive(Debug, Default)]
struct FirstTable(FxHashMap<NonTerminalIdx, FirstSet>);

impl FirstTable {
    /// Returns whether the value is added
    fn add_first(&mut self, non_terminal_idx: NonTerminalIdx, terminal: char) -> bool {
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

    fn get_first(&self, non_terminal_idx: NonTerminalIdx) -> Option<&FirstSet> {
        self.0.get(&non_terminal_idx)
    }
}

fn generate_first_table<A: std::fmt::Debug>(grammar: &Grammar<char, A>) -> FirstTable {
    let mut table: FirstTable = Default::default();

    let mut updated = true;
    while updated {
        updated = false;
        for (non_terminal_idx, non_terminal) in grammar.non_terminals.iter().enumerate() {
            println!("{}", non_terminal_idx);

            let non_terminal_idx = NonTerminalIdx::from_usize(non_terminal_idx);
            'production_loop: for production in &non_terminal.productions {
                println!("production={:?}", production);
                if production.symbols.is_empty() {
                    updated |= table.set_empty(non_terminal_idx);
                }
                for symbol in &production.symbols {
                    match symbol {
                        Symbol::NonTerminal(nt) => match table.get_first(*nt) {
                            Some(FirstSet { empty, terminals }) => {
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
                            None => {
                                // Non-terminal not in the table yet, we will revisit this later
                                continue 'production_loop;
                            }
                        },
                        Symbol::Terminal(char) => {
                            updated |= table.add_first(non_terminal_idx, *char);
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

#[cfg(test)]
fn get_nonterminal_firsts_sorted(table: &FirstTable, non_terminal: NonTerminalIdx) -> Vec<char> {
    let mut vec = table
        .get_first(non_terminal)
        .unwrap()
        .terminals
        .iter()
        .copied()
        .collect::<Vec<char>>();
    vec.sort();
    vec
}

#[test]
fn first_set_1() {
    let grammar = crate::test_grammars::grammar1();
    let table = generate_first_table(&grammar);

    assert_eq!(table.0.len(), 1);
    assert_eq!(table.get_first(NonTerminalIdx(0)).unwrap().empty, false);
    assert_eq!(
        get_nonterminal_firsts_sorted(&table, NonTerminalIdx(0)),
        vec!['n']
    );
}

#[test]
fn first_set_2() {
    let grammar = crate::test_grammars::grammar2();
    let table = generate_first_table(&grammar);

    assert_eq!(table.0.len(), 3);
    assert_eq!(table.get_first(NonTerminalIdx(2)).unwrap().empty, true);
    assert_eq!(table.get_first(NonTerminalIdx(1)).unwrap().empty, true);
    assert_eq!(table.get_first(NonTerminalIdx(0)).unwrap().empty, true);
    assert_eq!(
        get_nonterminal_firsts_sorted(&table, NonTerminalIdx(2)),
        vec![]
    );
    assert_eq!(
        get_nonterminal_firsts_sorted(&table, NonTerminalIdx(1)),
        vec!['a']
    );
    assert_eq!(
        get_nonterminal_firsts_sorted(&table, NonTerminalIdx(0)),
        vec!['a']
    );
}

#[test]
fn first_set_3() {
    let grammar = crate::test_grammars::grammar3();
    let table = generate_first_table(&grammar);

    assert_eq!(table.0.len(), 4);
    assert_eq!(table.get_first(NonTerminalIdx(3)).unwrap().empty, false);
    assert_eq!(table.get_first(NonTerminalIdx(2)).unwrap().empty, false);
    assert_eq!(table.get_first(NonTerminalIdx(1)).unwrap().empty, false);
    assert_eq!(table.get_first(NonTerminalIdx(0)).unwrap().empty, false);
    assert_eq!(
        get_nonterminal_firsts_sorted(&table, NonTerminalIdx(3)),
        vec!['a']
    );
    assert_eq!(
        get_nonterminal_firsts_sorted(&table, NonTerminalIdx(2)),
        vec!['a']
    );
    assert_eq!(
        get_nonterminal_firsts_sorted(&table, NonTerminalIdx(1)),
        vec!['a']
    );
    assert_eq!(
        get_nonterminal_firsts_sorted(&table, NonTerminalIdx(0)),
        vec!['a']
    );
}

#[test]
fn first_set_4() {
    let grammar = crate::test_grammars::grammar4();
    let table = generate_first_table(&grammar);

    assert_eq!(table.0.len(), 3);
    assert_eq!(table.get_first(NonTerminalIdx(2)).unwrap().empty, false);
    assert_eq!(table.get_first(NonTerminalIdx(1)).unwrap().empty, true);
    assert_eq!(table.get_first(NonTerminalIdx(0)).unwrap().empty, false);
    assert_eq!(
        get_nonterminal_firsts_sorted(&table, NonTerminalIdx(2)),
        vec!['a']
    );
    assert_eq!(
        get_nonterminal_firsts_sorted(&table, NonTerminalIdx(1)),
        vec![]
    );
    assert_eq!(
        get_nonterminal_firsts_sorted(&table, NonTerminalIdx(0)),
        vec!['a']
    );
}
