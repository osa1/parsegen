use crate::grammar::{Grammar, NonTerminalIdx, Symbol};

use std::collections::hash_map::Entry;

use fxhash::{FxHashMap, FxHashSet};

/// Maps non-terminals to their first sets.
#[derive(Debug, Default)]
struct FirstTable(FxHashMap<NonTerminalIdx, FirstSet>);

#[derive(Debug, Default)]
struct FirstSet {
    empty: bool,
    terminals: FxHashSet<char>,
}

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

fn generate_first_table<A>(grammar: &Grammar<char, A>) -> FirstTable {
    let mut table: FirstTable = Default::default();

    let mut updated = true;
    while updated {
        updated = false;
        for (non_terminal_idx, non_terminal) in grammar.non_terminals.iter().enumerate() {
            let non_terminal_idx = NonTerminalIdx::from_usize(non_terminal_idx);
            'production_loop: for production in &non_terminal.productions {
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

/// Maps non-terminals fo their follow sets.
#[derive(Debug, Default)]
struct FollowTable(FxHashMap<NonTerminalIdx, FollowSet>);

#[derive(Debug, Default, Clone)]
struct FollowSet {
    end: bool,
    terminals: FxHashSet<char>,
}

impl FollowTable {
    /// Returns whether the value is added
    fn add_follow(&mut self, non_terminal_idx: NonTerminalIdx, terminal: char) -> bool {
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

    fn get_follow(&self, non_terminal_idx: NonTerminalIdx) -> Option<&FollowSet> {
        self.0.get(&non_terminal_idx)
    }
}

fn generate_follow_table<A>(grammar: &Grammar<char, A>, first_table: &FirstTable) -> FollowTable {
    let mut table: FollowTable = Default::default();

    table.set_end(NonTerminalIdx(0));

    let mut updated = true;
    while updated {
        updated = false;

        // For each non-terminal
        for (non_terminal_idx, _) in grammar.non_terminals.iter().enumerate() {
            println!("Non terminal = {}", non_terminal_idx);

            let non_terminal_idx = NonTerminalIdx::from_usize(non_terminal_idx);
            // For each production
            for (non_terminal_idx_, non_terminal_) in grammar.non_terminals.iter().enumerate() {
                for production in non_terminal_.productions() {
                    // See if the non-terminal is in the RHS, and if it is, what's on the right
                    let mut symbol_iter = production.symbols().iter().cloned();
                    let mut current_symbol: Option<Symbol<char>> = symbol_iter.next();
                    'symbol_loop_0: while let Some(symbol) = current_symbol.take() {
                        if symbol != Symbol::NonTerminal(non_terminal_idx) {
                            current_symbol = symbol_iter.next();
                            continue;
                        }
                        let mut next_symbol = symbol_iter.next();
                        // Skip empty symbols
                        'symbol_loop_1: while let Some(next_symbol_) = next_symbol {
                            match next_symbol_ {
                                Symbol::Terminal(next_char) => {
                                    updated |= table.add_follow(non_terminal_idx, next_char);
                                    // There may be more of the non-terminal we're looking for in
                                    // the RHS, so continue
                                    current_symbol = symbol_iter.next();
                                    continue 'symbol_loop_0;
                                }
                                Symbol::NonTerminal(next_non_terminal) => {
                                    let next_first_set =
                                        first_table.get_first(next_non_terminal).unwrap();
                                    for next_first in &next_first_set.terminals {
                                        updated |= table.add_follow(non_terminal_idx, *next_first);
                                    }
                                    if next_first_set.empty {
                                        next_symbol = symbol_iter.next();
                                        continue 'symbol_loop_1;
                                    } else {
                                        // Same as the terminal case, we may have more of the
                                        // non-terminal we're looking for in the RHS, so continue
                                        current_symbol =
                                            Some(Symbol::NonTerminal(next_non_terminal));
                                        continue 'symbol_loop_0;
                                    }
                                }
                            }
                        }
                        // If we've reached here then the our non-terminal appears the end of the
                        // RHS, so follow set should be the follow set of the current production's
                        // non-terminal
                        if let Some(nt_follows) = table
                            .get_follow(NonTerminalIdx::from_usize(non_terminal_idx_))
                            .cloned()
                        {
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

#[test]
fn first_set_5() {
    let grammar = crate::test_grammars::grammar5();
    let table = generate_first_table(&grammar);

    let e_nt_idx = grammar.get_non_terminal_idx("E").unwrap();
    let e1_nt_idx = grammar.get_non_terminal_idx("E'").unwrap();
    let t_nt_idx = grammar.get_non_terminal_idx("T").unwrap();
    let t1_nt_idx = grammar.get_non_terminal_idx("T'").unwrap();
    let f_nt_idx = grammar.get_non_terminal_idx("F").unwrap();

    assert_eq!(table.0.len(), 5);
    assert_eq!(table.get_first(f_nt_idx).unwrap().empty, false);
    assert_eq!(
        get_nonterminal_firsts_sorted(&table, f_nt_idx),
        vec!['(', 'n']
    );
    assert_eq!(table.get_first(t_nt_idx).unwrap().empty, false);
    assert_eq!(
        get_nonterminal_firsts_sorted(&table, t_nt_idx),
        vec!['(', 'n']
    );
    assert_eq!(table.get_first(e_nt_idx).unwrap().empty, false);
    assert_eq!(
        get_nonterminal_firsts_sorted(&table, e_nt_idx),
        vec!['(', 'n']
    );
    assert_eq!(table.get_first(e1_nt_idx).unwrap().empty, true);
    assert_eq!(get_nonterminal_firsts_sorted(&table, e1_nt_idx), vec!['+']);
    assert_eq!(table.get_first(t1_nt_idx).unwrap().empty, true);
    assert_eq!(get_nonterminal_firsts_sorted(&table, t1_nt_idx), vec!['*']);
}

#[cfg(test)]
fn get_nonterminal_follows_sorted(table: &FollowTable, non_terminal: NonTerminalIdx) -> Vec<char> {
    let mut vec = table
        .get_follow(non_terminal)
        .unwrap()
        .terminals
        .iter()
        .copied()
        .collect::<Vec<char>>();
    vec.sort();
    vec
}

#[test]
fn follow_set_5() {
    let grammar = crate::test_grammars::grammar5();
    let first = generate_first_table(&grammar);
    let follow = generate_follow_table(&grammar, &first);

    let e_nt_idx = grammar.get_non_terminal_idx("E").unwrap();
    let e1_nt_idx = grammar.get_non_terminal_idx("E'").unwrap();
    let t_nt_idx = grammar.get_non_terminal_idx("T").unwrap();
    let t1_nt_idx = grammar.get_non_terminal_idx("T'").unwrap();
    let f_nt_idx = grammar.get_non_terminal_idx("F").unwrap();

    assert_eq!(follow.get_follow(e_nt_idx).unwrap().end, true);
    assert_eq!(get_nonterminal_follows_sorted(&follow, e_nt_idx), vec![')']);
    assert_eq!(follow.get_follow(e1_nt_idx).unwrap().end, true);
    assert_eq!(
        get_nonterminal_follows_sorted(&follow, e1_nt_idx),
        vec![')']
    );
    assert_eq!(follow.get_follow(t_nt_idx).unwrap().end, true);
    assert_eq!(
        get_nonterminal_follows_sorted(&follow, t_nt_idx),
        vec![')', '+']
    );
    assert_eq!(follow.get_follow(t1_nt_idx).unwrap().end, true);
    assert_eq!(
        get_nonterminal_follows_sorted(&follow, t1_nt_idx),
        vec![')', '+']
    );
    assert_eq!(follow.get_follow(f_nt_idx).unwrap().end, true);
    assert_eq!(
        get_nonterminal_follows_sorted(&follow, f_nt_idx),
        vec![')', '*', '+']
    );
}
