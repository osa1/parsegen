use crate::grammar::{Grammar, NonTerminalIdx, ProductionIdx, Symbol};

use std::collections::hash_map::Entry;
use std::hash::Hash;

use fxhash::{FxHashMap, FxHashSet};

/// Maps non-terminals to their first sets.
#[derive(Debug)]
struct FirstTable<T: Hash + Eq>(FxHashMap<NonTerminalIdx, FirstSet<T>>);

impl<T: Hash + Eq> Default for FirstTable<T> {
    fn default() -> Self {
        FirstTable(Default::default())
    }
}

#[derive(Debug)]
struct FirstSet<T> {
    empty: bool,
    terminals: FxHashSet<T>,
}

impl<T> Default for FirstSet<T> {
    fn default() -> Self {
        FirstSet {
            empty: false,
            terminals: Default::default(),
        }
    }
}

impl<T: Hash + Eq> FirstTable<T> {
    /// Returns whether the value is added
    fn add_first(&mut self, non_terminal_idx: NonTerminalIdx, terminal: T) -> bool {
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

    fn get_first(&self, non_terminal_idx: NonTerminalIdx) -> Option<&FirstSet<T>> {
        self.0.get(&non_terminal_idx)
    }
}

fn generate_first_table<T: Hash + Eq + Clone, A>(grammar: &Grammar<T, A>) -> FirstTable<T> {
    let mut table: FirstTable<T> = Default::default();

    let mut updated = true;
    while updated {
        updated = false;
        for (non_terminal_idx, non_terminal) in grammar.non_terminal_indices() {
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
                                    updated |= table.add_first(non_terminal_idx, terminal.clone());
                                }
                                continue 'production_loop;
                            }
                            None => {
                                // Non-terminal not in the table yet, we will revisit this later
                                continue 'production_loop;
                            }
                        },
                        Symbol::Terminal(terminal) => {
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

/// Maps non-terminals fo their follow sets.
#[derive(Debug)]
struct FollowTable<T: Hash + Eq>(FxHashMap<NonTerminalIdx, FollowSet<T>>);

impl<T: Hash + Eq> Default for FollowTable<T> {
    fn default() -> Self {
        FollowTable(Default::default())
    }
}

#[derive(Debug, Clone)]
struct FollowSet<T: Hash + Eq> {
    end: bool,
    terminals: FxHashSet<T>,
}

impl<T: Hash + Eq> Default for FollowSet<T> {
    fn default() -> Self {
        FollowSet {
            end: false,
            terminals: Default::default(),
        }
    }
}

impl<T: Hash + Eq> FollowTable<T> {
    /// Returns whether the value is added
    fn add_follow(&mut self, non_terminal_idx: NonTerminalIdx, terminal: T) -> bool {
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

    fn get_follow(&self, non_terminal_idx: NonTerminalIdx) -> Option<&FollowSet<T>> {
        self.0.get(&non_terminal_idx)
    }
}

fn generate_follow_table<T: Hash + Eq + Clone, A>(
    grammar: &Grammar<T, A>,
    first_table: &FirstTable<T>,
) -> FollowTable<T> {
    let mut table: FollowTable<T> = Default::default();

    table.set_end(NonTerminalIdx(0));

    let mut updated = true;
    while updated {
        updated = false;

        // For each non-terminal
        for (non_terminal_idx, _) in grammar.non_terminal_indices() {
            // For each production
            for (non_terminal_idx_, _, production) in grammar.production_indices() {
                // See if the non-terminal is in the RHS, and if it is, what's on the right
                let mut symbol_iter = production.symbols().iter().cloned();
                let mut current_symbol: Option<Symbol<T>> = symbol_iter.next();
                'symbol_loop_0: while let Some(symbol) = current_symbol.take() {
                    if symbol != Symbol::NonTerminal(non_terminal_idx) {
                        current_symbol = symbol_iter.next();
                        continue;
                    }
                    let mut next_symbol = symbol_iter.next();
                    // Skip empty symbols
                    'symbol_loop_1: while let Some(next_symbol_) = next_symbol {
                        match next_symbol_ {
                            Symbol::Terminal(next_terminal) => {
                                updated |=
                                    table.add_follow(non_terminal_idx, next_terminal.clone());
                                // There may be more of the non-terminal we're looking for in
                                // the RHS, so continue
                                current_symbol = symbol_iter.next();
                                continue 'symbol_loop_0;
                            }
                            Symbol::NonTerminal(next_non_terminal) => {
                                let next_first_set =
                                    first_table.get_first(next_non_terminal).unwrap();
                                for next_first in &next_first_set.terminals {
                                    updated |=
                                        table.add_follow(non_terminal_idx, next_first.clone());
                                }
                                if next_first_set.empty {
                                    next_symbol = symbol_iter.next();
                                    continue 'symbol_loop_1;
                                } else {
                                    // Same as the terminal case, we may have more of the
                                    // non-terminal we're looking for in the RHS, so continue
                                    current_symbol = Some(Symbol::NonTerminal(next_non_terminal));
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

/// Predictive parse table
#[derive(Debug)]
struct ParseTable<T: Hash + Eq> {
    // Informally: if I'm parsing the non-terminal `NT` and next token is `c`, then `(NT, c)` in
    // the table tells me which production to expect. If there isn't an entry for `(NT, c)` then we
    // have an error in the input. During building, if we try to add multiple productions to a
    // non-terminal, token pair, that means (I think, TODO: make sure) we have an ambiguous, or
    // left-recursive grammar.
    table: FxHashMap<(NonTerminalIdx, T), ProductionIdx>,

    // Same as `table`, but for `$`
    end: FxHashMap<NonTerminalIdx, ProductionIdx>,
}

impl<T: Hash + Eq> Default for ParseTable<T> {
    fn default() -> Self {
        ParseTable {
            table: Default::default(),
            end: Default::default(),
        }
    }
}

impl<T: Hash + Eq> ParseTable<T> {
    fn add(&mut self, non_terminal_idx: NonTerminalIdx, token: T, production_idx: ProductionIdx) {
        let old = self.table.insert((non_terminal_idx, token), production_idx);
        assert_eq!(old, None);
    }

    /// Same as `add`, except the token is EOF (`$`)
    fn add_end(&mut self, non_terminal_idx: NonTerminalIdx, production_idx: ProductionIdx) {
        let old = self.end.insert(non_terminal_idx, production_idx);
        assert_eq!(old, None);
    }

    fn get(&self, non_terminal_idx: NonTerminalIdx, token: T) -> Option<ProductionIdx> {
        self.table.get(&(non_terminal_idx, token)).copied()
    }

    fn get_end(&self, non_terminal_idx: NonTerminalIdx) -> Option<ProductionIdx> {
        self.end.get(&non_terminal_idx).copied()
    }
}

fn generate_parse_table<T: Hash + Eq + Clone, A>(
    grammar: &Grammar<T, A>,
    first_table: &FirstTable<T>,
    follow_table: &FollowTable<T>,
) -> ParseTable<T> {
    let mut table: ParseTable<T> = Default::default();

    for (non_terminal_idx, production_idx, production) in grammar.production_indices() {
        let mut all_empty = true;
        for symbol in production.symbols() {
            match symbol {
                Symbol::NonTerminal(nt_idx) => {
                    let nt_firsts = first_table.get_first(*nt_idx).unwrap();
                    for terminal in &nt_firsts.terminals {
                        table.add(non_terminal_idx, terminal.clone(), production_idx);
                    }
                    if !nt_firsts.empty {
                        all_empty = false;
                        break;
                    }
                }
                Symbol::Terminal(terminal) => {
                    table.add(non_terminal_idx, terminal.clone(), production_idx);
                    all_empty = false;
                    break;
                }
            }
        }
        if all_empty {
            let nt_follows = follow_table.get_follow(non_terminal_idx).unwrap();
            for terminal in &nt_follows.terminals {
                table.add(non_terminal_idx, terminal.clone(), production_idx);
            }
            if nt_follows.end {
                table.add_end(non_terminal_idx, production_idx);
            }
        }
    }

    table
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ParseError {
    UnexpectedToken { token_index: usize },
    UnexpectedEOF,
}

enum Action<T, A> {
    MatchNonTerminal(NonTerminalIdx),
    MatchTerminal(T),
    RunSemanticAction(fn(&mut Vec<A>) -> A),
}

fn parse<T: Hash + Eq + Clone, A>(
    grammar: &Grammar<T, fn(&mut Vec<A>) -> A>,
    table: &ParseTable<T>,
    input: &mut dyn Iterator<Item = T>,
) -> Result<A, ParseError> {
    let mut action_stack: Vec<Action<T, A>> = vec![Action::MatchNonTerminal(NonTerminalIdx(0))];
    let mut value_stack: Vec<A> = vec![];

    'next_token: for (token_index, token) in input.enumerate() {
        loop {
            match action_stack.pop().unwrap() {
                Action::MatchNonTerminal(non_terminal_idx) => {
                    // TODO: Redundant clone below
                    match table.get(non_terminal_idx, token.clone()) {
                        None => return Err(ParseError::UnexpectedToken { token_index }),
                        Some(production_idx) => {
                            let production =
                                grammar.get_production(non_terminal_idx, production_idx);
                            action_stack.push(Action::RunSemanticAction(production.action));
                            for symbol in production.symbols().iter().rev() {
                                let action: Action<T, A> = match symbol.clone() {
                                    Symbol::NonTerminal(non_terminal_idx) => {
                                        Action::MatchNonTerminal(non_terminal_idx)
                                    }
                                    Symbol::Terminal(terminal) => Action::MatchTerminal(terminal),
                                };
                                action_stack.push(action);
                            }
                        }
                    }
                }
                Action::MatchTerminal(terminal) => {
                    if terminal == token {
                        continue 'next_token;
                    } else {
                        return Err(ParseError::UnexpectedToken { token_index });
                    }
                }
                Action::RunSemanticAction(action) => {
                    let result = action(&mut value_stack);
                    value_stack.push(result);
                    break;
                }
            }
        }
    }

    while let Some(action) = action_stack.pop() {
        match action {
            Action::MatchNonTerminal(non_terminal_idx) => match table.get_end(non_terminal_idx) {
                Some(production_idx) => {
                    assert_eq!(
                        grammar
                            .get_production(non_terminal_idx, production_idx)
                            .symbols()
                            .len(),
                        0
                    );
                }
                None => {
                    return Err(ParseError::UnexpectedEOF);
                }
            },
            Action::MatchTerminal(_) => {
                return Err(ParseError::UnexpectedEOF);
            }
            Action::RunSemanticAction(action) => {
                let result = action(&mut value_stack);
                value_stack.push(result);
            }
        }
    }

    assert_eq!(value_stack.len(), 1);
    Ok(value_stack.pop().unwrap())
}

#[cfg(test)]
mod test {
    use super::*;

    fn get_nonterminal_firsts_sorted(
        table: &FirstTable<char>,
        non_terminal: NonTerminalIdx,
    ) -> Vec<char> {
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

    fn get_nonterminal_follows_sorted(
        table: &FollowTable<char>,
        non_terminal: NonTerminalIdx,
    ) -> Vec<char> {
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

    // Example 4.32 in dragon book
    #[test]
    fn parse_table_5() {
        let grammar = crate::test_grammars::grammar5();
        let first = generate_first_table(&grammar);
        let follow = generate_follow_table(&grammar, &first);
        let parse_table = generate_parse_table(&grammar, &first, &follow);

        let e_nt_idx = grammar.get_non_terminal_idx("E").unwrap();
        let e1_nt_idx = grammar.get_non_terminal_idx("E'").unwrap();
        let t_nt_idx = grammar.get_non_terminal_idx("T").unwrap();
        let t1_nt_idx = grammar.get_non_terminal_idx("T'").unwrap();
        let f_nt_idx = grammar.get_non_terminal_idx("F").unwrap();

        assert_eq!(parse_table.table.len(), 11);
        assert_eq!(parse_table.end.len(), 2);

        assert_eq!(parse_table.get(e_nt_idx, 'n').unwrap(), ProductionIdx(0));
        assert_eq!(parse_table.get(e_nt_idx, '(').unwrap(), ProductionIdx(0));
        assert_eq!(parse_table.get_end(e_nt_idx), None);

        assert_eq!(parse_table.get(e1_nt_idx, '+').unwrap(), ProductionIdx(0));
        assert_eq!(parse_table.get(e1_nt_idx, ')').unwrap(), ProductionIdx(1));
        assert_eq!(parse_table.get_end(e1_nt_idx).unwrap(), ProductionIdx(1));

        assert_eq!(parse_table.get(t_nt_idx, 'n').unwrap(), ProductionIdx(0));
        assert_eq!(parse_table.get(t_nt_idx, '(').unwrap(), ProductionIdx(0));
        assert_eq!(parse_table.get_end(t_nt_idx), None);

        assert_eq!(parse_table.get(t1_nt_idx, '+').unwrap(), ProductionIdx(1));
        assert_eq!(parse_table.get(t1_nt_idx, '*').unwrap(), ProductionIdx(0));
        assert_eq!(parse_table.get(t1_nt_idx, ')').unwrap(), ProductionIdx(1));
        assert_eq!(parse_table.get_end(t1_nt_idx).unwrap(), ProductionIdx(1));

        assert_eq!(parse_table.get(f_nt_idx, 'n').unwrap(), ProductionIdx(1));
        assert_eq!(parse_table.get(f_nt_idx, '(').unwrap(), ProductionIdx(0));
        assert_eq!(parse_table.get_end(f_nt_idx), None);
    }

    // E  -> T E'
    // E' -> + T E' | (empty)
    // T  -> F T'
    // T' -> * F T' | (empty)
    // F -> ( E ) | n
    fn grammar5() -> Grammar<char, fn(&mut Vec<i64>) -> i64> {
        let mut grammar: Grammar<char, fn(&mut Vec<i64>) -> i64> = Grammar::new();
        let e_nt_idx = grammar.add_non_terminal("E".to_owned());
        let e1_nt_idx = grammar.add_non_terminal("E'".to_owned());
        let t_nt_idx = grammar.add_non_terminal("T".to_owned());
        let t1_nt_idx = grammar.add_non_terminal("T'".to_owned());
        let f_nt_idx = grammar.add_non_terminal("F".to_owned());

        grammar.set_init(e_nt_idx);

        // E -> T E'
        grammar.add_production(
            e_nt_idx,
            vec![
                Symbol::NonTerminal(t_nt_idx),
                Symbol::NonTerminal(e1_nt_idx),
            ],
            |vals| todo!(),
        );

        // E' -> + T E'
        grammar.add_production(
            e1_nt_idx,
            vec![
                Symbol::Terminal('+'),
                Symbol::NonTerminal(t_nt_idx),
                Symbol::NonTerminal(e1_nt_idx),
            ],
            |vals| todo!(),
        );

        // E' -> (empty)
        grammar.add_production(e1_nt_idx, vec![], |vals| todo!());

        // T -> F T'
        grammar.add_production(
            t_nt_idx,
            vec![
                Symbol::NonTerminal(f_nt_idx),
                Symbol::NonTerminal(t1_nt_idx),
            ],
            |vals| todo!(),
        );

        // T' -> * F T'
        grammar.add_production(
            t1_nt_idx,
            vec![
                Symbol::Terminal('*'),
                Symbol::NonTerminal(f_nt_idx),
                Symbol::NonTerminal(t1_nt_idx),
            ],
            |vals| todo!(),
        );

        // T' -> (empty)
        grammar.add_production(t1_nt_idx, vec![], |vals| todo!());

        // F -> ( E )
        grammar.add_production(
            f_nt_idx,
            vec![
                Symbol::Terminal('('),
                Symbol::NonTerminal(e_nt_idx),
                Symbol::Terminal(')'),
            ],
            |vals| todo!(),
        );

        // F -> n
        grammar.add_production(f_nt_idx, vec![Symbol::Terminal('n')], |vals| todo!());

        grammar
    }

    // Example 4.35 in dragon book
    #[test]
    fn parse_ll1_5() {
        let grammar = grammar5();
        let first = generate_first_table(&grammar);
        let follow = generate_follow_table(&grammar, &first);
        let parse_table = generate_parse_table(&grammar, &first, &follow);

        assert_eq!(parse(&grammar, &parse_table, &mut "1+2*3".chars()), Ok(7));
    }
}
