use crate::grammar::{Grammar, NonTerminalIdx, ProductionIdx, Symbol};

use std::collections::hash_map::Entry;
use std::fmt;
use std::hash::Hash;

use fxhash::{FxHashMap, FxHashSet};

trait Token {
    type Kind: Eq + Copy + Hash + fmt::Debug;
    type Value: fmt::Debug;

    fn kind(&self) -> Self::Kind;
    fn extract_value(self) -> Option<Self::Value>;
}

impl Token for char {
    type Kind = char;
    type Value = char;

    fn kind(&self) -> char {
        *self
    }

    fn extract_value(self) -> Option<char> {
        Some(self)
    }
}

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

fn generate_first_table<T: Eq + Hash + Clone, A>(grammar: &Grammar<T, A>) -> FirstTable<T> {
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

fn generate_follow_table<T: Token, A>(
    grammar: &Grammar<T::Kind, A>,
    first_table: &FirstTable<T::Kind>,
) -> FollowTable<T::Kind> {
    let mut table: FollowTable<T::Kind> = Default::default();

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
                let mut current_symbol: Option<Symbol<T::Kind>> = symbol_iter.next();
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

impl<T: Hash + Eq + Clone> ParseTable<T> {
    fn get_expected_tokens(&self, non_terminal_idx: NonTerminalIdx) -> Vec<T> {
        let mut ret = vec![];
        for (non_terminal_idx_, token) in self.table.keys() {
            if *non_terminal_idx_ == non_terminal_idx {
                ret.push(token.clone());
            }
        }
        ret
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

#[derive(Debug, PartialEq, Eq, Clone)]
enum ParseError<T> {
    UnexpectedToken {
        token_index: usize,
        expected: Vec<T>,
    },
    UnexpectedEOF,
}

enum Action<T, A> {
    MatchNonTerminal(NonTerminalIdx),
    MatchTerminal(T),
    RunSemanticAction(fn(&mut Vec<A>)),
}

impl<T: fmt::Debug, A> fmt::Debug for Action<T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Action::MatchNonTerminal(token) => token.fmt(f),
            Action::MatchTerminal(terminal_idx) => terminal_idx.fmt(f),
            Action::RunSemanticAction(_) => "<action>".fmt(f),
        }
    }
}

fn parse<T: Token + fmt::Debug>(
    grammar: &Grammar<T::Kind, fn(&mut Vec<T::Value>)>,
    table: &ParseTable<T::Kind>,
    input: &mut dyn Iterator<Item = T>,
) -> Result<T::Value, ParseError<T::Kind>> {
    let mut action_stack: Vec<Action<T::Kind, T::Value>> =
        vec![Action::MatchNonTerminal(NonTerminalIdx(0))];
    let mut value_stack: Vec<T::Value> = vec![];

    'next_token: for (token_index, token) in input.enumerate() {
        // println!(
        //     "token={:?}, action_stack={:?}, value_stack={:?}",
        //     token, action_stack, value_stack
        // );

        loop {
            match action_stack.pop().unwrap() {
                Action::MatchNonTerminal(non_terminal_idx) => {
                    // TODO: Redundant clone below
                    match table.get(non_terminal_idx, token.kind()) {
                        None => {
                            return Err(ParseError::UnexpectedToken {
                                token_index,
                                expected: table.get_expected_tokens(non_terminal_idx),
                            })
                        }
                        Some(production_idx) => {
                            let production =
                                grammar.get_production(non_terminal_idx, production_idx);
                            action_stack.push(Action::RunSemanticAction(production.action));
                            for symbol in production.symbols().iter().rev() {
                                let action: Action<T::Kind, T::Value> = match symbol.clone() {
                                    Symbol::NonTerminal(non_terminal_idx) => {
                                        Action::MatchNonTerminal(non_terminal_idx)
                                    }
                                    Symbol::Terminal(terminal) => Action::MatchTerminal(terminal),
                                };
                                action_stack.push(action);
                            }
                        }
                    }
                    // println!(
                    //     "action stack after expanding non-terminal={:?}",
                    //     action_stack
                    // );
                }
                Action::MatchTerminal(terminal) => {
                    if terminal == token.kind() {
                        if let Some(value) = token.extract_value() {
                            value_stack.push(value);
                        }
                        continue 'next_token;
                    } else {
                        return Err(ParseError::UnexpectedToken {
                            token_index,
                            expected: vec![terminal],
                        });
                    }
                }
                Action::RunSemanticAction(action) => {
                    action(&mut value_stack);
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
                action(&mut value_stack);
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
        let grammar: Grammar<char, ()> = crate::test_grammars::grammar5();
        let first = generate_first_table(&grammar);
        let follow: FollowTable<char> = generate_follow_table::<char, ()>(&grammar, &first);

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
        let grammar: Grammar<char, ()> = crate::test_grammars::grammar5();
        let first = generate_first_table(&grammar);
        let follow: FollowTable<char> = generate_follow_table::<char, ()>(&grammar, &first);
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

    mod grammar5 {
        use super::*;
        use crate::grammar::*;

        #[derive(Debug)]
        enum Token {
            Plus,
            Star,
            LParen,
            RParen,
            Int(i64),
        }

        #[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
        enum TokenKind {
            Plus,
            Star,
            LParen,
            RParen,
            Int,
        }

        impl super::Token for Token {
            type Kind = TokenKind;
            type Value = ActionResult;

            fn kind(&self) -> TokenKind {
                use TokenKind::*;
                match self {
                    Token::Plus => Plus,
                    Token::Star => Star,
                    Token::LParen => LParen,
                    Token::RParen => RParen,
                    Token::Int(_) => Int,
                }
            }

            fn extract_value(self) -> Option<ActionResult> {
                match self {
                    Token::Plus | Token::Star | Token::LParen | Token::RParen => None,
                    Token::Int(i) => Some(ActionResult::Token(i)),
                }
            }
        }

        #[derive(Debug, PartialEq, Eq)]
        struct E(T, E1);

        #[derive(Debug, PartialEq, Eq)]
        enum E1 {
            Empty,
            Plus(T, Box<E1>),
        }

        #[derive(Debug, PartialEq, Eq)]
        struct T(F, T1);

        #[derive(Debug, PartialEq, Eq)]
        enum T1 {
            Empty,
            Star(F, Box<T1>),
        }

        #[derive(Debug, PartialEq, Eq)]
        enum F {
            Int(i64),
            Paren(Box<E>),
        }

        #[derive(Debug, PartialEq, Eq)]
        enum ActionResult {
            E(E),
            E1(E1),
            T(T),
            T1(T1),
            F(F),
            Token(i64),
        }

        // E  -> T E'
        // E' -> + T E' | (empty)
        // T  -> F T'
        // T' -> * F T' | (empty)
        // F -> ( E ) | n
        fn grammar5() -> Grammar<TokenKind, fn(&mut Vec<ActionResult>)> {
            let mut grammar: Grammar<TokenKind, fn(&mut Vec<ActionResult>)> = Grammar::new();
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
                |vals| {
                    println!("vals={:?}", vals);
                    let e1 = match vals.pop().unwrap() {
                        ActionResult::E1(e1) => e1,
                        other => panic!("Unexpected value: {:?}", other),
                    };
                    let t = match vals.pop().unwrap() {
                        ActionResult::T(t) => t,
                        other => panic!("Unexpected value: {:?}", other),
                    };
                    vals.push(ActionResult::E(E(t, e1)));
                    println!("vals after={:?}", vals);
                },
            );

            // E' -> + T E'
            grammar.add_production(
                e1_nt_idx,
                vec![
                    Symbol::Terminal(TokenKind::Plus),
                    Symbol::NonTerminal(t_nt_idx),
                    Symbol::NonTerminal(e1_nt_idx),
                ],
                |vals| {
                    println!("vals={:?}", vals);
                    let e1 = match vals.pop().unwrap() {
                        ActionResult::E1(e1) => e1,
                        other => panic!("Unexpected value: {:?}", other),
                    };
                    let t = match vals.pop().unwrap() {
                        ActionResult::T(t) => t,
                        other => panic!("Unexpected value: {:?}", other),
                    };
                    vals.push(ActionResult::E1(E1::Plus(t, Box::new(e1))));
                    println!("vals after={:?}", vals);
                },
            );

            // E' -> (empty)
            grammar.add_production(e1_nt_idx, vec![], |vals| {
                println!("vals={:?}", vals);
                vals.push(ActionResult::E1(E1::Empty));
                println!("vals after={:?}", vals);
            });

            // T -> F T'
            grammar.add_production(
                t_nt_idx,
                vec![
                    Symbol::NonTerminal(f_nt_idx),
                    Symbol::NonTerminal(t1_nt_idx),
                ],
                |vals| {
                    println!("vals={:?}", vals);
                    let t1 = match vals.pop().unwrap() {
                        ActionResult::T1(t1) => t1,
                        other => panic!("Unexpected value: {:?}", other),
                    };
                    let f = match vals.pop().unwrap() {
                        ActionResult::F(f) => f,
                        other => panic!("Unexpected value: {:?}", other),
                    };
                    vals.push(ActionResult::T(T(f, t1)));
                    println!("vals after={:?}", vals);
                },
            );

            // T' -> * F T'
            grammar.add_production(
                t1_nt_idx,
                vec![
                    Symbol::Terminal(TokenKind::Star),
                    Symbol::NonTerminal(f_nt_idx),
                    Symbol::NonTerminal(t1_nt_idx),
                ],
                |vals| {
                    println!("vals={:?}", vals);
                    let t1 = match vals.pop().unwrap() {
                        ActionResult::T1(t1) => t1,
                        other => panic!("Unexpected value: {:?}", other),
                    };
                    let f = match vals.pop().unwrap() {
                        ActionResult::F(f) => f,
                        other => panic!("Unexpected value: {:?}", other),
                    };
                    vals.push(ActionResult::T1(T1::Star(f, Box::new(t1))));
                    println!("vals after={:?}", vals);
                },
            );

            // T' -> (empty)
            grammar.add_production(t1_nt_idx, vec![], |vals| {
                println!("vals={:?}", vals);
                vals.push(ActionResult::T1(T1::Empty));
                println!("vals after={:?}", vals);
            });

            // F -> ( E )
            grammar.add_production(
                f_nt_idx,
                vec![
                    Symbol::Terminal(TokenKind::LParen),
                    Symbol::NonTerminal(e_nt_idx),
                    Symbol::Terminal(TokenKind::RParen),
                ],
                |vals| {
                    println!("vals={:?}", vals);
                    let e = match vals.pop().unwrap() {
                        ActionResult::E(e) => e,
                        other => panic!("Unexpected value: {:?}", other),
                    };
                    vals.push(ActionResult::F(F::Paren(Box::new(e))));
                    println!("vals after={:?}", vals);
                },
            );

            // F -> n
            grammar.add_production(f_nt_idx, vec![Symbol::Terminal(TokenKind::Int)], |vals| {
                println!("vals={:?}", vals);
                let n = match vals.pop().unwrap() {
                    ActionResult::Token(n) => n,
                    other => panic!("Unexpected value: {:?}", other),
                };
                vals.push(ActionResult::F(F::Int(n)));
                println!("vals after={:?}", vals);
            });

            grammar
        }

        // Example 4.35 in dragon book
        #[test]
        fn parse_ll1_5() {
            let grammar: Grammar<TokenKind, _> = grammar5();
            let first = generate_first_table(&grammar);
            let follow = generate_follow_table::<Token, _>(&grammar, &first);
            let parse_table = generate_parse_table(&grammar, &first, &follow);

            let tokens = vec![
                Token::Int(1),
                Token::Plus,
                Token::Int(2),
                Token::Star,
                Token::Int(3),
            ];
            assert!(parse(&grammar, &parse_table, &mut tokens.into_iter()).is_ok());
        }
    }
}
