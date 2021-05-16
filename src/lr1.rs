use crate::first::{FirstSet, FirstTable};
use crate::grammar::{Grammar, NonTerminalIdx, Production, ProductionIdx, SymbolKind};
use crate::lr_common::{LRTable, LRTableBuilder, StateIdx};

use std::collections::BTreeSet;
use std::hash::Hash;

use fxhash::{FxHashMap, FxHashSet};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
struct LR1Item<T: Clone> {
    non_terminal_idx: NonTerminalIdx,
    production_idx: ProductionIdx,
    cursor: usize,
    // None => EOF
    lookahead: Option<T>,
}

impl<T: Clone> LR1Item<T> {
    #[cfg(test)]
    fn new(
        non_terminal_idx: usize,
        production_idx: usize,
        cursor: usize,
        lookahead: Option<T>,
    ) -> Self {
        Self {
            non_terminal_idx: NonTerminalIdx::from_usize(non_terminal_idx),
            production_idx: ProductionIdx::from_usize(production_idx),
            cursor,
            lookahead,
        }
    }

    fn next_symbol<'grammar, T1, A>(
        &self,
        grammar: &'grammar Grammar<T1, A>,
    ) -> Option<&'grammar SymbolKind<T1>> {
        let production = grammar.get_production(self.non_terminal_idx, self.production_idx);
        production.symbols().get(self.cursor).map(|s| &s.kind)
    }

    /// Returns non-terminal expected by the item, if the next expected symbol is a non-terminal.
    /// Otherwise returns `None`.
    fn next_non_terminal<'grammar, T1, A>(
        &self,
        grammar: &'grammar Grammar<T1, A>,
    ) -> Option<NonTerminalIdx> {
        match self.next_symbol(grammar) {
            Some(SymbolKind::NonTerminal(nt_idx)) => Some(*nt_idx),
            _ => None,
        }
    }

    /// Returns terminal expected by the item, if the next expected symbol is a terminal. Otherwise
    /// returns `None`.
    fn next_terminal<'grammar, A>(&self, grammar: &'grammar Grammar<T, A>) -> Option<&'grammar T> {
        match self.next_symbol(grammar) {
            Some(SymbolKind::Terminal(t)) => Some(t),
            _ => None,
        }
    }

    fn get_production<'grammar, T1, A>(
        &self,
        grammar: &'grammar Grammar<T1, A>,
    ) -> &'grammar Production<T1, A> {
        grammar.get_production(self.non_terminal_idx, self.production_idx)
    }

    fn advance(&self) -> LR1Item<T> {
        let mut item: LR1Item<T> = (*self).clone();
        item.cursor += 1;
        item
    }

    fn is_complete<T1, A>(&self, grammar: &Grammar<T1, A>) -> bool {
        let production = self.get_production(grammar);
        self.cursor == production.symbols().len()
    }
}

fn compute_lr1_closure<T: Ord + Eq + Hash + Clone + std::fmt::Debug, A>(
    grammar: &Grammar<T, A>,
    first_table: &FirstTable<T>,
    items: FxHashSet<LR1Item<T>>,
) -> BTreeSet<LR1Item<T>> {
    let mut closure: FxHashSet<LR1Item<T>> = items;

    let mut work_list: Vec<LR1Item<T>> = closure.iter().cloned().collect();
    while let Some(item) = work_list.pop() {
        if let Some(next) = item.next_non_terminal(grammar) {
            // Need to find the `first` set of the item after `next`. So if the item is
            //
            //     [ X -> ... . B x | t ]
            //
            // `next` is `B`. We need the first set of `x t`, where `x` is whatever's next after
            // `B` (not a single terminal/non-terminal, but the whole rest of the production), and
            // `t` is the item's lookahead token.
            let first = {
                let production = item.get_production(grammar);
                let mut first: FirstSet<T> = Default::default();
                if item.cursor + 1 == production.symbols().len() {
                    // `B` is the last symbol in the production, so the first set is just `t`
                    match &item.lookahead {
                        Some(lookahead) => first.add(lookahead.clone()),
                        None => first.set_empty(),
                    }
                } else {
                    // Otherwise scan through symbols after `B`. The process is the same as follow set
                    // computation
                    let mut end_allowed = true;
                    for symbol in &production.symbols()[item.cursor + 1..] {
                        // println!(
                        //     "Checking symbol {}",
                        //     SymbolKindDisplay {
                        //         symbol: &symbol.kind,
                        //         grammar
                        //     }
                        // );
                        match &symbol.kind {
                            SymbolKind::Terminal(t) => {
                                end_allowed = false;
                                first.add(t.clone());
                            }
                            SymbolKind::NonTerminal(nt) => {
                                let nt_first = first_table.get_first(*nt);
                                for t in nt_first.terminals() {
                                    first.add(t.clone());
                                }
                                if !nt_first.has_empty() {
                                    end_allowed = false;
                                    break;
                                }
                            }
                        }
                    }
                    if end_allowed {
                        match &item.lookahead {
                            Some(lookahead) => first.add(lookahead.clone()),
                            None => first.set_empty(),
                        }
                    }
                }
                first
            };

            // println!(
            //     "LR1 closure item = {}, next = {}, first = {}",
            //     LR1ItemDisplay { item, grammar },
            //     grammar.get_non_terminal(next).non_terminal,
            //     FirstSetDisplay { set: &first }
            // );

            for (production_idx, _) in grammar.non_terminal_production_indices(next) {
                for t in first.terminals() {
                    let item = LR1Item {
                        non_terminal_idx: next,
                        production_idx,
                        cursor: 0,
                        lookahead: Some(t.clone()),
                    };
                    if closure.insert(item.clone()) {
                        work_list.push(item);
                    }
                }
                if first.has_empty() {
                    let item = LR1Item {
                        non_terminal_idx: next,
                        production_idx,
                        cursor: 0,
                        lookahead: None,
                    };
                    if closure.insert(item.clone()) {
                        work_list.push(item);
                    }
                }
            }
        }
    }

    closure.into_iter().collect()
}

fn compute_lr1_goto<T: Hash + Clone + Eq + Ord + std::fmt::Debug, A>(
    state: &BTreeSet<LR1Item<T>>,
    symbol: &SymbolKind<T>,
    grammar: &Grammar<T, A>,
    first: &FirstTable<T>,
) -> BTreeSet<LR1Item<T>> {
    let mut goto: FxHashSet<LR1Item<T>> = Default::default();

    for item in state {
        if let Some(next_symbol) = item.next_symbol(grammar) {
            if next_symbol == symbol {
                goto.insert(item.advance());
            }
        }
    }

    compute_lr1_closure(grammar, first, goto)
}

#[derive(Debug)]
struct LR1State<T: Clone> {
    items: BTreeSet<LR1Item<T>>,
    goto: FxHashMap<SymbolKind<T>, StateIdx>,
}

impl<T: Clone> LR1State<T> {
    fn items(&self) -> impl Iterator<Item = &LR1Item<T>> {
        self.items.iter()
    }

    fn gotos(&self) -> impl Iterator<Item = (&SymbolKind<T>, &StateIdx)> {
        self.goto.iter()
    }
}

#[derive(Debug)]
pub struct LR1Automaton<T: Clone> {
    // Indexed by `StateIdx`
    states: Vec<LR1State<T>>,
}

struct LR1AutomatonStateIndicesIter<'automaton, T: Clone> {
    automaton: &'automaton LR1Automaton<T>,
    idx: StateIdx,
}

impl<'automaton, T: Clone> Iterator for LR1AutomatonStateIndicesIter<'automaton, T> {
    type Item = (StateIdx, &'automaton LR1State<T>);

    fn next(&mut self) -> Option<Self::Item> {
        let idx = self.idx;
        match self.automaton.states.get(self.idx.as_usize()) {
            None => None,
            Some(state) => {
                self.idx = StateIdx(idx.0 + 1);
                Some((idx, state))
            }
        }
    }
}

impl<T: Clone> LR1Automaton<T> {
    fn state_indices(&self) -> impl Iterator<Item = (StateIdx, &LR1State<T>)> {
        LR1AutomatonStateIndicesIter {
            automaton: self,
            idx: StateIdx(0),
        }
    }
}

impl<T: Clone> Default for LR1Automaton<T> {
    fn default() -> Self {
        LR1Automaton { states: vec![] }
    }
}

pub fn generate_lr1_automaton<T, A, F>(
    grammar: &Grammar<T, A>,
    first_table: &FirstTable<T>,
    terminal_iter: F,
) -> (LR1Automaton<T>, FxHashMap<NonTerminalIdx, StateIdx>)
where
    T: Eq + Ord + Hash + Clone + fmt::Debug,
    F: Fn() -> Box<dyn Iterator<Item = T>>,
{
    // Maps existing item sets to their state indices, to maintain sharing.
    let mut state_indices: FxHashMap<BTreeSet<LR1Item<T>>, StateIdx> = Default::default();

    // Maps entry points to their state indices
    let mut non_terminal_state_indices: FxHashMap<NonTerminalIdx, StateIdx> = Default::default();

    let mut automaton: LR1Automaton<T> = Default::default();

    for (non_terminal_idx, non_terminal) in grammar.non_terminal_indices() {
        if non_terminal.public {
            assert_eq!(non_terminal.productions().len(), 1);

            let i0_items: FxHashSet<LR1Item<T>> = hashset! {
                LR1Item {
                    non_terminal_idx,
                    production_idx: ProductionIdx(0),
                    cursor: 0,
                    lookahead: None,
                }
            };

            let i0_items = compute_lr1_closure(grammar, first_table, i0_items);

            let i0_state_idx: StateIdx = StateIdx(automaton.states.len());

            state_indices.insert(i0_items.clone(), i0_state_idx);

            automaton.states.push(LR1State {
                items: i0_items,
                goto: Default::default(),
            });

            non_terminal_state_indices.insert(non_terminal_idx, i0_state_idx);
        }
    }

    let mut updated = true;
    while updated {
        updated = false;

        let mut next_state_idx: StateIdx = StateIdx(automaton.states.len());

        // New states allocated in this iteration
        let mut new_states: Vec<LR1State<T>> = vec![];

        // New GOTOs added in this iteration
        let mut new_gotos: Vec<(StateIdx, SymbolKind<T>, StateIdx)> = Default::default();

        for (state_idx, state) in automaton.state_indices() {
            for symbol in grammar
                .non_terminal_indices()
                .map(|(nt, _)| SymbolKind::NonTerminal(nt))
                .chain(terminal_iter().map(SymbolKind::Terminal))
            {
                let goto = compute_lr1_goto(&state.items, &symbol, grammar, first_table);

                if goto.is_empty() {
                    continue;
                }

                match state_indices.get(&goto) {
                    Some(goto_state_idx) => {
                        new_gotos.push((state_idx, symbol, *goto_state_idx));
                    }
                    None => {
                        let new_state = LR1State {
                            items: goto.clone(),
                            goto: Default::default(),
                        };
                        new_states.push(new_state);
                        state_indices.insert(goto, next_state_idx);
                        new_gotos.push((state_idx, symbol, next_state_idx));
                        next_state_idx = StateIdx(next_state_idx.0 + 1);
                    }
                }
            }
        }

        if !new_states.is_empty() {
            updated = true;
        }

        automaton.states.extend(new_states.into_iter());

        // println!(
        //     "{}",
        //     LR1AutomatonDisplay {
        //         automaton: &automaton,
        //         grammar
        //     }
        // );

        for (state, symbol, next) in new_gotos.into_iter() {
            let old = automaton.states[state.as_usize()].goto.insert(symbol, next);
            updated |= old.is_none();
            // TODO: I don't understand why this doesn't hold.. needs debugging.
            // assert_eq!(old, None, "trying to insert {}", next.0);
        }
    }

    (automaton, non_terminal_state_indices)
}

pub fn build_lr1_table<T: Clone + Eq + Hash + fmt::Debug, A: Clone>(
    grammar: &Grammar<T, A>,
    automaton: &LR1Automaton<T>,
) -> LRTable<T, A> {
    let mut table: LRTableBuilder<T, A> = LRTableBuilder::new(automaton.states.len());

    for (state_idx, state) in automaton.state_indices() {
        for item in state.items() {
            // Rule 2.a
            if let Some(next_terminal) = item.next_terminal(grammar) {
                if let Some(next_state) =
                    state.goto.get(&SymbolKind::Terminal(next_terminal.clone()))
                {
                    table.add_shift(state_idx, next_terminal.clone(), *next_state);
                }
            }

            let non_terminal = grammar.get_non_terminal(item.non_terminal_idx);

            // Rule 2.b
            if item.is_complete(grammar) && !non_terminal.public {
                let production = grammar.get_production(item.non_terminal_idx, item.production_idx);
                table.add_reduce(
                    state_idx,
                    item.lookahead.clone(),
                    item.non_terminal_idx,
                    item.production_idx,
                    production.action.clone(),
                );
            }

            // Rule 2.c
            if non_terminal.public && item.cursor == 1 && item.lookahead.is_none() {
                // `pub` non-terminals are created in lowering, and should have one production to
                // the original `pub` non-terminal
                assert_eq!(non_terminal.productions().len(), 1);
                table.add_accept(state_idx);
            }

            // Add gotos
            for (symbol, next) in state.gotos() {
                if let SymbolKind::NonTerminal(nt) = symbol {
                    table.add_goto(state_idx, *nt, *next);
                }
            }
        }
    }

    table.build()
}

#[cfg(test)]
use crate::lr_common::LRTableDisplay;

use std::fmt;

pub struct LR1AutomatonDisplay<'a, 'b, T: Clone, A> {
    pub automaton: &'a LR1Automaton<T>,
    pub grammar: &'b Grammar<T, A>,
}

struct LR1StateDisplay<'a, 'b, T: Clone, A> {
    state: &'a LR1State<T>,
    grammar: &'b Grammar<T, A>,
}

struct LR1ItemDisplay<'a, 'b, T: Clone, A> {
    item: &'a LR1Item<T>,
    grammar: &'b Grammar<T, A>,
}

impl<'a, 'b, T: Clone + fmt::Debug, A> fmt::Display for LR1ItemDisplay<'a, 'b, T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let non_terminal = self.grammar.get_non_terminal(self.item.non_terminal_idx);
        let production = non_terminal.get_production(self.item.production_idx);

        write!(f, "[{} -> ", &non_terminal.non_terminal)?;

        for (symbol_idx, symbol) in production.symbols().iter().enumerate() {
            if symbol_idx == self.item.cursor {
                write!(f, "|")?;
            }
            write!(
                f,
                "{}",
                crate::grammar::SymbolKindDisplay::new(&symbol.kind, self.grammar),
            )?;
            if symbol_idx != production.symbols().len() - 1 {
                write!(f, " ")?;
            }
        }

        if self.item.cursor == production.symbols().len() {
            write!(f, "|")?;
        }

        write!(f, ", {:?}]", self.item.lookahead)
    }
}

impl<'a, 'b, T: Clone + fmt::Debug, A> fmt::Display for LR1StateDisplay<'a, 'b, T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for item in &self.state.items {
            writeln!(
                f,
                "  {}",
                LR1ItemDisplay {
                    item,
                    grammar: self.grammar
                }
            )?;
        }

        for (symbol, next) in &self.state.goto {
            writeln!(
                f,
                "  GOTO {} -> {}",
                crate::grammar::SymbolKindDisplay::new(symbol, self.grammar),
                next.0
            )?;
        }

        Ok(())
    }
}

impl<'a, 'b, T: Clone + fmt::Debug, A> fmt::Display for LR1AutomatonDisplay<'a, 'b, T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (state_idx, state) in self.automaton.state_indices() {
            writeln!(f, "{}: {{", state_idx.0)?;
            write!(
                f,
                "{}",
                LR1StateDisplay {
                    state,
                    grammar: self.grammar
                }
            )?;
            writeln!(f, "}}")?;
        }

        Ok(())
    }
}

#[test]
fn grammar8_lr1_states() {
    use crate::first::generate_first_table;
    use crate::test_grammars::{grammar8, Grammar8Token};

    let grammar = grammar8();
    let first_table = generate_first_table(&grammar);
    let (lr1_automaton, _) = generate_lr1_automaton(&grammar, &first_table, || {
        Box::new(vec![Grammar8Token::C, Grammar8Token::D].into_iter())
    });

    println!(
        "{}",
        LR1AutomatonDisplay {
            automaton: &lr1_automaton,
            grammar: &grammar
        }
    );

    let i0 = &lr1_automaton.states[0];
    assert_eq!(
        i0.items,
        btreeset! {
            LR1Item::new(0, 0, 0, None), // [S0 -> | S, EOF]
            LR1Item::new(1, 0, 0, None), // [S -> | C C, EOF]
            LR1Item::new(2, 0, 0, Some(Grammar8Token::D)), // [C -> . c C, d]
            LR1Item::new(2, 0, 0, Some(Grammar8Token::C)), // [C -> . c C, c]
            LR1Item::new(2, 1, 0, Some(Grammar8Token::C)), // [C -> . d, c]
            LR1Item::new(2, 1, 0, Some(Grammar8Token::D)), // [C -> . d, c]
        },
    );

    let i1 = &lr1_automaton.states[1];
    assert_eq!(
        i1.items,
        btreeset! {
            LR1Item::new(0, 0, 1, None), // [S0 -> S |, EOF]
        },
    );

    let i2 = &lr1_automaton.states[2];
    assert_eq!(
        i2.items,
        btreeset! {
            LR1Item::new(1, 0, 1, None), // [S -> C | C, EOF]
            LR1Item::new(2, 0, 0, None), // [C -> | c C, EOF]
            LR1Item::new(2, 1, 0, None), // [C -> | d, EOF]
        },
    );

    let i3 = &lr1_automaton.states[3];
    assert_eq!(
        i3.items,
        btreeset! {
            LR1Item::new(2, 0, 1, Some(Grammar8Token::C)), // [C -> c | C, c]
            LR1Item::new(2, 0, 1, Some(Grammar8Token::D)), // [C -> c | C, d]
            LR1Item::new(2, 0, 0, Some(Grammar8Token::C)), // [C -> | c C, c]
            LR1Item::new(2, 0, 0, Some(Grammar8Token::D)), // [C -> | c C, d]
            LR1Item::new(2, 1, 0, Some(Grammar8Token::C)), // [C -> | d, c]
            LR1Item::new(2, 1, 0, Some(Grammar8Token::D)), // [C -> | d, d]
        },
    );

    let i4 = &lr1_automaton.states[4];
    assert_eq!(
        i4.items,
        btreeset! {
            LR1Item::new(2, 1, 1, Some(Grammar8Token::C)), // [C -> d |, c]
            LR1Item::new(2, 1, 1, Some(Grammar8Token::D)), // [C -> d |, c]
        },
    );

    let i5 = &lr1_automaton.states[5];
    assert_eq!(
        i5.items,
        btreeset! {
            LR1Item::new(1, 0, 2, None), // [S -> C C |, EOF]
            LR1Item::new(1, 0, 2, None), // [S -> C C |, EOF]
        },
    );

    // ...

    let i8 = &lr1_automaton.states[8];
    assert_eq!(
        i8.items,
        btreeset! {
            LR1Item::new(2, 0, 2, Some(Grammar8Token::C)), // [C -> c C |, c]
            LR1Item::new(2, 0, 2, Some(Grammar8Token::D)), // [C -> c C |, d]
        },
    );

    let i9 = &lr1_automaton.states[9];
    assert_eq!(
        i9.items,
        btreeset! {
            LR1Item::new(2, 0, 2, None), // [C -> c C |, EOF]
            LR1Item::new(2, 0, 2, None), // [C -> c C |, EOF]
        },
    );
}

#[test]
fn simulate1() {
    use crate::first::generate_first_table;
    use crate::test_grammars::{grammar6, Grammar6Token};

    let grammar = grammar6();
    let first = generate_first_table(&grammar);
    let (lr_automaton, _) = generate_lr1_automaton(&grammar, &first, || {
        Box::new(
            vec![
                Grammar6Token::LParen,
                Grammar6Token::RParen,
                Grammar6Token::Plus,
                Grammar6Token::Star,
                Grammar6Token::Id,
            ]
            .into_iter(),
        )
    });

    println!(
        "{}",
        LR1AutomatonDisplay {
            automaton: &lr_automaton,
            grammar: &grammar
        }
    );

    let lr1 = build_lr1_table(&grammar, &lr_automaton);

    // println!(
    //     "{}",
    //     LR1TableDisplay {
    //         table: &slr,
    //         grammar: &grammar
    //     }
    // );

    crate::lr_common::simulate(
        &lr1,
        &grammar,
        vec![Grammar6Token::Id, Grammar6Token::Plus, Grammar6Token::Id].into_iter(),
    );

    crate::lr_common::simulate(
        &lr1,
        &grammar,
        vec![
            Grammar6Token::Id,
            Grammar6Token::Plus,
            Grammar6Token::Id,
            Grammar6Token::Star,
            Grammar6Token::Id,
        ]
        .into_iter(),
    );

    crate::lr_common::simulate(
        &lr1,
        &grammar,
        vec![
            Grammar6Token::LParen,
            Grammar6Token::Id,
            Grammar6Token::Plus,
            Grammar6Token::Id,
            Grammar6Token::RParen,
            Grammar6Token::Star,
            Grammar6Token::Id,
        ]
        .into_iter(),
    );
}

#[test]
fn simulate2() {
    use crate::first::generate_first_table;
    use crate::test_grammars::{grammar9, Grammar9Token};

    let grammar = grammar9();
    let first = generate_first_table(&grammar);
    let (lr_automaton, _) = generate_lr1_automaton(&grammar, &first, || {
        Box::new(vec![Grammar9Token::LParen, Grammar9Token::RParen].into_iter())
    });

    // println!(
    //     "{}",
    //     LR1AutomatonDisplay {
    //         automaton: &lr_automaton,
    //         grammar: &grammar
    //     }
    // );

    let lr1 = build_lr1_table(&grammar, &lr_automaton);

    println!("{}", LRTableDisplay::new(&lr1, &grammar),);

    crate::lr_common::simulate(&lr1, &grammar, vec![].into_iter());
    crate::lr_common::simulate(
        &lr1,
        &grammar,
        vec![Grammar9Token::LParen, Grammar9Token::RParen].into_iter(),
    );
}

#[test]
fn simulate3() {
    use crate::first::generate_first_table;
    use crate::test_grammars::{grammar7, Grammar7Token};

    let grammar = grammar7();
    let first = generate_first_table(&grammar);
    let (lr_automaton, _) = generate_lr1_automaton(&grammar, &first, || {
        Box::new(vec![Grammar7Token::Eq, Grammar7Token::Star, Grammar7Token::Id].into_iter())
    });
    let lr1 = build_lr1_table(&grammar, &lr_automaton);

    println!("{}", LRTableDisplay::new(&lr1, &grammar),);

    crate::lr_common::simulate(
        &lr1,
        &grammar,
        vec![Grammar7Token::Star, Grammar7Token::Id].into_iter(),
    );
    crate::lr_common::simulate(
        &lr1,
        &grammar,
        vec![Grammar7Token::Id, Grammar7Token::Eq, Grammar7Token::Id].into_iter(),
    );
    crate::lr_common::simulate(
        &lr1,
        &grammar,
        vec![
            Grammar7Token::Id,
            Grammar7Token::Eq,
            Grammar7Token::Star,
            Grammar7Token::Id,
        ]
        .into_iter(),
    );
    crate::lr_common::simulate(
        &lr1,
        &grammar,
        vec![
            Grammar7Token::Star,
            Grammar7Token::Id,
            Grammar7Token::Eq,
            Grammar7Token::Id,
        ]
        .into_iter(),
    );
    crate::lr_common::simulate(
        &lr1,
        &grammar,
        vec![
            Grammar7Token::Star,
            Grammar7Token::Id,
            Grammar7Token::Eq,
            Grammar7Token::Star,
            Grammar7Token::Id,
        ]
        .into_iter(),
    );
}

#[test]
fn simulate4() {
    use crate::first::generate_first_table;
    use crate::test_grammars::grammar5;

    let grammar = grammar5();

    println!("{}", grammar);

    let first = generate_first_table(&grammar);
    let (lr_automaton, _) = generate_lr1_automaton(&grammar, &first, || {
        Box::new(vec!['+', '*', '(', ')', 'n'].into_iter())
    });

    println!(
        "{}",
        LR1AutomatonDisplay {
            automaton: &lr_automaton,
            grammar: &grammar
        }
    );

    let lr1 = build_lr1_table(&grammar, &lr_automaton);

    println!("{}", LRTableDisplay::new(&lr1, &grammar),);

    crate::lr_common::simulate(&lr1, &grammar, vec!['n'].into_iter());
    crate::lr_common::simulate(&lr1, &grammar, vec!['n', '+', 'n'].into_iter());
    crate::lr_common::simulate(&lr1, &grammar, vec!['n', '+', 'n', '*', 'n'].into_iter());
}
