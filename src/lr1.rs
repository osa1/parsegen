use crate::first::{FirstSet, FirstTable};
use crate::grammar::{Grammar, NonTerminalIdx, Production, ProductionIdx, SymbolKind, TerminalIdx};
use crate::lr_common::{LRTable, LRTableBuilder, StateIdx};

use std::collections::BTreeSet;
use std::hash::Hash;

use fxhash::{FxHashMap, FxHashSet};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
struct LR1Item {
    non_terminal_idx: NonTerminalIdx,
    production_idx: ProductionIdx,
    cursor: usize,
    // None => EOF
    lookahead: Option<TerminalIdx>,
}

impl LR1Item {
    #[cfg(test)]
    fn new(
        non_terminal_idx: usize,
        production_idx: usize,
        cursor: usize,
        lookahead: Option<TerminalIdx>,
    ) -> Self {
        Self {
            non_terminal_idx: NonTerminalIdx::from_usize(non_terminal_idx),
            production_idx: ProductionIdx::from_usize(production_idx),
            cursor,
            lookahead,
        }
    }

    fn next_symbol<'grammar, A>(
        &self,
        grammar: &'grammar Grammar<A>,
    ) -> Option<&'grammar SymbolKind> {
        let production = grammar.get_production(self.non_terminal_idx, self.production_idx);
        production.symbols().get(self.cursor).map(|s| &s.kind)
    }

    /// Returns non-terminal expected by the item, if the next expected symbol is a non-terminal.
    /// Otherwise returns `None`.
    fn next_non_terminal<A>(&self, grammar: &Grammar<A>) -> Option<NonTerminalIdx> {
        match self.next_symbol(grammar) {
            Some(SymbolKind::NonTerminal(nt_idx)) => Some(*nt_idx),
            _ => None,
        }
    }

    /// Returns terminal expected by the item, if the next expected symbol is a terminal. Otherwise
    /// returns `None`.
    fn next_terminal<A>(&self, grammar: &Grammar<A>) -> Option<TerminalIdx> {
        match self.next_symbol(grammar) {
            Some(SymbolKind::Terminal(t)) => Some(*t),
            _ => None,
        }
    }

    fn get_production<'grammar, A>(
        &self,
        grammar: &'grammar Grammar<A>,
    ) -> &'grammar Production<A> {
        grammar.get_production(self.non_terminal_idx, self.production_idx)
    }

    fn advance(&self) -> LR1Item {
        let mut item: LR1Item = (*self).clone();
        item.cursor += 1;
        item
    }

    fn is_complete<A>(&self, grammar: &Grammar<A>) -> bool {
        let production = self.get_production(grammar);
        self.cursor == production.symbols().len()
    }
}

fn compute_lr1_closure<A>(
    grammar: &Grammar<A>,
    first_table: &FirstTable,
    items: FxHashSet<LR1Item>,
) -> BTreeSet<LR1Item> {
    let mut closure: FxHashSet<LR1Item> = items;

    let mut work_list: Vec<LR1Item> = closure.iter().cloned().collect();
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
                let mut first: FirstSet = Default::default();
                if item.cursor + 1 == production.symbols().len() {
                    // `B` is the last symbol in the production, so the first set is just `t`
                    match &item.lookahead {
                        Some(lookahead) => first.add(*lookahead),
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
                                first.add(*t);
                                break;
                            }
                            SymbolKind::NonTerminal(nt) => {
                                let nt_first = first_table.get_first(*nt);
                                for t in nt_first.terminals() {
                                    first.add(*t);
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
                            Some(lookahead) => first.add(*lookahead),
                            None => first.set_empty(),
                        }
                    }
                }
                first
            };

            // println!(
            //     "LR1 closure item = {}, next = {}, first = {}",
            //     LR1ItemDisplay { item: &item, grammar },
            //     grammar.get_non_terminal(next).non_terminal,
            //     crate::first::FirstSetDisplay { set: &first, grammar }
            // );

            for (production_idx, _) in grammar.non_terminal_production_indices(next) {
                for t in first.terminals() {
                    let item = LR1Item {
                        non_terminal_idx: next,
                        production_idx,
                        cursor: 0,
                        lookahead: Some(*t),
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

fn compute_lr1_goto<A>(
    state: &BTreeSet<LR1Item>,
    symbol: &SymbolKind,
    grammar: &Grammar<A>,
    first: &FirstTable,
) -> BTreeSet<LR1Item> {
    let mut goto: FxHashSet<LR1Item> = Default::default();

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
struct LR1State {
    items: BTreeSet<LR1Item>,
    goto: FxHashMap<SymbolKind, StateIdx>,
}

impl LR1State {
    fn items(&self) -> impl Iterator<Item = &LR1Item> {
        self.items.iter()
    }

    fn gotos(&self) -> impl Iterator<Item = (&SymbolKind, &StateIdx)> {
        self.goto.iter()
    }
}

#[derive(Debug)]
pub struct LR1Automaton {
    // Indexed by `StateIdx`
    states: Vec<LR1State>,
}

struct LR1AutomatonStateIndicesIter<'automaton> {
    automaton: &'automaton LR1Automaton,
    idx: StateIdx,
}

impl<'automaton> Iterator for LR1AutomatonStateIndicesIter<'automaton> {
    type Item = (StateIdx, &'automaton LR1State);

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

impl LR1Automaton {
    fn state_indices(&self) -> impl Iterator<Item = (StateIdx, &LR1State)> {
        LR1AutomatonStateIndicesIter {
            automaton: self,
            idx: StateIdx(0),
        }
    }
}

impl Default for LR1Automaton {
    fn default() -> Self {
        LR1Automaton { states: vec![] }
    }
}

pub fn generate_lr1_automaton<A>(
    grammar: &Grammar<A>,
    first_table: &FirstTable,
) -> (LR1Automaton, FxHashMap<NonTerminalIdx, StateIdx>) {
    // Maps existing item sets to their state indices, to maintain sharing.
    let mut state_indices: FxHashMap<BTreeSet<LR1Item>, StateIdx> = Default::default();

    // Maps entry points to their state indices
    let mut non_terminal_state_indices: FxHashMap<NonTerminalIdx, StateIdx> = Default::default();

    let mut automaton: LR1Automaton = Default::default();

    for (non_terminal_idx, non_terminal) in grammar.non_terminal_indices() {
        if non_terminal.public {
            assert_eq!(non_terminal.productions().len(), 1);

            let i0_items: FxHashSet<LR1Item> = hashset! {
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
        let mut new_states: Vec<LR1State> = vec![];

        // New GOTOs added in this iteration
        let mut new_gotos: Vec<(StateIdx, SymbolKind, StateIdx)> = Default::default();

        for (state_idx, state) in automaton.state_indices() {
            for symbol in grammar
                .non_terminal_indices()
                .map(|(nt, _)| SymbolKind::NonTerminal(nt))
                .chain(grammar.terminal_indices().map(SymbolKind::Terminal))
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

pub fn build_lr1_table<A: Clone + fmt::Debug + Eq>(
    grammar: &Grammar<A>,
    automaton: &LR1Automaton,
) -> LRTable<A> {
    let mut table: LRTableBuilder<A> = LRTableBuilder::new(automaton.states.len());

    for (state_idx, state) in automaton.state_indices() {
        for item in state.items() {
            // Rule 2.a
            if let Some(next_terminal) = item.next_terminal(grammar) {
                if let Some(next_state) = state.goto.get(&SymbolKind::Terminal(next_terminal)) {
                    table.add_shift(grammar, state_idx, next_terminal, *next_state);
                }
            }

            let non_terminal = grammar.get_non_terminal(item.non_terminal_idx);

            // Rule 2.b
            if item.is_complete(grammar) && !non_terminal.public {
                let production = grammar.get_production(item.non_terminal_idx, item.production_idx);
                table.add_reduce(
                    grammar,
                    state_idx,
                    item.lookahead,
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

pub struct LR1AutomatonDisplay<'a, 'b, A> {
    pub automaton: &'a LR1Automaton,
    pub grammar: &'b Grammar<A>,
}

struct LR1StateDisplay<'a, 'b, A> {
    state: &'a LR1State,
    grammar: &'b Grammar<A>,
}

struct LR1ItemDisplay<'a, 'b, A> {
    item: &'a LR1Item,
    grammar: &'b Grammar<A>,
}

impl<'a, 'b, A> fmt::Display for LR1ItemDisplay<'a, 'b, A> {
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

        write!(
            f,
            ", {:?}]",
            self.item.lookahead.map(|t| self.grammar.get_terminal(t))
        )
    }
}

impl<'a, 'b, A> fmt::Display for LR1StateDisplay<'a, 'b, A> {
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

impl<'a, 'b, A> fmt::Display for LR1AutomatonDisplay<'a, 'b, A> {
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
    use crate::test_grammars::grammar8;

    let test_grammar = grammar8();
    let grammar = test_grammar.get_grammar();
    let first_table = generate_first_table(grammar);
    let (lr1_automaton, _) = generate_lr1_automaton(grammar, &first_table);

    println!(
        "{}",
        LR1AutomatonDisplay {
            automaton: &lr1_automaton,
            grammar: &grammar
        }
    );

    let d = test_grammar.t("d");
    let c = test_grammar.t("c");

    let i0 = &lr1_automaton.states[0];
    assert_eq!(
        i0.items,
        btreeset! {
            LR1Item::new(0, 0, 0, None), // [S0 -> | S, EOF]
            LR1Item::new(1, 0, 0, None), // [S -> | C C, EOF]
            LR1Item::new(2, 0, 0, Some(c)), // [C -> . c C, d]
            LR1Item::new(2, 0, 0, Some(d)), // [C -> . c C, c]
            LR1Item::new(2, 1, 0, Some(c)), // [C -> . d, c]
            LR1Item::new(2, 1, 0, Some(d)), // [C -> . d, c]
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
            LR1Item::new(2, 0, 1, Some(c)), // [C -> c | C, c]
            LR1Item::new(2, 0, 1, Some(d)), // [C -> c | C, d]
            LR1Item::new(2, 0, 0, Some(c)), // [C -> | c C, c]
            LR1Item::new(2, 0, 0, Some(d)), // [C -> | c C, d]
            LR1Item::new(2, 1, 0, Some(c)), // [C -> | d, c]
            LR1Item::new(2, 1, 0, Some(d)), // [C -> | d, d]
        },
    );

    let i4 = &lr1_automaton.states[4];
    assert_eq!(
        i4.items,
        btreeset! {
            LR1Item::new(2, 1, 1, Some(c)), // [C -> d |, c]
            LR1Item::new(2, 1, 1, Some(d)), // [C -> d |, c]
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
            LR1Item::new(2, 0, 2, Some(c)), // [C -> c C |, c]
            LR1Item::new(2, 0, 2, Some(d)), // [C -> c C |, d]
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
    use crate::test_grammars::grammar6;

    let test_grammar = grammar6();
    let grammar = test_grammar.get_grammar();
    let first = generate_first_table(grammar);
    let (lr_automaton, _) = generate_lr1_automaton(grammar, &first);

    println!(
        "{}",
        LR1AutomatonDisplay {
            automaton: &lr_automaton,
            grammar: &grammar
        }
    );

    let lr1 = build_lr1_table(&grammar, &lr_automaton);

    let id = test_grammar.t("id");
    let plus = test_grammar.t("+");
    let star = test_grammar.t("*");
    let lparen = test_grammar.t("(");
    let rparen = test_grammar.t(")");

    // println!(
    //     "{}",
    //     LR1TableDisplay {
    //         table: &slr,
    //         grammar: &grammar
    //     }
    // );

    crate::lr_common::simulate(&lr1, &grammar, vec![id, plus, id].into_iter());

    crate::lr_common::simulate(&lr1, &grammar, vec![id, plus, id, star, id].into_iter());

    crate::lr_common::simulate(
        &lr1,
        &grammar,
        vec![lparen, id, plus, id, rparen, star, id].into_iter(),
    );
}

#[test]
fn simulate2() {
    use crate::first::generate_first_table;
    use crate::test_grammars::grammar9;

    let test_grammar = grammar9();
    let grammar = test_grammar.get_grammar();
    let first = generate_first_table(grammar);
    let (lr_automaton, _) = generate_lr1_automaton(&grammar, &first);

    // println!(
    //     "{}",
    //     LR1AutomatonDisplay {
    //         automaton: &lr_automaton,
    //         grammar: &grammar
    //     }
    // );

    let lr1 = build_lr1_table(&grammar, &lr_automaton);

    println!("{}", LRTableDisplay::new(&lr1, &grammar),);

    let lparen = test_grammar.t("(");
    let rparen = test_grammar.t(")");

    crate::lr_common::simulate(&lr1, &grammar, vec![].into_iter());
    crate::lr_common::simulate(&lr1, &grammar, vec![lparen, rparen].into_iter());
}

#[test]
fn simulate3() {
    use crate::first::generate_first_table;
    use crate::test_grammars::grammar7;

    let test_grammar = grammar7();
    let grammar = test_grammar.get_grammar();
    let first = generate_first_table(&grammar);
    let (lr_automaton, _) = generate_lr1_automaton(&grammar, &first);
    let lr1 = build_lr1_table(&grammar, &lr_automaton);

    println!("{}", LRTableDisplay::new(&lr1, &grammar),);

    let star = test_grammar.t("*");
    let id = test_grammar.t("id");
    let eq = test_grammar.t("=");

    crate::lr_common::simulate(&lr1, &grammar, vec![star, id].into_iter());
    crate::lr_common::simulate(&lr1, &grammar, vec![id, eq, id].into_iter());
    crate::lr_common::simulate(&lr1, &grammar, vec![id, eq, star, id].into_iter());
    crate::lr_common::simulate(&lr1, &grammar, vec![star, id, eq, id].into_iter());
    crate::lr_common::simulate(&lr1, &grammar, vec![star, id, eq, star, id].into_iter());
}

#[test]
fn simulate4() {
    use crate::first::generate_first_table;
    use crate::test_grammars::grammar5;

    let test_grammar = grammar5();
    let grammar = test_grammar.get_grammar();

    println!("{}", grammar);

    let first = generate_first_table(&grammar);
    let (lr_automaton, _) = generate_lr1_automaton(&grammar, &first);

    println!(
        "{}",
        LR1AutomatonDisplay {
            automaton: &lr_automaton,
            grammar: &grammar
        }
    );

    let lr1 = build_lr1_table(&grammar, &lr_automaton);

    println!("{}", LRTableDisplay::new(&lr1, &grammar));

    let n = test_grammar.t("n");
    let plus = test_grammar.t("+");
    let star = test_grammar.t("*");
    // let lparen = test_grammar.t("(");
    // let rparen = test_grammar.t(")");

    crate::lr_common::simulate(&lr1, &grammar, vec![n].into_iter());
    crate::lr_common::simulate(&lr1, &grammar, vec![n, plus, n].into_iter());
    crate::lr_common::simulate(&lr1, &grammar, vec![n, plus, n, star, n].into_iter());
}
