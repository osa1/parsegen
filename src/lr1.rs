use crate::first::{FirstSet, FirstTable};
use crate::grammar::{Grammar, NonTerminalIdx, Production, ProductionIdx, SymbolKind};

use std::collections::BTreeSet;
use std::hash::Hash;

use fxhash::FxHashMap;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
struct LR1Item<T: Clone> {
    non_terminal_idx: NonTerminalIdx,
    production_idx: ProductionIdx,
    cursor: usize,
    // None => EOF
    lookahead: Option<T>,
}

impl<T: Clone> LR1Item<T> {
    fn next_symbol<'grammar, T1, A>(
        &self,
        grammar: &'grammar Grammar<T1, A>,
    ) -> Option<&'grammar SymbolKind<T1>> {
        let production = grammar.get_production(self.non_terminal_idx, self.production_idx);
        production.symbols().get(self.cursor).map(|s| &s.kind)
    }

    fn next_non_terminal<'grammar, T1, A>(
        &self,
        grammar: &'grammar Grammar<T1, A>,
    ) -> Option<NonTerminalIdx> {
        match self.next_symbol(grammar) {
            Some(SymbolKind::NonTerminal(nt_idx)) => Some(*nt_idx),
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

    fn try_advance<T1, A>(&self, grammar: &Grammar<T1, A>) -> Option<LR1Item<T>> {
        let production = self.get_production(grammar);
        if self.cursor == production.symbols().len() {
            None
        } else {
            Some(self.advance())
        }
    }
}

fn compute_lr1_closure<T: Ord + Eq + Hash + Clone, A>(
    grammar: &Grammar<T, A>,
    first_table: &FirstTable<T>,
    items: &BTreeSet<LR1Item<T>>,
) -> BTreeSet<LR1Item<T>> {
    let mut closure: BTreeSet<LR1Item<T>> = items.clone();

    let mut updated = true;
    while updated {
        updated = false;

        for item in items {
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
                        if let Some(lookahead) = &item.lookahead {
                            first.add(lookahead.clone());
                        }
                    } else {
                        // Otherwise scan through symbols after `B`. The process is the same as follow set
                        // computation
                        let mut end_allowed = true;
                        for symbol in &production.symbols()[item.cursor + 1..] {
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
                            if let Some(lookahead) = &item.lookahead {
                                first.add(lookahead.clone());
                            }
                        }
                    }
                    first
                };

                for (production_idx, _) in grammar.non_terminal_production_indices(next) {
                    for t in first.terminals() {
                        updated |= closure.insert(LR1Item {
                            non_terminal_idx: next,
                            production_idx,
                            cursor: 0,
                            lookahead: Some(t.clone()),
                        });
                    }
                    if first.has_empty() {
                        updated |= closure.insert(LR1Item {
                            non_terminal_idx: next,
                            production_idx,
                            cursor: 0,
                            lookahead: None,
                        });
                    }
                }
            }
        }
    }

    closure
}

fn compute_lr1_goto<T: Hash + Clone + Eq + Ord, A>(
    state: &BTreeSet<LR1Item<T>>,
    symbol: &SymbolKind<T>,
    grammar: &Grammar<T, A>,
    first: &FirstTable<T>,
) -> BTreeSet<LR1Item<T>> {
    let mut goto: BTreeSet<LR1Item<T>> = Default::default();

    for item in state {
        if let Some(next_symbol) = item.next_symbol(grammar) {
            if next_symbol == symbol {
                goto.insert(item.advance());
            }
        }
    }

    compute_lr1_closure(grammar, first, &goto)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct StateIdx(usize);

impl StateIdx {
    fn as_usize(&self) -> usize {
        self.0
    }
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
}

#[derive(Debug)]
struct LR1Automaton<T: Clone> {
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

fn compute_lr1_states<T: Ord + Clone + Hash, A>(
    grammar: &Grammar<T, A>,
    first_table: &FirstTable<T>,
) -> LR1Automaton<T> {
    // Maps existing item sets to their state indices, to maintain sharing.
    let mut state_indices: FxHashMap<BTreeSet<LR1Item<T>>, StateIdx> = Default::default();

    let mut automaton: LR1Automaton<T> = Default::default();

    {
        let i0_items: BTreeSet<LR1Item<T>> = btreeset! {
            LR1Item {
                non_terminal_idx: NonTerminalIdx(0),
                production_idx: ProductionIdx(0),
                cursor: 0,
                lookahead: None,
            }
        };

        let i0_items = compute_lr1_closure(grammar, first_table, &i0_items);

        state_indices.insert(i0_items.clone(), StateIdx(0));

        automaton.states.push(LR1State {
            items: i0_items,
            goto: Default::default(),
        });
    }

    let mut updated = true;
    while updated {
        updated = false;

        let mut state_indices_: FxHashMap<BTreeSet<LR1Item<T>>, StateIdx> = state_indices.clone();
        let mut new_state_idx: StateIdx = StateIdx(automaton.states.len());
        let mut new_states: Vec<LR1State<T>> = vec![];
        let mut new_gotos: FxHashMap<(StateIdx, SymbolKind<T>), StateIdx> = Default::default();

        for (state_idx, state) in automaton.state_indices() {
            // The book iterates all grammar symbols here for the GOTOs of the state, however that
            // requires being able to iterate all terminals.. which I don't want to do. Instead
            // what we do is we look at symbols at the right side of dots in the items, and compute
            // GOTOs of those.

            for item in state.items() {
                if let Some(next_symbol) = item.next_symbol(grammar) {
                    let goto = compute_lr1_goto(&state.items, next_symbol, grammar, first_table);
                    if !goto.is_empty() {
                        match state_indices_.get(&goto) {
                            Some(goto_idx) => {
                                updated |= new_gotos
                                    .insert((state_idx, next_symbol.clone()), *goto_idx)
                                    .is_some();
                            }
                            None => {
                                updated = true;
                                new_states.push(LR1State {
                                    items: goto.clone(),
                                    goto: Default::default(),
                                });
                                state_indices_.insert(goto.clone(), new_state_idx);
                                new_gotos.insert((state_idx, next_symbol.clone()), new_state_idx);
                                new_state_idx = StateIdx(new_state_idx.0 + 1);
                            }
                        }
                    }
                }
            }
        }

        automaton.states.extend(new_states.into_iter());
        for ((state, symbol), next) in new_gotos.into_iter() {
            let old = automaton.states[state.as_usize()].goto.insert(symbol, next);
            assert_eq!(old, None);
        }
    }

    automaton
}
