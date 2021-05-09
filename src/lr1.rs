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

#[derive(Debug)]
struct LR1State<T: Clone> {
    items: BTreeSet<LR1Item<T>>,
    goto: FxHashMap<SymbolKind<T>, StateIdx>,
}

#[derive(Debug)]
struct LR1Automaton<T: Clone> {
    // Indexed by `StateIdx`
    states: Vec<LR1State<T>>,
    // Maps existing item sets to their state indices, to maintain sharing.
    state_indices: FxHashMap<BTreeSet<LR1Item<T>>, StateIdx>,
}

fn compute_lr1_states<T: Clone, A>(grammar: &Grammar<T, A>) -> LR1Automaton<T> {
    todo!()
}
