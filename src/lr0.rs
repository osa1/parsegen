use crate::grammar::{Grammar, NonTerminalIdx, ProductionIdx, SymbolKind};

use std::collections::BTreeSet;

use fxhash::{FxHashMap, FxHashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Item {
    non_terminal_idx: NonTerminalIdx,
    production_idx: ProductionIdx,
    cursor: usize,
}

impl Item {
    fn new(non_terminal_idx: NonTerminalIdx, production_idx: ProductionIdx) -> Item {
        Item {
            non_terminal_idx,
            production_idx,
            cursor: 0,
        }
    }

    fn new_with_cursor(
        non_terminal_idx: NonTerminalIdx,
        production_idx: ProductionIdx,
        cursor: usize,
    ) -> Item {
        Item {
            non_terminal_idx,
            production_idx,
            cursor,
        }
    }

    fn next_symbol<'grammar, T, A>(
        &self,
        grammar: &'grammar Grammar<T, A>,
    ) -> Option<&'grammar SymbolKind<T>> {
        let production = grammar.get_production(self.non_terminal_idx, self.production_idx);
        let symbols = production.symbols();
        symbols.get(self.cursor).map(|s| &s.kind)
    }

    fn next_non_terminal<T, A>(&self, grammar: &Grammar<T, A>) -> Option<NonTerminalIdx> {
        match self.next_symbol(grammar) {
            Some(SymbolKind::NonTerminal(nt_idx)) => Some(*nt_idx),
            _ => None,
        }
    }

    fn advance(&self) -> Item {
        Item {
            non_terminal_idx: self.non_terminal_idx,
            production_idx: self.production_idx,
            cursor: self.cursor + 1,
        }
    }
}

fn compute_closure<T, A>(grammar: &Grammar<T, A>, items: &BTreeSet<Item>) -> BTreeSet<Item> {
    let mut closure: BTreeSet<Item> = items.clone();

    let mut updated = true;
    while updated {
        updated = false;

        let mut new_items: FxHashSet<Item> = Default::default();

        for item in &closure {
            if let Some(next_nt) = item.next_non_terminal(grammar) {
                let nt = grammar.get_non_terminal(next_nt);
                for prod_idx in nt.production_indices() {
                    let item = Item::new(next_nt, prod_idx);
                    if !closure.contains(&item) {
                        updated |= new_items.insert(item);
                    }
                }
            }
        }

        closure.extend(new_items.into_iter());
    }

    closure
}

fn compute_goto<T: Eq, A>(
    grammar: &Grammar<T, A>,
    items: &BTreeSet<Item>,
    symbol: &SymbolKind<T>,
) -> BTreeSet<Item> {
    let mut goto: BTreeSet<Item> = Default::default();

    for item in items {
        if let Some(next_symbol) = item.next_symbol(grammar) {
            if next_symbol == symbol {
                goto.insert(item.advance());
            }
        }
    }

    compute_closure(grammar, &goto)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct StateIdx(usize);

#[derive(Debug, Default)]
struct LR0Automaton {
    states: Vec<State>,
    // Maps existing item sets to their state indices, to maintain sharing.
    state_indices: FxHashMap<BTreeSet<Item>, StateIdx>,
}

#[derive(Debug)]
struct State {
    set: BTreeSet<Item>,
    transitions: FxHashMap<SymbolKind<char>, StateIdx>,
}

impl LR0Automaton {
    fn add_state_or_get_idx(&mut self, state: BTreeSet<Item>) -> (StateIdx, bool) {
        match self.state_indices.get(&state) {
            Some(idx) => (*idx, false),
            None => {
                let idx = self.states.len();
                self.states.push(State {
                    set: state.clone(),
                    transitions: Default::default(),
                });
                self.state_indices.insert(state, StateIdx(idx));
                (StateIdx(idx), true)
            }
        }
    }

    fn get_state(&self, idx: StateIdx) -> &BTreeSet<Item> {
        &self.states[idx.0].set
    }

    fn add_transition(&mut self, from: StateIdx, to: StateIdx, symbol: SymbolKind<char>) {
        self.states[from.0].transitions.insert(symbol, to);
    }
}

// (sets, transitions indexed by set indices)
fn compute_lr0_automaton<A>(grammar: &Grammar<char, A>) -> LR0Automaton {
    let mut automaton: LR0Automaton = Default::default();

    let init = compute_closure(
        grammar,
        &btreeset! { Item::new(NonTerminalIdx(0), ProductionIdx(0)) },
    );

    let (init_state_idx, _) = automaton.add_state_or_get_idx(init);

    let mut work_list: Vec<StateIdx> = vec![init_state_idx];

    while let Some(state_idx) = work_list.pop() {
        let state = automaton.get_state(state_idx).clone();
        let mut state_next_symbols: FxHashSet<SymbolKind<char>> = Default::default();
        for item in &state {
            if let Some(item_next_symbol) = item.next_symbol(grammar) {
                state_next_symbols.insert(item_next_symbol.clone());
            }
        }

        for symbol in state_next_symbols {
            let goto = compute_goto(grammar, &state, &symbol);
            let (goto_state, added) = automaton.add_state_or_get_idx(goto);
            if added {
                work_list.push(goto_state);
            }
            automaton.add_transition(state_idx, goto_state, symbol);
        }
    }

    automaton
}

#[test]
fn closure_1() {
    let grammar = crate::test_grammars::grammar6();

    let i0: BTreeSet<Item> = btreeset! { Item::new(NonTerminalIdx(0), ProductionIdx(0)) };
    let i0_closure = compute_closure(&grammar, &i0);

    assert_eq!(
        i0_closure,
        btreeset! {
            Item::new(NonTerminalIdx(0), ProductionIdx(0)), // E0 -> . E
            Item::new(NonTerminalIdx(1), ProductionIdx(0)), // E -> . E + T
            Item::new(NonTerminalIdx(1), ProductionIdx(1)), // E -> . T
            Item::new(NonTerminalIdx(2), ProductionIdx(0)), // T -> . T * F
            Item::new(NonTerminalIdx(2), ProductionIdx(1)), // T -> . F
            Item::new(NonTerminalIdx(3), ProductionIdx(0)), // F -> . ( E )
            Item::new(NonTerminalIdx(3), ProductionIdx(1)), // F -> . id
        }
    );
}

#[test]
fn goto_1() {
    let grammar = crate::test_grammars::grammar6();

    let i0: BTreeSet<Item> = btreeset! {
        Item::new_with_cursor(NonTerminalIdx(0), ProductionIdx(0), 1), // E0 -> E .
        Item::new_with_cursor(NonTerminalIdx(1), ProductionIdx(0), 1), // E -> E . + T
    };

    let goto = compute_goto(&grammar, &i0, &SymbolKind::Terminal('+'));

    assert_eq!(
        goto,
        btreeset! {
            Item::new_with_cursor(NonTerminalIdx(1), ProductionIdx(0), 2), // E -> E + . T
            Item::new(NonTerminalIdx(2), ProductionIdx(0)), // T -> . T * F
            Item::new(NonTerminalIdx(2), ProductionIdx(1)), // T -> . F
            Item::new(NonTerminalIdx(3), ProductionIdx(0)), // F -> . ( E )
            Item::new(NonTerminalIdx(3), ProductionIdx(1)), // F -> . id
        }
    );
}

#[test]
fn automaton_1() {
    let grammar = crate::test_grammars::grammar6();

    let e0_nt_idx = grammar.get_non_terminal_idx("E0").unwrap();
    let e_nt_idx = grammar.get_non_terminal_idx("E").unwrap();
    let t_nt_idx = grammar.get_non_terminal_idx("T").unwrap();
    let f_nt_idx = grammar.get_non_terminal_idx("F").unwrap();

    let automaton = compute_lr0_automaton(&grammar);

    let i0 = &automaton.states[0];
    assert_eq!(
        i0.set,
        btreeset! {
            Item::new(e0_nt_idx, ProductionIdx(0)), // E0 -> . E
            Item::new(e_nt_idx, ProductionIdx(0)), // E -> . E + T
            Item::new(e_nt_idx, ProductionIdx(1)), // E -> . T
            Item::new(t_nt_idx, ProductionIdx(0)), // T -> . T * F
            Item::new(t_nt_idx, ProductionIdx(1)), // T -> . F
            Item::new(f_nt_idx, ProductionIdx(0)), // F -> . ( E )
            Item::new(f_nt_idx, ProductionIdx(1)), // F -> . id
        }
    );

    let i1_idx = i0
        .transitions
        .get(&SymbolKind::NonTerminal(e_nt_idx))
        .unwrap();
    let i1 = &automaton.states[i1_idx.0];
    assert_eq!(
        i1.set,
        btreeset! {
            Item::new_with_cursor(e0_nt_idx, ProductionIdx(0), 1), // E0 -> E .
            Item::new_with_cursor(e_nt_idx, ProductionIdx(0), 1), // E -> E . + T
        }
    );

    let i2_idx = i0
        .transitions
        .get(&SymbolKind::NonTerminal(t_nt_idx))
        .unwrap();
    let i2 = &automaton.states[i2_idx.0];
    assert_eq!(
        i2.set,
        btreeset! {
            Item::new_with_cursor(e_nt_idx, ProductionIdx(1), 1), // E -> T .
            Item::new_with_cursor(t_nt_idx, ProductionIdx(0), 1), // T -> T . * F
        }
    );

    let i3_idx = i0
        .transitions
        .get(&SymbolKind::NonTerminal(f_nt_idx))
        .unwrap();
    let i3 = &automaton.states[i3_idx.0];
    assert_eq!(
        i3.set,
        btreeset! {
            Item::new_with_cursor(t_nt_idx, ProductionIdx(1), 1), // T -> F .
        }
    );

    let i4_idx = i0.transitions.get(&SymbolKind::Terminal('(')).unwrap();
    let i4 = &automaton.states[i4_idx.0];
    assert_eq!(
        i4.set,
        btreeset! {
            Item::new_with_cursor(f_nt_idx, ProductionIdx(0), 1), // F -> ( . E )
            Item::new(e_nt_idx, ProductionIdx(0)), // E -> . E + T
            Item::new(e_nt_idx, ProductionIdx(1)), // E -> . T
            Item::new(t_nt_idx, ProductionIdx(0)), // T -> . T * F
            Item::new(t_nt_idx, ProductionIdx(1)), // T -> . F
            Item::new(f_nt_idx, ProductionIdx(0)), // F -> . ( E )
            Item::new(f_nt_idx, ProductionIdx(1)), // F -> . id
        }
    );

    let i5_idx = i0.transitions.get(&SymbolKind::Terminal('x')).unwrap();
    let i5 = &automaton.states[i5_idx.0];
    assert_eq!(
        i5.set,
        btreeset! {
            Item::new_with_cursor(f_nt_idx, ProductionIdx(1), 1), // F -> id .
        }
    );

    let i6_idx = i1.transitions.get(&SymbolKind::Terminal('+')).unwrap();
    let i6 = &automaton.states[i6_idx.0];
    assert_eq!(
        i6.set,
        btreeset! {
            Item::new_with_cursor(e_nt_idx, ProductionIdx(0), 2), // E -> E + . T
            Item::new(t_nt_idx, ProductionIdx(0)), // T -> . T * F
            Item::new(t_nt_idx, ProductionIdx(1)), // T -> . F
            Item::new(f_nt_idx, ProductionIdx(0)), // F -> . ( E )
            Item::new(f_nt_idx, ProductionIdx(1)), // F -> . id
        }
    );

    let i7_idx = i2.transitions.get(&SymbolKind::Terminal('*')).unwrap();
    let i7 = &automaton.states[i7_idx.0];
    assert_eq!(
        i7.set,
        btreeset! {
            Item::new_with_cursor(t_nt_idx, ProductionIdx(0), 2), // T -> T * . F
            Item::new(f_nt_idx, ProductionIdx(0)), // F -> . ( E )
            Item::new(f_nt_idx, ProductionIdx(1)), // F -> . id
        }
    );

    let i8_idx = i4
        .transitions
        .get(&SymbolKind::NonTerminal(e_nt_idx))
        .unwrap();
    let i8 = &automaton.states[i8_idx.0];
    assert_eq!(
        i8.set,
        btreeset! {
            Item::new_with_cursor(f_nt_idx, ProductionIdx(0), 2), // F -> ( E . )
            Item::new_with_cursor(e_nt_idx, ProductionIdx(0), 1), // E -> E . + T
        }
    );

    let i9_idx = i6
        .transitions
        .get(&SymbolKind::NonTerminal(t_nt_idx))
        .unwrap();
    let i9 = &automaton.states[i9_idx.0];
    assert_eq!(
        i9.set,
        btreeset! {
            Item::new_with_cursor(e_nt_idx, ProductionIdx(0), 3), // E -> E + T .
            Item::new_with_cursor(t_nt_idx, ProductionIdx(0), 1), // T -> T . * F
        }
    );

    let i10_idx = i7
        .transitions
        .get(&SymbolKind::NonTerminal(f_nt_idx))
        .unwrap();
    let i10 = &automaton.states[i10_idx.0];
    assert_eq!(
        i10.set,
        btreeset! {
            Item::new_with_cursor(t_nt_idx, ProductionIdx(0), 3), // T -> T * F .
        }
    );

    let i11_idx = i8.transitions.get(&SymbolKind::Terminal(')')).unwrap();
    let i11 = &automaton.states[i11_idx.0];
    assert_eq!(
        i11.set,
        btreeset! {
            Item::new_with_cursor(f_nt_idx, ProductionIdx(0), 3), // F -> ( E ) .
        }
    );

    assert_eq!(automaton.states.len(), 12);
}
