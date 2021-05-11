use crate::follow::FollowTable;
use crate::grammar::{Grammar, NonTerminalIdx, Production, ProductionIdx, Symbol, SymbolKind};
use crate::lr_common::{LRAction, LRTable, LRTableBuilder, StateIdx};

use std::collections::BTreeSet;
use std::hash::Hash;

use fxhash::{FxHashMap, FxHashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct LR0Item {
    non_terminal_idx: NonTerminalIdx,
    production_idx: ProductionIdx,
    cursor: usize,
}

impl LR0Item {
    fn new(non_terminal_idx: NonTerminalIdx, production_idx: ProductionIdx) -> LR0Item {
        LR0Item {
            non_terminal_idx,
            production_idx,
            cursor: 0,
        }
    }

    fn new_with_cursor(
        non_terminal_idx: NonTerminalIdx,
        production_idx: ProductionIdx,
        cursor: usize,
    ) -> LR0Item {
        LR0Item {
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

    fn is_cursor_at_end<T, A>(&self, grammar: &Grammar<T, A>) -> bool {
        self.next_symbol(grammar).is_none()
    }

    fn advance(&self) -> LR0Item {
        LR0Item {
            non_terminal_idx: self.non_terminal_idx,
            production_idx: self.production_idx,
            cursor: self.cursor + 1,
        }
    }
}

fn compute_lr0_closure<T, A>(
    grammar: &Grammar<T, A>,
    items: &BTreeSet<LR0Item>,
) -> BTreeSet<LR0Item> {
    let mut closure: BTreeSet<LR0Item> = items.clone();

    let mut updated = true;
    while updated {
        updated = false;

        let mut new_items: FxHashSet<LR0Item> = Default::default();

        for item in &closure {
            if let Some(next_nt) = item.next_non_terminal(grammar) {
                let nt = grammar.get_non_terminal(next_nt);
                for prod_idx in nt.production_indices() {
                    let item = LR0Item::new(next_nt, prod_idx);
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

fn compute_lr0_goto<T: Eq, A>(
    grammar: &Grammar<T, A>,
    items: &BTreeSet<LR0Item>,
    symbol: &SymbolKind<T>,
) -> BTreeSet<LR0Item> {
    let mut goto: BTreeSet<LR0Item> = Default::default();

    for item in items {
        if let Some(next_symbol) = item.next_symbol(grammar) {
            if next_symbol == symbol {
                goto.insert(item.advance());
            }
        }
    }

    compute_lr0_closure(grammar, &goto)
}

#[derive(Debug)]
struct LR0Automaton<T> {
    states: Vec<LR0State<T>>,
    // Maps existing item sets to their state indices, to maintain sharing.
    state_indices: FxHashMap<BTreeSet<LR0Item>, StateIdx>,
}

impl<T> Default for LR0Automaton<T> {
    fn default() -> Self {
        LR0Automaton {
            states: vec![],
            state_indices: Default::default(),
        }
    }
}

impl<T> LR0Automaton<T> {
    fn state_indices(&self) -> impl Iterator<Item = (StateIdx, &LR0State<T>)> {
        self.states
            .iter()
            .enumerate()
            .map(|(i, s)| (StateIdx(i), s))
    }
}

#[derive(Debug)]
struct LR0State<T> {
    items: BTreeSet<LR0Item>,
    goto: FxHashMap<SymbolKind<T>, StateIdx>,
}

impl<T: Eq + Hash> LR0Automaton<T> {
    fn add_state_or_get_idx(&mut self, state: BTreeSet<LR0Item>) -> (StateIdx, bool) {
        match self.state_indices.get(&state) {
            Some(idx) => (*idx, false),
            None => {
                let idx = self.states.len();
                self.states.push(LR0State {
                    items: state.clone(),
                    goto: Default::default(),
                });
                self.state_indices.insert(state, StateIdx(idx));
                (StateIdx(idx), true)
            }
        }
    }

    fn get_state_items(&self, idx: StateIdx) -> &BTreeSet<LR0Item> {
        &self.get_state(idx).items
    }

    fn get_state(&self, idx: StateIdx) -> &LR0State<T> {
        &self.states[idx.0]
    }

    fn add_goto(&mut self, from: StateIdx, to: StateIdx, symbol: SymbolKind<T>) {
        self.states[from.0].goto.insert(symbol, to);
    }
}

// (sets, transitions indexed by set indices)
fn compute_lr0_automaton<T: Eq + Hash + Clone, A>(grammar: &Grammar<T, A>) -> LR0Automaton<T> {
    let mut automaton: LR0Automaton<T> = Default::default();

    let init = compute_lr0_closure(
        grammar,
        &btreeset! { LR0Item::new(NonTerminalIdx(0), ProductionIdx(0)) },
    );

    let (init_state_idx, _) = automaton.add_state_or_get_idx(init);

    let mut work_list: Vec<StateIdx> = vec![init_state_idx];

    while let Some(state_idx) = work_list.pop() {
        let state = automaton.get_state_items(state_idx).clone();
        let mut state_next_symbols: FxHashSet<SymbolKind<T>> = Default::default();
        for item in &state {
            if let Some(item_next_symbol) = item.next_symbol(grammar) {
                state_next_symbols.insert(item_next_symbol.clone());
            }
        }

        for symbol in state_next_symbols {
            let goto = compute_lr0_goto(grammar, &state, &symbol);
            let (goto_state, added) = automaton.add_state_or_get_idx(goto);
            if added {
                work_list.push(goto_state);
            }
            automaton.add_goto(state_idx, goto_state, symbol);
        }
    }

    automaton
}

fn symbols_eq<T: Eq>(ss1: &[Symbol<T>], ss2: &[SymbolKind<T>]) -> bool {
    ss1.len() == ss2.len() && ss1.iter().zip(ss2).all(|(s1, s2)| s1.kind == *s2)
}

fn build_slr_table<T: Eq + Hash + Copy, A>(
    grammar: &Grammar<T, A>,
    automaton: &LR0Automaton<T>,
    follow_table: &FollowTable<T>,
    n_terminals: usize,
) -> LRTable<T> {
    let mut table: LRTableBuilder<T> = LRTableBuilder::new(
        automaton.states.len(),
        n_terminals,
        grammar.non_terminals().len(),
    );

    for (state_idx, LR0State { items, goto }) in automaton.state_indices() {
        for item in items {
            // let production = grammar.get_production(non_terminal_idx, production_idx);

            // Rule 2.a
            if let Some(SymbolKind::Terminal(t)) = item.next_symbol(grammar) {
                if let Some(next_state) = goto.get(&SymbolKind::Terminal(*t)) {
                    table.add_shift(state_idx, *t, *next_state);
                }
            }

            // Rule 2.b
            if item.is_cursor_at_end(grammar) {
                let follow_set = follow_table.get_follow(item.non_terminal_idx);

                for follow in follow_set.terminals() {
                    table.add_reduce(
                        state_idx,
                        Some(*follow),
                        item.non_terminal_idx,
                        item.production_idx,
                    );
                }

                if follow_set.has_end() {
                    table.add_reduce(state_idx, None, item.non_terminal_idx, item.production_idx);
                }
            }

            // Rule 2.c
            // S' -> S .
            if item.non_terminal_idx == NonTerminalIdx(0)
                && item.production_idx == ProductionIdx(0)
                && item.cursor == 1
            {
                table.add_accept(state_idx);
            }
        }

        for (symbol_kind, next) in goto {
            if let SymbolKind::NonTerminal(non_terminal_idx) = symbol_kind {
                table.add_goto(state_idx, *non_terminal_idx, *next);
            }
        }
    }

    table.build()
}

#[cfg(test)]
use crate::lr_common::LRTableDisplay;

use std::fmt;

struct ItemDisplay<'a, T, A> {
    item: LR0Item,
    grammar: &'a Grammar<T, A>,
}

struct LR0AutomatonDisplay<'a, 'b, T, A> {
    automaton: &'a LR0Automaton<T>,
    grammar: &'b Grammar<T, A>,
}

impl<'a, T: fmt::Debug, A> fmt::Display for ItemDisplay<'a, T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let non_terminal = self.grammar.get_non_terminal(self.item.non_terminal_idx);
        write!(f, "[{} -> ", non_terminal.non_terminal)?;

        let production = self
            .grammar
            .get_production(self.item.non_terminal_idx, self.item.production_idx);
        for (symbol_idx, symbol) in production.symbols().iter().enumerate() {
            if symbol_idx == self.item.cursor {
                write!(f, "|")?;
            }
            match &symbol.kind {
                SymbolKind::NonTerminal(nt) => {
                    let nt = self.grammar.get_non_terminal(*nt);
                    write!(f, "{}", nt.non_terminal)?;
                }
                SymbolKind::Terminal(t) => {
                    write!(f, "{:?}", t)?;
                }
            }
            if symbol_idx != production.symbols().len() - 1 {
                write!(f, " ")?;
            }
        }

        if self.item.cursor == production.symbols().len() {
            write!(f, "|")?;
        }

        write!(f, "]")
    }
}

impl<'a, 'b, T: fmt::Debug, A> fmt::Display for LR0AutomatonDisplay<'a, 'b, T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (state_idx, state) in self.automaton.states.iter().enumerate() {
            writeln!(f, "{}: {{", state_idx)?;
            for item in &state.items {
                writeln!(
                    f,
                    "  {}",
                    ItemDisplay {
                        item: *item,
                        grammar: self.grammar
                    }
                )?;
            }

            for (symbol, next) in state.goto.iter() {
                match symbol {
                    SymbolKind::NonTerminal(nt) => {
                        writeln!(
                            f,
                            "  {} -> {}",
                            self.grammar.get_non_terminal(*nt).non_terminal,
                            next.0
                        )?;
                    }
                    SymbolKind::Terminal(token) => {
                        writeln!(f, "  {:?} -> {}", token, next.0)?;
                    }
                }
            }

            writeln!(f, "}}")?;
        }
        Ok(())
    }
}

#[test]
fn closure_1() {
    let grammar = crate::test_grammars::grammar6();

    let i0: BTreeSet<LR0Item> = btreeset! { LR0Item::new(NonTerminalIdx(0), ProductionIdx(0)) };
    let i0_closure = compute_lr0_closure(&grammar, &i0);

    assert_eq!(
        i0_closure,
        btreeset! {
            LR0Item::new(NonTerminalIdx(0), ProductionIdx(0)), // E0 -> . E
            LR0Item::new(NonTerminalIdx(1), ProductionIdx(0)), // E -> . E + T
            LR0Item::new(NonTerminalIdx(1), ProductionIdx(1)), // E -> . T
            LR0Item::new(NonTerminalIdx(2), ProductionIdx(0)), // T -> . T * F
            LR0Item::new(NonTerminalIdx(2), ProductionIdx(1)), // T -> . F
            LR0Item::new(NonTerminalIdx(3), ProductionIdx(0)), // F -> . ( E )
            LR0Item::new(NonTerminalIdx(3), ProductionIdx(1)), // F -> . id
        }
    );
}

#[test]
fn goto_1() {
    use crate::test_grammars::{grammar6, Grammar6Token};

    let grammar = grammar6();

    let i0: BTreeSet<LR0Item> = btreeset! {
        LR0Item::new_with_cursor(NonTerminalIdx(0), ProductionIdx(0), 1), // E0 -> E .
        LR0Item::new_with_cursor(NonTerminalIdx(1), ProductionIdx(0), 1), // E -> E . + T
    };

    let goto = compute_lr0_goto(&grammar, &i0, &SymbolKind::Terminal(Grammar6Token::Plus));

    assert_eq!(
        goto,
        btreeset! {
            LR0Item::new_with_cursor(NonTerminalIdx(1), ProductionIdx(0), 2), // E -> E + . T
            LR0Item::new(NonTerminalIdx(2), ProductionIdx(0)), // T -> . T * F
            LR0Item::new(NonTerminalIdx(2), ProductionIdx(1)), // T -> . F
            LR0Item::new(NonTerminalIdx(3), ProductionIdx(0)), // F -> . ( E )
            LR0Item::new(NonTerminalIdx(3), ProductionIdx(1)), // F -> . id
        }
    );
}

#[test]
fn automaton_1() {
    use crate::test_grammars::{grammar6, Grammar6Token};

    let grammar = grammar6();

    let e0_nt_idx = grammar.get_non_terminal_idx("E0").unwrap();
    let e_nt_idx = grammar.get_non_terminal_idx("E").unwrap();
    let t_nt_idx = grammar.get_non_terminal_idx("T").unwrap();
    let f_nt_idx = grammar.get_non_terminal_idx("F").unwrap();

    let automaton = compute_lr0_automaton(&grammar);

    let i0 = &automaton.states[0];
    assert_eq!(
        i0.items,
        btreeset! {
            LR0Item::new(e0_nt_idx, ProductionIdx(0)), // E0 -> . E
            LR0Item::new(e_nt_idx, ProductionIdx(0)), // E -> . E + T
            LR0Item::new(e_nt_idx, ProductionIdx(1)), // E -> . T
            LR0Item::new(t_nt_idx, ProductionIdx(0)), // T -> . T * F
            LR0Item::new(t_nt_idx, ProductionIdx(1)), // T -> . F
            LR0Item::new(f_nt_idx, ProductionIdx(0)), // F -> . ( E )
            LR0Item::new(f_nt_idx, ProductionIdx(1)), // F -> . id
        }
    );

    let i1_idx = i0.goto.get(&SymbolKind::NonTerminal(e_nt_idx)).unwrap();
    let i1 = &automaton.states[i1_idx.0];
    assert_eq!(
        i1.items,
        btreeset! {
            LR0Item::new_with_cursor(e0_nt_idx, ProductionIdx(0), 1), // E0 -> E .
            LR0Item::new_with_cursor(e_nt_idx, ProductionIdx(0), 1), // E -> E . + T
        }
    );

    let i2_idx = i0.goto.get(&SymbolKind::NonTerminal(t_nt_idx)).unwrap();
    let i2 = &automaton.states[i2_idx.0];
    assert_eq!(
        i2.items,
        btreeset! {
            LR0Item::new_with_cursor(e_nt_idx, ProductionIdx(1), 1), // E -> T .
            LR0Item::new_with_cursor(t_nt_idx, ProductionIdx(0), 1), // T -> T . * F
        }
    );

    let i3_idx = i0.goto.get(&SymbolKind::NonTerminal(f_nt_idx)).unwrap();
    let i3 = &automaton.states[i3_idx.0];
    assert_eq!(
        i3.items,
        btreeset! {
            LR0Item::new_with_cursor(t_nt_idx, ProductionIdx(1), 1), // T -> F .
        }
    );

    let i4_idx = i0
        .goto
        .get(&SymbolKind::Terminal(Grammar6Token::LParen))
        .unwrap();
    let i4 = &automaton.states[i4_idx.0];
    assert_eq!(
        i4.items,
        btreeset! {
            LR0Item::new_with_cursor(f_nt_idx, ProductionIdx(0), 1), // F -> ( . E )
            LR0Item::new(e_nt_idx, ProductionIdx(0)), // E -> . E + T
            LR0Item::new(e_nt_idx, ProductionIdx(1)), // E -> . T
            LR0Item::new(t_nt_idx, ProductionIdx(0)), // T -> . T * F
            LR0Item::new(t_nt_idx, ProductionIdx(1)), // T -> . F
            LR0Item::new(f_nt_idx, ProductionIdx(0)), // F -> . ( E )
            LR0Item::new(f_nt_idx, ProductionIdx(1)), // F -> . id
        }
    );

    let i5_idx = i0
        .goto
        .get(&SymbolKind::Terminal(Grammar6Token::Id))
        .unwrap();
    let i5 = &automaton.states[i5_idx.0];
    assert_eq!(
        i5.items,
        btreeset! {
            LR0Item::new_with_cursor(f_nt_idx, ProductionIdx(1), 1), // F -> id .
        }
    );

    let i6_idx = i1
        .goto
        .get(&SymbolKind::Terminal(Grammar6Token::Plus))
        .unwrap();
    let i6 = &automaton.states[i6_idx.0];
    assert_eq!(
        i6.items,
        btreeset! {
            LR0Item::new_with_cursor(e_nt_idx, ProductionIdx(0), 2), // E -> E + . T
            LR0Item::new(t_nt_idx, ProductionIdx(0)), // T -> . T * F
            LR0Item::new(t_nt_idx, ProductionIdx(1)), // T -> . F
            LR0Item::new(f_nt_idx, ProductionIdx(0)), // F -> . ( E )
            LR0Item::new(f_nt_idx, ProductionIdx(1)), // F -> . id
        }
    );

    let i7_idx = i2
        .goto
        .get(&SymbolKind::Terminal(Grammar6Token::Star))
        .unwrap();
    let i7 = &automaton.states[i7_idx.0];
    assert_eq!(
        i7.items,
        btreeset! {
            LR0Item::new_with_cursor(t_nt_idx, ProductionIdx(0), 2), // T -> T * . F
            LR0Item::new(f_nt_idx, ProductionIdx(0)), // F -> . ( E )
            LR0Item::new(f_nt_idx, ProductionIdx(1)), // F -> . id
        }
    );

    let i8_idx = i4.goto.get(&SymbolKind::NonTerminal(e_nt_idx)).unwrap();
    let i8 = &automaton.states[i8_idx.0];
    assert_eq!(
        i8.items,
        btreeset! {
            LR0Item::new_with_cursor(f_nt_idx, ProductionIdx(0), 2), // F -> ( E . )
            LR0Item::new_with_cursor(e_nt_idx, ProductionIdx(0), 1), // E -> E . + T
        }
    );

    let i9_idx = i6.goto.get(&SymbolKind::NonTerminal(t_nt_idx)).unwrap();
    let i9 = &automaton.states[i9_idx.0];
    assert_eq!(
        i9.items,
        btreeset! {
            LR0Item::new_with_cursor(e_nt_idx, ProductionIdx(0), 3), // E -> E + T .
            LR0Item::new_with_cursor(t_nt_idx, ProductionIdx(0), 1), // T -> T . * F
        }
    );

    let i10_idx = i7.goto.get(&SymbolKind::NonTerminal(f_nt_idx)).unwrap();
    let i10 = &automaton.states[i10_idx.0];
    assert_eq!(
        i10.items,
        btreeset! {
            LR0Item::new_with_cursor(t_nt_idx, ProductionIdx(0), 3), // T -> T * F .
        }
    );

    let i11_idx = i8
        .goto
        .get(&SymbolKind::Terminal(Grammar6Token::RParen))
        .unwrap();
    let i11 = &automaton.states[i11_idx.0];
    assert_eq!(
        i11.items,
        btreeset! {
            LR0Item::new_with_cursor(f_nt_idx, ProductionIdx(0), 3), // F -> ( E ) .
        }
    );

    assert_eq!(automaton.states.len(), 12);
}

#[test]
fn simulate1() {
    use crate::first::generate_first_table;
    use crate::follow::generate_follow_table;
    use crate::test_grammars::{grammar6, Grammar6Token};

    let grammar = grammar6();
    let first = generate_first_table(&grammar);
    let follow = generate_follow_table(&grammar, &first);
    let lr_automaton = compute_lr0_automaton(&grammar);

    println!(
        "{}",
        LR0AutomatonDisplay {
            automaton: &lr_automaton,
            grammar: &grammar
        }
    );

    let slr = build_slr_table(&grammar, &lr_automaton, &follow, 5);

    println!("{}", LRTableDisplay::new(&slr, &grammar),);

    crate::lr_common::simulate(
        &slr,
        &grammar,
        vec![Grammar6Token::Id, Grammar6Token::Plus, Grammar6Token::Id].into_iter(),
    );
}
