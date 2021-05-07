use crate::grammar::{Grammar, NonTerminalIdx, ProductionIdx, Symbol, SymbolKind};

use std::collections::BTreeSet;
use std::hash::Hash;

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

#[derive(Debug)]
struct LR0Automaton<T> {
    states: Vec<State<T>>,
    // Maps existing item sets to their state indices, to maintain sharing.
    state_indices: FxHashMap<BTreeSet<Item>, StateIdx>,
}

impl<T> Default for LR0Automaton<T> {
    fn default() -> Self {
        LR0Automaton {
            states: vec![],
            state_indices: Default::default(),
        }
    }
}

#[derive(Debug)]
struct State<T> {
    set: BTreeSet<Item>,
    transitions: FxHashMap<SymbolKind<T>, StateIdx>,
}

impl<T: Eq + Hash> LR0Automaton<T> {
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

    fn get_state_items(&self, idx: StateIdx) -> &BTreeSet<Item> {
        &self.get_state(idx).set
    }

    fn get_state(&self, idx: StateIdx) -> &State<T> {
        &self.states[idx.0]
    }

    fn add_transition(&mut self, from: StateIdx, to: StateIdx, symbol: SymbolKind<T>) {
        self.states[from.0].transitions.insert(symbol, to);
    }
}

// (sets, transitions indexed by set indices)
fn compute_lr0_automaton<T: Eq + Hash + Clone, A>(grammar: &Grammar<T, A>) -> LR0Automaton<T> {
    let mut automaton: LR0Automaton<T> = Default::default();

    let init = compute_closure(
        grammar,
        &btreeset! { Item::new(NonTerminalIdx(0), ProductionIdx(0)) },
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

fn symbols_eq<T: Eq>(ss1: &[Symbol<T>], ss2: &[SymbolKind<T>]) -> bool {
    ss1.len() == ss2.len() && ss1.iter().zip(ss2).all(|(s1, s2)| s1.kind == *s2)
}

fn simulate<A>(grammar: &Grammar<char, A>, input: &str) {
    let automaton = compute_lr0_automaton(grammar);

    println!(
        "{}",
        LR0AutomatonDisplay {
            automaton: &automaton,
            grammar
        }
    );

    // FIXME: we only need state stack. terminals are useful for semantic actions.
    let mut symbol_stack: Vec<SymbolKind<char>> = vec![];
    let mut state_stack: Vec<StateIdx> = vec![StateIdx(0)];

    let mut chars = input.chars();
    let mut current_char = chars.next();
    'main_loop: loop {
        let char = match current_char {
            None => break,
            Some(char) => char,
        };

        println!("char={:?}", char);
        println!("symbol_stack={:?}", symbol_stack);
        println!("state_stack={:?}", state_stack);

        let current_state_idx = *state_stack.last().unwrap();
        let current_state = automaton.get_state(current_state_idx);

        // Shift
        if let Some(next_state) = current_state.transitions.get(&SymbolKind::Terminal(char)) {
            state_stack.push(*next_state);
            symbol_stack.push(SymbolKind::Terminal(char));
            current_char = chars.next();

            println!("--- Shifting {:?}, new state={}", char, next_state.0);

            continue 'main_loop;
        }

        // Reduce
        for item in current_state.set.iter().copied() {
            let item_production =
                grammar.get_production(item.non_terminal_idx, item.production_idx);

            let item_symbols = item_production.symbols();
            let item_n_symbols = item_symbols.len();

            if item.cursor != item_n_symbols {
                continue;
            }

            if symbol_stack.len() < item_n_symbols {
                continue;
            }

            let stack_symbols = &symbol_stack[symbol_stack.len() - item_n_symbols..];
            if symbols_eq(item_symbols, stack_symbols) {
                println!("Reducing item {}", ItemDisplay { item, grammar });

                for _ in 0..item_n_symbols {
                    symbol_stack.pop();
                }
                symbol_stack.push(SymbolKind::NonTerminal(item.non_terminal_idx));
                state_stack.pop();
                let current_state_idx = *state_stack.last().unwrap();
                let current_state = automaton.get_state(current_state_idx);
                match current_state
                    .transitions
                    .get(&SymbolKind::NonTerminal(item.non_terminal_idx))
                {
                    None => panic!("Stuck!"),
                    Some(next_state) => state_stack.push(*next_state),
                }
                continue 'main_loop;
            }
        }

        panic!("Can't shift or reduce");
    }

    println!("Final symbol stack = {:?}", symbol_stack);
    println!("Final state  stack = {:?}", state_stack);
}

use std::fmt;

struct ItemDisplay<'a, A> {
    item: Item,
    grammar: &'a Grammar<char, A>,
}

struct LR0AutomatonDisplay<'a, 'b, T, A> {
    automaton: &'a LR0Automaton<T>,
    grammar: &'b Grammar<char, A>,
}

impl<'a, A> fmt::Display for ItemDisplay<'a, A> {
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
            match symbol.kind {
                SymbolKind::NonTerminal(nt) => {
                    let nt = self.grammar.get_non_terminal(nt);
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
            for item in &state.set {
                writeln!(
                    f,
                    "  {}",
                    ItemDisplay {
                        item: *item,
                        grammar: self.grammar
                    }
                )?;
            }

            for (symbol, next) in state.transitions.iter() {
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
    use crate::test_grammars::{grammar6, Grammar6Token};

    let grammar = grammar6();

    let i0: BTreeSet<Item> = btreeset! {
        Item::new_with_cursor(NonTerminalIdx(0), ProductionIdx(0), 1), // E0 -> E .
        Item::new_with_cursor(NonTerminalIdx(1), ProductionIdx(0), 1), // E -> E . + T
    };

    let goto = compute_goto(&grammar, &i0, &SymbolKind::Terminal(Grammar6Token::Plus));

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
    use crate::test_grammars::{grammar6, Grammar6Token};

    let grammar = grammar6();

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

    let i4_idx = i0
        .transitions
        .get(&SymbolKind::Terminal(Grammar6Token::LParen))
        .unwrap();
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

    let i5_idx = i0
        .transitions
        .get(&SymbolKind::Terminal(Grammar6Token::Id))
        .unwrap();
    let i5 = &automaton.states[i5_idx.0];
    assert_eq!(
        i5.set,
        btreeset! {
            Item::new_with_cursor(f_nt_idx, ProductionIdx(1), 1), // F -> id .
        }
    );

    let i6_idx = i1
        .transitions
        .get(&SymbolKind::Terminal(Grammar6Token::Plus))
        .unwrap();
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

    let i7_idx = i2
        .transitions
        .get(&SymbolKind::Terminal(Grammar6Token::Star))
        .unwrap();
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

    let i11_idx = i8
        .transitions
        .get(&SymbolKind::Terminal(Grammar6Token::RParen))
        .unwrap();
    let i11 = &automaton.states[i11_idx.0];
    assert_eq!(
        i11.set,
        btreeset! {
            Item::new_with_cursor(f_nt_idx, ProductionIdx(0), 3), // F -> ( E ) .
        }
    );

    assert_eq!(automaton.states.len(), 12);
}

#[test]
fn simulate_1() {
    // simulate(&crate::test_grammars::grammar6(), "x*x");
}
