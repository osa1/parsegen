use crate::grammar::{Grammar, NonTerminalIdx, ProductionIdx};

use std::hash::Hash;

use fxhash::FxHashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StateIdx(pub usize);

impl StateIdx {
    pub fn as_usize(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LRAction<A> {
    /// Shift current terminal, switch to given state
    Shift(StateIdx),

    /// Reduce using the given production
    Reduce(NonTerminalIdx, ProductionIdx, A),

    /// Accept the input
    Accept,
}

pub struct LRTable<T: Eq + Hash, A> {
    action: FxHashMap<StateIdx, FxHashMap<Option<T>, LRAction<A>>>,
    goto: FxHashMap<StateIdx, FxHashMap<NonTerminalIdx, StateIdx>>,
    n_states: usize,
}

pub struct LRTableBuilder<T: Eq + Hash, A> {
    table: LRTable<T, A>,
}

impl<T: Eq + Hash + Clone + fmt::Debug, A: fmt::Debug + Eq> LRTableBuilder<T, A> {
    pub fn new(n_states: usize) -> Self {
        Self {
            table: LRTable {
                action: Default::default(),
                goto: Default::default(),
                n_states,
            },
        }
    }

    pub fn build(self) -> LRTable<T, A> {
        self.table
    }

    pub fn add_shift(&mut self, state: StateIdx, token: T, next_state: StateIdx) {
        let action = self.table.action.entry(state).or_default();

        let old_action = action.insert(Some(token.clone()), LRAction::Shift(next_state));

        if let Some(old_action) = old_action {
            match old_action {
                LRAction::Shift(s) => {
                    if s != next_state {
                        panic!(
                            "({}, {:?}): Overriding Shift({}) with Shift({})",
                            state.0, token, s.0, next_state.0,
                        );
                    }
                }
                LRAction::Reduce(_nt_, _p_, _) => {
                    // TODO: Allowing overriding reduce actions for shift for now
                    // panic!(
                    //     "({}, {:?}): Overriding Reduce({}, {}) action with Shift({})",
                    //     state.0, token, nt_.0, p_.0, next_state.0,
                    // );
                }
                LRAction::Accept => {
                    panic!(
                        "({}, {:?}): Overriding Accept action with Shift({})",
                        state.0, token, next_state.0,
                    );
                }
            }
        }
    }

    pub fn add_reduce(
        &mut self,
        state: StateIdx,
        token: Option<T>,
        non_terminal_idx: NonTerminalIdx,
        production_idx: ProductionIdx,
        semantic_action: A,
    ) {
        let action = self.table.action.entry(state).or_default();

        let old_action = action.insert(
            token.clone(),
            LRAction::Reduce(non_terminal_idx, production_idx, semantic_action),
        );

        if let Some(old_action) = old_action {
            match old_action {
                LRAction::Shift(s) => {
                    panic!(
                        "({}, {:?}): Overriding Shift({}) with Reduce({}, {})",
                        state.0, token, s.0, non_terminal_idx.0, production_idx.0
                    )
                }
                LRAction::Reduce(nt_, p_, _) => {
                    panic!(
                        "({}, {:?}): Overriding Reduce({}, {}) action with Reduce({}, {})",
                        state.0, token, nt_.0, p_.0, non_terminal_idx.0, production_idx.0
                    );
                }
                LRAction::Accept => {
                    panic!(
                        "({}, {:?}): Overriding Accept action with Reduce({}, {})",
                        state.0, token, non_terminal_idx.0, production_idx.0
                    );
                }
            }
        }
    }

    pub fn add_accept(&mut self, state: StateIdx) {
        let _old = self
            .table
            .action
            .entry(state)
            .or_default()
            .insert(None, LRAction::Accept);
        // assert_eq!(old, None);
    }

    pub fn add_goto(&mut self, state: StateIdx, non_terminal_idx: NonTerminalIdx, next: StateIdx) {
        let _old = self
            .table
            .goto
            .entry(state)
            .or_default()
            .insert(non_terminal_idx, next);
        // same as add_shift..
        // assert_eq!(old, None, "trying to add {}", next.0);
    }
}

impl<T: Eq + Hash, A> LRTable<T, A> {
    #[cfg(test)]
    pub fn get_action(&self, state: StateIdx, non_terminal: Option<T>) -> Option<&LRAction<A>> {
        self.action
            .get(&state)
            .and_then(|action| action.get(&non_terminal))
    }

    #[cfg(test)]
    pub fn get_goto(&self, state: StateIdx, non_terminal: NonTerminalIdx) -> Option<StateIdx> {
        self.goto
            .get(&state)
            .and_then(|goto| goto.get(&non_terminal))
            .copied()
    }

    pub fn get_action_table(&self) -> &FxHashMap<StateIdx, FxHashMap<Option<T>, LRAction<A>>> {
        &self.action
    }

    pub fn get_goto_table(&self) -> &FxHashMap<StateIdx, FxHashMap<NonTerminalIdx, StateIdx>> {
        &self.goto
    }

    pub fn n_states(&self) -> usize {
        self.n_states
    }
}

#[cfg(test)]
pub fn simulate<T: Eq + Hash + Copy + std::fmt::Debug, A>(
    table: &LRTable<T, A>,
    grammar: &Grammar<T, A>,
    mut input: impl Iterator<Item = T>,
) {
    let mut stack: Vec<StateIdx> = vec![StateIdx(0)];

    let mut a = input.next();

    loop {
        let s = *stack.last().unwrap();
        match table.get_action(s, a) {
            Some(action) => {
                match action {
                    LRAction::Shift(t) => {
                        stack.push(*t);
                        a = input.next();
                    }
                    LRAction::Reduce(non_terminal_idx, production_idx, _) => {
                        let n_symbols = grammar
                            .get_production(*non_terminal_idx, *production_idx)
                            .symbols()
                            .len();
                        for _ in 0..n_symbols {
                            stack.pop();
                        }
                        let s = *stack.last().unwrap();
                        match table.get_goto(s, *non_terminal_idx) {
                            None => panic!("Stuck! (1)"),
                            Some(next) => stack.push(next),
                        }
                        // TODO: semantic action
                    }
                    LRAction::Accept => {
                        break;
                    }
                }
            }
            None => {
                panic!(
                    "Stuck! state = {:?}, stack = {:?}, token = {:?}",
                    s, stack, a
                );
            }
        }
    }

    println!(
        "Parsing done. Stack = {:?}, input.next() = {:?}",
        stack,
        input.next()
    );
}

pub struct LRTableDisplay<'a, 'b, T: Hash + Eq, A> {
    table: &'a LRTable<T, A>,
    grammar: &'b Grammar<T, A>,
}

impl<'a, 'b, T: Hash + Eq, A> LRTableDisplay<'a, 'b, T, A> {
    #[cfg(test)]
    pub fn new(table: &'a LRTable<T, A>, grammar: &'b Grammar<T, A>) -> Self {
        Self { table, grammar }
    }
}

pub struct LRActionDisplay<'a, 'b, T, A> {
    action: &'a LRAction<A>,
    grammar: &'b Grammar<T, A>,
}

use crate::grammar::ProductionDisplay;

use std::fmt;

impl<'a, 'b, T: Hash + Eq + fmt::Debug, A> fmt::Display for LRTableDisplay<'a, 'b, T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for state_idx in 0..self.table.n_states() {
            writeln!(f, "{}: {{", state_idx)?;
            let state_idx = StateIdx(state_idx);

            let actions = self.table.action.get(&state_idx);

            let gotos = self.table.goto.get(&state_idx);

            if let Some(actions) = actions {
                for (terminal, action) in actions {
                    match terminal {
                        Some(t) => write!(f, "  {:?} -> ", t)?,
                        None => write!(f, "  EOF -> ")?,
                    }
                    writeln!(
                        f,
                        "{}",
                        LRActionDisplay {
                            action,
                            grammar: self.grammar
                        }
                    )?;
                }
            }

            if let Some(gotos) = gotos {
                for (non_terminal, next_state) in gotos {
                    let nt = &self.grammar.get_non_terminal(*non_terminal).non_terminal;
                    writeln!(f, "  {} -> {}", nt, next_state.0)?;
                }
            }

            writeln!(f, "}}")?;
        }

        Ok(())
    }
}

impl<'a, 'b, T: fmt::Debug, A> fmt::Display for LRActionDisplay<'a, 'b, T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.action {
            LRAction::Shift(next) => write!(f, "Shift {}", next.0),
            LRAction::Reduce(nt, p, _) => {
                let p_ = self.grammar.get_production(*nt, *p);
                let nt_ = self.grammar.get_non_terminal(*nt);
                write!(
                    f,
                    "Reduce ({} -> {}) (nt_idx={}, p_idx={})",
                    nt_.non_terminal,
                    ProductionDisplay::new(p_, self.grammar),
                    nt.0,
                    p.0,
                )
            }
            LRAction::Accept => write!(f, "Accept"),
        }
    }
}
