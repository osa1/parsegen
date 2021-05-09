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
pub enum LRAction {
    /// Shift current terminal, switch to given state
    Shift(StateIdx),

    /// Reduce using the given production
    Reduce(NonTerminalIdx, ProductionIdx),

    /// Accept the input
    Accept,
}

pub struct LRTable<T: Eq + Hash> {
    action: FxHashMap<(StateIdx, Option<T>), LRAction>,
    goto: FxHashMap<(StateIdx, NonTerminalIdx), StateIdx>,
}

pub struct LRTableBuilder<T: Eq + Hash> {
    action: FxHashMap<(StateIdx, Option<T>), LRAction>,
    goto: FxHashMap<(StateIdx, NonTerminalIdx), StateIdx>,
}

impl<T: Eq + Hash> Default for LRTableBuilder<T> {
    fn default() -> Self {
        Self {
            action: Default::default(),
            goto: Default::default(),
        }
    }
}

impl<T: Eq + Hash> LRTableBuilder<T> {
    pub fn build(self) -> LRTable<T> {
        LRTable {
            action: self.action,
            goto: self.goto,
        }
    }

    pub fn add_shift(&mut self, state: StateIdx, token: T, next_state: StateIdx) {
        let old = self
            .action
            .insert((state, Some(token)), LRAction::Shift(next_state));
        // In LR(1) we sometimes add shift to same state mutiple times, not sure why
        // assert_eq!(old, None, "trying to add {}", next_state.0);
    }

    pub fn add_reduce(
        &mut self,
        state: StateIdx,
        token: Option<T>,
        non_terminal_idx: NonTerminalIdx,
        production_idx: ProductionIdx,
    ) {
        let old = self.action.insert(
            (state, token),
            LRAction::Reduce(non_terminal_idx, production_idx),
        );
        assert!(old.is_none());
    }

    pub fn add_accept(&mut self, state: StateIdx) {
        let old = self.action.insert((state, None), LRAction::Accept);
        // assert_eq!(old, None);
    }

    pub fn add_goto(&mut self, state: StateIdx, non_terminal_idx: NonTerminalIdx, next: StateIdx) {
        let old = self.goto.insert((state, non_terminal_idx), next);
        // same as add_shift..
        // assert_eq!(old, None, "trying to add {}", next.0);
    }
}

impl<T: Eq + Hash> LRTable<T> {
    pub fn get_action(&self, state: StateIdx, non_terminal: Option<T>) -> Option<LRAction> {
        self.action.get(&(state, non_terminal)).copied()
    }

    pub fn get_goto(&self, state: StateIdx, non_terminal: NonTerminalIdx) -> Option<StateIdx> {
        self.goto.get(&(state, non_terminal)).cloned()
    }

    // For debugging
    pub fn actions(&self) -> impl Iterator<Item = (&(StateIdx, Option<T>), &LRAction)> {
        self.action.iter()
    }

    // For debugging
    pub fn gotos(&self) -> impl Iterator<Item = (&(StateIdx, NonTerminalIdx), &StateIdx)> {
        self.goto.iter()
    }
}

// TODO: The same as LR0 simulation
pub fn simulate<T: Eq + Hash + Copy + std::fmt::Debug, A>(
    table: &LRTable<T>,
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
                        stack.push(t);
                        a = input.next();
                    }
                    LRAction::Reduce(non_terminal_idx, terminal_idx) => {
                        let production = grammar.get_production(non_terminal_idx, terminal_idx);
                        let n_symbols = production.symbols().len();
                        for _ in 0..n_symbols {
                            stack.pop();
                        }
                        let s = *stack.last().unwrap();
                        match table.get_goto(s, non_terminal_idx) {
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
