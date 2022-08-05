use crate::grammar::{Grammar, NonTerminalIdx, ProductionIdx, TerminalIdx};

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

pub struct LRTable<A> {
    action: FxHashMap<StateIdx, FxHashMap<Option<TerminalIdx>, LRAction<A>>>,
    goto: FxHashMap<StateIdx, FxHashMap<NonTerminalIdx, StateIdx>>,
    n_states: usize,
}

pub struct LRTableBuilder<A> {
    table: LRTable<A>,
}

impl<A: fmt::Debug + Eq> LRTableBuilder<A> {
    pub fn new(n_states: usize) -> Self {
        Self {
            table: LRTable {
                action: Default::default(),
                goto: Default::default(),
                n_states,
            },
        }
    }

    pub fn build(self) -> LRTable<A> {
        self.table
    }

    pub fn add_shift(
        &mut self,
        grammar: &Grammar<A>,
        state: StateIdx,
        token: TerminalIdx,
        next_state: StateIdx,
    ) {
        let action = self.table.action.entry(state).or_default();

        let old_action = action.insert(Some(token), LRAction::Shift(next_state));

        if let Some(old_action) = old_action {
            match old_action {
                LRAction::Shift(s) => {
                    if s != next_state {
                        panic!(
                            "({}, {:?}): Overriding Shift({}) with Shift({})",
                            state.0,
                            grammar.get_terminal(token),
                            s.0,
                            next_state.0,
                        );
                    }
                }
                LRAction::Reduce(_nt_, _p_, _) => {
                    // TODO: Allowing overriding reduce actions for shift for now
                    // let production = grammar.get_production(nt_, p_);
                    // panic!(
                    //     "({}, {:?}): Overriding Reduce({} -> {}) action with Shift({})",
                    //     state.0,
                    //     grammar.get_terminal(token),
                    //     grammar.get_non_terminal(nt_).non_terminal,
                    //     ProductionDisplay::new(production, grammar),
                    //     next_state.0,
                    // );
                }
                LRAction::Accept => {
                    panic!(
                        "({}, {:?}): Overriding Accept action with Shift({})",
                        state.0,
                        grammar.get_terminal(token),
                        next_state.0,
                    );
                }
            }
        }
    }

    pub fn add_reduce(
        &mut self,
        grammar: &Grammar<A>,
        state: StateIdx,
        token: Option<TerminalIdx>,
        non_terminal_idx: NonTerminalIdx,
        production_idx: ProductionIdx,
        semantic_action: A,
    ) {
        let action = self.table.action.entry(state).or_default();

        let old_action = action.insert(
            token,
            LRAction::Reduce(non_terminal_idx, production_idx, semantic_action),
        );

        if let Some(old_action) = old_action {
            match old_action {
                LRAction::Shift(s) => {
                    panic!(
                        "({}, {:?}): Overriding Shift({}) with Reduce({}, {})",
                        state.0,
                        token.map(|t| grammar.get_terminal(t)),
                        s.0,
                        non_terminal_idx.0,
                        production_idx.0
                    )
                }
                LRAction::Reduce(nt_, p_, _) => {
                    panic!(
                        "({}, {:?}): Overriding Reduce({}, {}) action with Reduce({}, {})",
                        state.0,
                        token.map(|t| grammar.get_terminal(t)),
                        nt_.0,
                        p_.0,
                        non_terminal_idx.0,
                        production_idx.0
                    );
                }
                LRAction::Accept => {
                    panic!(
                        "({}, {:?}): Overriding Accept action with Reduce({}, {})",
                        state.0,
                        token.map(|t| grammar.get_terminal(t)),
                        non_terminal_idx.0,
                        production_idx.0
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

impl<A> LRTable<A> {
    pub fn get_action(
        &self,
        state: StateIdx,
        non_terminal: Option<TerminalIdx>,
    ) -> Option<&LRAction<A>> {
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

    pub fn get_action_table(
        &self,
    ) -> &FxHashMap<StateIdx, FxHashMap<Option<TerminalIdx>, LRAction<A>>> {
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
pub fn simulate<A>(
    table: &LRTable<A>,
    grammar: &Grammar<A>,
    mut input: impl Iterator<Item = TerminalIdx>,
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

pub struct LRTableDisplay<'a, 'b, A> {
    table: &'a LRTable<A>,
    grammar: &'b Grammar<A>,
}

impl<'a, 'b, A> LRTableDisplay<'a, 'b, A> {
    #[cfg(test)]
    pub fn new(table: &'a LRTable<A>, grammar: &'b Grammar<A>) -> Self {
        Self { table, grammar }
    }
}

pub struct LRActionDisplay<'a, 'b, A> {
    action: &'a LRAction<A>,
    grammar: &'b Grammar<A>,
}

use crate::grammar::ProductionDisplay;

use std::fmt;

impl<'a, 'b, A> fmt::Display for LRTableDisplay<'a, 'b, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for state_idx in 0..self.table.n_states() {
            writeln!(f, "{}: {{", state_idx)?;
            let state_idx = StateIdx(state_idx);

            let actions = self.table.action.get(&state_idx);

            let gotos = self.table.goto.get(&state_idx);

            if let Some(actions) = actions {
                for (terminal, action) in actions {
                    match terminal {
                        Some(t) => {
                            let t = self.grammar.get_terminal(*t);
                            write!(f, "  {:?} -> ", t)?
                        }
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

impl<'a, 'b, A> fmt::Display for LRActionDisplay<'a, 'b, A> {
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
