use crate::collections::{Map, Set};
use crate::grammar::Symbol;
use crate::lr_common::StateIdx;

use std::fmt;

#[derive(Debug)]
pub struct StateGraph {
    succs: Map<StateIdx, Set<StateIdx>>,
    preds: Map<StateIdx, Set<StateIdx>>,
}

impl StateGraph {
    pub fn new() -> Self {
        StateGraph {
            succs: Default::default(),
            preds: Default::default(),
        }
    }

    pub fn add_successor(&mut self, s1: StateIdx, s2: StateIdx) {
        self.succs.entry(s1).or_default().insert(s2);
        self.preds.entry(s2).or_default().insert(s1);
    }

    pub fn add_predecessor(&mut self, s1: StateIdx, s2: StateIdx) {
        self.preds.entry(s1).or_default().insert(s2);
        self.succs.entry(s2).or_default().insert(s1);
    }

    pub fn predecessors(&mut self, state_idx: StateIdx) -> impl Iterator<Item = &StateIdx> {
        // TODO: We can't return different iterators based on whether state exists in the map or
        // not, so we're creating an empty set here, which requires the method to be `mut`.
        self.preds.entry(state_idx).or_default().iter()
    }
}

impl fmt::Display for StateGraph {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (state, succs) in &self.succs {
            write!(f, "{} -> ", state.0)?;
            for succ in succs {
                write!(f, "{} ", succ.0)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}
