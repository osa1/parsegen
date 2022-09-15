use crate::collections::{Map, Set};
use crate::grammar::{Grammar, TerminalIdx};
use crate::lane_tracer::ConflictIdx;
use crate::lr0::LR0ItemIdx;
use crate::lr_common::StateIdx;

use std::fmt;

#[derive(Debug, Default)]
pub struct LaneTable {
    lookaheads: Map<(StateIdx, ConflictIdx), Set<Option<TerminalIdx>>>,
}

impl LaneTable {
    pub fn add_lookahead(
        &mut self,
        state: StateIdx,
        idx: ConflictIdx,
        lookahead: Option<TerminalIdx>,
    ) {
        self.lookaheads
            .entry((state, idx))
            .or_default()
            .insert(lookahead);
    }
}

pub struct LaneTableDisplay<'a, 'b, A> {
    pub lane_table: &'a LaneTable,
    pub grammar: &'b Grammar<A>,
}

impl<'a, 'b, A> fmt::Display for LaneTableDisplay<'a, 'b, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut conflicts: Map<ConflictIdx, Vec<(StateIdx, Set<Option<TerminalIdx>>)>> =
            Default::default();

        for ((state_idx, conflict_idx), lookaheads) in &self.lane_table.lookaheads {
            conflicts
                .entry(*conflict_idx)
                .or_default()
                .push((*state_idx, lookaheads.clone()));
        }

        for (conflict_idx, state_contributions) in conflicts {
            writeln!(f, "C{}", conflict_idx.0)?;
            for (state_idx, lookaheads) in state_contributions {
                write!(f, "    S{}: {{", state_idx.as_usize())?;
                for (t_idx, t) in lookaheads.iter().enumerate() {
                    match t {
                        Some(t) => write!(f, "{}", self.grammar.get_terminal(*t))?,
                        None => write!(f, "$")?,
                    }
                    if t_idx != lookaheads.len() - 1 {
                        write!(f, " ")?;
                    }
                }
                writeln!(f, "}}")?;
            }
        }

        Ok(())
    }
}
