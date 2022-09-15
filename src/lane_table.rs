use crate::collections::{Map, Set};
use crate::grammar::TerminalIdx;
use crate::lane_tracer::ConflictIdx;
use crate::lr0::LR0ItemIdx;
use crate::lr_common::StateIdx;

#[derive(Debug)]
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
