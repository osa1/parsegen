use crate::collections::{Map, Set};
use crate::grammar::TerminalIdx;
use crate::lr_common::StateIdx;

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord, Hash)]
pub struct ItemIdx(pub u32);

/// A lane table records lookahead contributions of states to LR items.
///
/// For example, if we have a reduce item `[X -> ...|]` in state N and in a predecessor state M we
/// see an item like `[Y -> ... | X a ...]`, state M adds lookahead 'a' to the reduce item in state
/// N. Lane tables stores this information in a map from item and state index pairs to lookahead
/// sets.
#[derive(Debug)]
pub struct LaneTable {
    lookaheads: Map<(StateIdx, ItemIdx), Set<TerminalIdx>>,
}

impl LaneTable {
    pub fn add_lookahead(&mut self, state: StateIdx, item: ItemIdx, lookahead: TerminalIdx) {
        self.lookaheads
            .entry((state, item))
            .or_default()
            .insert(lookahead);
    }
}
