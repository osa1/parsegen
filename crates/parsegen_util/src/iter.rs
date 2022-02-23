use crate::{NodeArena, NodeIdx, NodeKind};

pub trait TerminalIter {
    type Terminal;

    fn next<NT>(&mut self, arena: &mut NodeArena<Self::Terminal, NT>) -> Option<NodeIdx>;

    fn collect<NT, B: FromIterator<NodeIdx>>(self, arena: &mut NodeArena<Self::Terminal, NT>) -> B
    where
        Self: Sized,
    {
        FromIterator::from_iter(TerminalIterToIter { iter: self, arena })
    }
}

// Converts a terminal iterator into `TerminalIter`
pub struct IterToTerminalIter<T, I: Iterator<Item = T>> {
    iter: I,
}

impl<T, I: Iterator<Item = T>> TerminalIter for IterToTerminalIter<T, I> {
    type Terminal = T;

    fn next<NT>(&mut self, arena: &mut NodeArena<T, NT>) -> Option<NodeIdx> {
        match self.iter.next() {
            Some(t) => Some(arena.new_node(NodeKind::Terminal(t))),
            None => None,
        }
    }
}

// Converts a `TerminalIter` into `Iterator<Item = NodeIdx>`
pub struct TerminalIterToIter<'a, T, NT, I: TerminalIter<Terminal = T>> {
    iter: I,
    arena: &'a mut NodeArena<T, NT>,
}

impl<'a, T, NT, I: TerminalIter<Terminal = T>> Iterator for TerminalIterToIter<'a, T, NT, I> {
    type Item = NodeIdx;

    fn next(&mut self) -> Option<NodeIdx> {
        self.iter.next(self.arena)
    }
}

pub struct ArenaTerminalIter<T> {
    stack: Vec<NodeIdx>,
    _t: std::marker::PhantomData<T>,
}

impl<T> ArenaTerminalIter<T> {
    pub fn new(root: NodeIdx) -> Self {
        ArenaTerminalIter {
            stack: vec![root],
            _t: Default::default(),
        }
    }
}

impl<T> TerminalIter for ArenaTerminalIter<T> {
    type Terminal = T;

    fn next<NT>(&mut self, arena: &mut NodeArena<T, NT>) -> Option<NodeIdx> {
        let current_node = match self.stack.pop() {
            Some(node_idx) => node_idx,
            None => return None,
        };

        match arena.get(current_node).child {
            Some(child) => {
                debug_assert!(arena.get(current_node).is_non_terminal());
                self.stack.push(child);
                self.next(arena)
            }
            None => {
                if let Some(next) = arena.get(current_node).next {
                    self.stack.push(next);
                }
                Some(current_node)
            }
        }
    }
}
