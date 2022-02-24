use crate::{NodeArena, NodeIdx, NodeKind};

use std::marker::PhantomData;

pub trait ArenaIter<T, NT> {
    type Item;

    fn next(&mut self, arena: &mut NodeArena<T, NT>) -> Option<Self::Item>;
}

/// Converts a lexer into an iterator that takes a `&mut NodeArena` argument and allocates
/// non-terminals in the arena
pub struct LexerArenaIterAdapter<T, E, I: Iterator<Item = Result<T, E>>> {
    lexer: I,
}

impl<T, NT, E, I: Iterator<Item = Result<T, E>>> ArenaIter<T, NT>
    for LexerArenaIterAdapter<T, E, I>
{
    type Item = Result<NodeIdx, E>;

    fn next(&mut self, arena: &mut NodeArena<T, NT>) -> Option<Result<NodeIdx, E>> {
        match self.lexer.next() {
            Some(Ok(tok)) => Some(Ok(arena.new_node(NodeKind::Terminal(tok)))),
            Some(Err(err)) => Some(Err(err)),
            None => None,
        }
    }
}

pub fn lexer_to_arena_iter<T, NT, E, I>(lexer: I) -> LexerArenaIterAdapter<T, E, I>
where
    I: Iterator<Item = Result<T, E>>,
{
    LexerArenaIterAdapter { lexer }
}

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
    _t: PhantomData<T>,
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

        if arena.get(current_node).is_terminal() {
            debug_assert!(arena.get(current_node).child.is_none());
            if let Some(next) = arena.get(current_node).next {
                self.stack.push(next);
            }
            Some(current_node)
        } else {
            // Node is non-terminal
            if let Some(next) = arena.get(current_node).next {
                self.stack.push(next);
            }

            if let Some(child) = arena.get(current_node).child {
                self.stack.push(child);
            }

            self.next(arena)
        }
    }
}

pub struct ArenaTerminalIterToArenaIter<T, NT, E> {
    iter: ArenaTerminalIter<T>,
    _nt: PhantomData<NT>,
    _e: PhantomData<E>,
}

pub fn arena_terminal_iter_to_arena_iter<T, NT, E>(
    iter: ArenaTerminalIter<T>,
) -> ArenaTerminalIterToArenaIter<T, NT, E> {
    ArenaTerminalIterToArenaIter {
        iter,
        _nt: Default::default(),
        _e: Default::default(),
    }
}

impl<T, NT, E> ArenaIter<T, NT> for ArenaTerminalIterToArenaIter<T, NT, E> {
    type Item = Result<NodeIdx, E>;

    fn next(&mut self, arena: &mut NodeArena<T, NT>) -> Option<Result<NodeIdx, E>> {
        self.iter.next(arena).map(Ok)
    }
}
