mod debug;
mod iter;

pub use debug::print_parse_tree;
pub use iter::{
    arena_terminal_iter_to_arena_iter, lexer_to_arena_iter, ArenaIter, ArenaTerminalIter,
    TerminalIter,
};

use std::num::NonZeroU32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeIdx(NonZeroU32);

#[derive(Debug)]
pub struct NodeArena<T, NT> {
    nodes: Vec<NodeInfo<T, NT>>,
}

#[derive(Debug)]
pub struct NodeInfo<T, NT> {
    pub kind: NodeKind<T, NT>,
    pub next: Option<NodeIdx>,
    pub prev: Option<NodeIdx>,
    pub parent: Option<NodeIdx>,
    pub child: Option<NodeIdx>,
    pub changed: bool,
}

#[derive(Debug)]
pub enum NodeKind<T, NT> {
    NonTerminal(NT),
    Terminal(T),
}

impl<T, NT> NodeArena<T, NT> {
    pub fn new() -> Self {
        NodeArena { nodes: Vec::new() }
    }

    pub fn iter_terminals(&self, root: NodeIdx) -> ArenaTerminalIter<T> {
        ArenaTerminalIter::new(root)
    }

    pub fn new_node(&mut self, node: NodeKind<T, NT>) -> NodeIdx {
        let node_idx = self.nodes.len() + 1;
        self.nodes.push(NodeInfo::new(node));
        NodeIdx(NonZeroU32::new(node_idx as u32).unwrap())
    }

    pub fn get(&self, idx: NodeIdx) -> &NodeInfo<T, NT> {
        &self.nodes[idx.0.get() as usize - 1]
    }

    pub fn get_mut(&mut self, idx: NodeIdx) -> &mut NodeInfo<T, NT> {
        &mut self.nodes[idx.0.get() as usize - 1]
    }

    /// Updates `next` of `idx` and `prev` of `idx`'s `next`
    pub fn set_next(&mut self, idx: NodeIdx, next: Option<NodeIdx>) {
        if let Some(old_next) = self.get_mut(idx).next.take() {
            self.get_mut(old_next).prev = None;
        }

        if let Some(new_next) = next {
            self.get_mut(new_next).prev = Some(idx);
        }

        self.get_mut(idx).next = next;
    }

    /// Updates `prev` of `idx` and `next` of `idx`s `prev`
    pub fn set_prev(&mut self, idx: NodeIdx, prev: Option<NodeIdx>) {
        if let Some(old_prev) = self.get_mut(idx).prev.take() {
            self.get_mut(old_prev).next = None;
        }

        if let Some(new_prev) = prev {
            self.get_mut(new_prev).next = Some(idx);
        }

        self.get_mut(idx).prev = prev;
    }

    pub fn leftmost_terminal(&self, idx: NodeIdx) -> Option<NodeIdx> {
        let node = self.get(idx);
        match node.kind {
            NodeKind::NonTerminal(_) => match node.child {
                Some(child) => match self.leftmost_terminal(child) {
                    Some(node) => Some(node),
                    None => match node.next {
                        Some(node) => self.leftmost_terminal(node),
                        None => None,
                    },
                },
                None => match node.next {
                    Some(node) => self.leftmost_terminal(node),
                    None => None,
                },
            },
            NodeKind::Terminal(_) => Some(idx),
        }
    }
}

impl<T: std::fmt::Debug, NT: std::fmt::Debug> NodeArena<T, NT> {
    pub fn get_terminal(&self, idx: NodeIdx) -> &T {
        match &self.get(idx).kind {
            NodeKind::NonTerminal(nt) => {
                panic!("get_terminal: node index is for non-terminal {:?}", nt)
            }
            NodeKind::Terminal(token) => token,
        }
    }

    pub fn get_non_terminal(&self, idx: NodeIdx) -> &NT {
        match &self.get(idx).kind {
            NodeKind::NonTerminal(nt) => nt,
            NodeKind::Terminal(t) => panic!("get_non_terminal: node index is for terminal {:?}", t),
        }
    }
}

impl<T, NT> NodeInfo<T, NT> {
    pub fn new(kind: NodeKind<T, NT>) -> Self {
        NodeInfo {
            kind,
            next: None,
            prev: None,
            parent: None,
            child: None,
            changed: false,
        }
    }

    pub fn is_terminal(&self) -> bool {
        matches!(self.kind, NodeKind::Terminal(_))
    }

    pub fn is_non_terminal(&self) -> bool {
        matches!(self.kind, NodeKind::NonTerminal(_))
    }
}

#[test]
fn node_terminal_iter_1() {
    let mut arena: NodeArena<u32, u32> = NodeArena::new();
    let root = arena.new_node(NodeKind::Terminal(0));

    let nts: Vec<NodeIdx> = arena.iter_terminals(root).collect(&mut arena);
    assert_eq!(nts, vec![root]);
}

#[test]
fn node_terminal_iter_2() {
    let mut arena: NodeArena<u32, u32> = NodeArena::new();

    let root = arena.new_node(NodeKind::NonTerminal(0));
    let node_1 = arena.new_node(NodeKind::Terminal(0));
    let node_2 = arena.new_node(NodeKind::Terminal(1));

    arena.get_mut(root).child = Some(node_1);
    arena.get_mut(node_1).next = Some(node_2);

    let nts: Vec<NodeIdx> = arena.iter_terminals(root).collect(&mut arena);
    assert_eq!(nts, vec![node_1, node_2]);
}

#[test]
fn node_terminal_iter_3() {
    let mut arena: NodeArena<u32, u32> = NodeArena::new();

    let root = arena.new_node(NodeKind::NonTerminal(0));
    let node_1 = arena.new_node(NodeKind::Terminal(0));
    let node_2 = arena.new_node(NodeKind::NonTerminal(2));
    let node_3 = arena.new_node(NodeKind::Terminal(2));

    arena.get_mut(root).child = Some(node_1);
    arena.get_mut(node_1).next = Some(node_2);
    arena.get_mut(node_2).child = Some(node_3);

    let nts: Vec<NodeIdx> = arena.iter_terminals(root).collect(&mut arena);
    assert_eq!(nts, vec![node_1, node_3]);
}
