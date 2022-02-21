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

    pub fn iter_terminals(&self, root: NodeIdx) -> TerminalIter<T, NT> {
        TerminalIter {
            arena: self,
            stack: vec![root],
        }
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
}

impl<T, NT> NodeInfo<T, NT> {
    pub fn new(kind: NodeKind<T, NT>) -> Self {
        NodeInfo {
            kind,
            next: None,
            prev: None,
            parent: None,
            child: None,
        }
    }

    pub fn is_terminal(&self) -> bool {
        matches!(self.kind, NodeKind::Terminal(_))
    }

    pub fn is_non_terminal(&self) -> bool {
        matches!(self.kind, NodeKind::NonTerminal(_))
    }
}

pub struct TerminalIter<'a, T, NT> {
    arena: &'a NodeArena<T, NT>,
    stack: Vec<NodeIdx>,
}

impl<'a, T, NT> Iterator for TerminalIter<'a, T, NT> {
    type Item = NodeIdx;

    fn next(&mut self) -> Option<NodeIdx> {
        let current_node = match self.stack.pop() {
            Some(node_idx) => node_idx,
            None => return None,
        };

        match self.arena.get(current_node).child {
            Some(child) => {
                debug_assert!(self.arena.get(current_node).is_non_terminal());
                self.stack.push(child);
                self.next()
            }
            None => {
                if let Some(next) = self.arena.get(current_node).next {
                    self.stack.push(next);
                }
                Some(current_node)
            }
        }
    }
}

#[test]
fn node_terminal_iter_1() {
    let mut arena: NodeArena<u32, u32> = NodeArena::new();
    let root = arena.new_node(NodeKind::Terminal(0));

    let nts: Vec<NodeIdx> = arena.iter_terminals(root).collect();
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

    let nts: Vec<NodeIdx> = arena.iter_terminals(root).collect();
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

    let nts: Vec<NodeIdx> = arena.iter_terminals(root).collect();
    assert_eq!(nts, vec![node_1, node_3]);
}
