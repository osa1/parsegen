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
}
