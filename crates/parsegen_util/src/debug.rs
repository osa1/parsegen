use crate::{NodeArena, NodeIdx, NodeKind};

use std::fmt::Debug;

pub fn print_parse_tree<T: Debug>(
    arena: &NodeArena<T, usize>,
    node_idx: NodeIdx,
    nt_names: &'static [&'static str],
) {
    let mut stack: Vec<(NodeIdx, usize)> = vec![(node_idx, 0)];

    loop {
        let (node_idx, depth) = match stack.pop() {
            Some(work) => work,
            None => return,
        };

        let node = arena.get(node_idx);

        match &node.kind {
            NodeKind::NonTerminal(nt) => {
                print_indent(depth * 4);
                println!("NT({}) {}", node_idx, nt_names[*nt]);

                if let Some(next) = node.next {
                    stack.push((next, depth));
                }

                if let Some(child) = node.child {
                    stack.push((child, depth + 1));
                }
            }
            NodeKind::Terminal(t) => {
                print_indent(depth * 4);
                println!("T({}) {:?}", node_idx, t);

                if let Some(next) = node.next {
                    stack.push((next, depth));
                }
            }
        }
    }
}

static WS: &str = "                                    \
                                                       \
                                                       ";

fn print_indent(amt: usize) {
    print!("{}", &WS[..amt]);
}
