use crate::earley::{EarleyItem, EarleyItemIdx, EarleySet, EarleySetDisplay};
use crate::grammar::{Grammar, NonTerminalIdx, ProductionIdx, Symbol};

use fxhash::{FxHashMap, FxHashSet};

struct EarleyItemGraph {
    next_id: EarleyItemIdx,

    /// Maps items to their child items
    item_child: FxHashMap<EarleyItemIdx, FxHashSet<EarleyItemIdx>>,

    /// Maps item indices to items
    items: Vec<EarleyItem>,
}

impl EarleyItemGraph {
    fn new() -> Self {
        EarleyItemGraph {
            next_id: EarleyItemIdx(0),
            item_child: Default::default(),
            items: vec![],
        }
    }

    fn fresh_idx(&mut self) -> EarleyItemIdx {
        let ret = self.next_id;
        self.next_id = EarleyItemIdx(ret.0 + 1);
        ret
    }

    fn add_item(
        &mut self,
        non_terminal: NonTerminalIdx,
        production: ProductionIdx,
        position: u32,
        set_idx: u32,
    ) -> EarleyItemIdx {
        let idx = self.fresh_idx();
        let item = EarleyItem {
            idx,
            non_terminal,
            production,
            position,
            set_idx,
        };
        self.items.push(item);
        idx
    }

    fn add_child(&mut self, parent: EarleyItemIdx, child: EarleyItemIdx) -> bool {
        println!("add child {} -> {}", parent.0, child.0);
        let not_exists = self.item_child.entry(parent).or_default().insert(child);
        // Invalid assertion as we visit the same state until fixpoint
        // assert!(not_exists);
        not_exists
    }
}

pub fn simulate<A>(grammar: &Grammar<char, A>, input: &mut dyn Iterator<Item = char>) -> bool {
    let mut state: Vec<EarleySet> = vec![];

    let mut item_graph = EarleyItemGraph::new();

    // Create an initial set with productions of the initial non-terminal
    {
        let mut initial_items: EarleySet = Default::default();

        let initial_nt_idx = grammar.get_init();
        let initial_nt = grammar.get_non_terminal(initial_nt_idx);
        for production_idx in initial_nt.production_indices() {
            match initial_items.get_idx(initial_nt_idx, production_idx, 0, 0) {
                Some(_) => {}
                None => {
                    let idx = item_graph.add_item(initial_nt_idx, production_idx, 0, 0);
                    initial_items.insert(EarleyItem {
                        idx,
                        non_terminal: initial_nt_idx,
                        production: production_idx,
                        position: 0,
                        set_idx: 0,
                    });
                }
            }
        }
        state.push(initial_items);
    }

    for (token_idx, token) in input.enumerate() {
        let mut updated = true;
        while updated {
            updated = false;
            updated |= predictor(
                &mut state[token_idx],
                token_idx as u32,
                grammar,
                &mut item_graph,
            );
            updated |= completer(&mut state, token_idx as u32, grammar, &mut item_graph);

            // For the scanner rule make sure we have the next set available
            let mut next_set = if state.len() == token_idx + 1 {
                // Create new state
                Default::default()
            } else {
                // Use existing state
                assert!(state.len() == token_idx + 2);
                state.pop().unwrap()
            };

            updated |= scanner(
                &mut state[token_idx],
                token_idx as u32,
                &mut next_set,
                grammar,
                &mut item_graph,
                token,
            );

            state.push(next_set);
        }
    }

    // Processed the input, run predictor and completer on the final state
    let state_idx = state.len() - 1;
    {
        let mut updated = true;
        while updated {
            updated = false;
            updated |= predictor(
                &mut state[state_idx],
                state_idx as u32,
                grammar,
                &mut item_graph,
            );
            updated |= completer(&mut state, state_idx as u32, grammar, &mut item_graph);
        }
    }

    println!("Final states:");
    for (set_idx, set) in state.iter().enumerate() {
        println!("{:>4}: {}", set_idx, EarleySetDisplay { set, grammar });
    }

    println!(
        "{}",
        crate::graphviz::generate_graphviz(grammar, &state, &item_graph.item_child)
    );

    // There should be an item `[S -> ... |, i]` in the last set where `S` is the initial
    // non-terminal
    for EarleyItem {
        idx: _,
        non_terminal,
        production,
        position,
        set_idx,
    } in &state[state_idx].items
    {
        if *set_idx == 0 && *non_terminal == grammar.get_init() {
            let prod = grammar.get_production(*non_terminal, *production);
            if *position as usize == prod.symbols().len() {
                return true;
            }
        }
    }

    false
}

/// The `predictor` rule in the paper "Practical Earley Parsing". Returns whether we updated the
/// set.
///
/// In my words: whenever we see an item `[A -> ... |X ..., j]` in the current set, we add items
/// for the productions of `X` to the current set, with the current set index as the item set
/// index.
fn predictor<A>(
    current_set: &mut EarleySet,
    current_set_idx: u32,
    grammar: &Grammar<char, A>,
    graph: &mut EarleyItemGraph,
) -> bool {
    println!("predictor({})", current_set_idx);
    println!(
        "{:>4}: {}",
        current_set_idx,
        EarleySetDisplay {
            set: current_set,
            grammar
        }
    );

    // New items are added here as we iterate the current items
    let mut new_items: EarleySet = Default::default();

    for EarleyItem {
        idx: parent_idx,
        non_terminal,
        production,
        position,
        set_idx: _,
    } in current_set.items.iter()
    {
        let prod = grammar.get_production(*non_terminal, *production);
        let prod_syms = prod.symbols();
        if *position as usize == prod_syms.len() {
            // `completer` will take care of this
            continue;
        }

        // [A -> ... |B ..., i]
        if let Symbol::NonTerminal(non_terminal_idx) = prod_syms[*position as usize] {
            let nt = grammar.get_non_terminal(non_terminal_idx);
            for prod in nt.production_indices() {
                match current_set.get_idx(non_terminal_idx, prod, 0, current_set_idx) {
                    Some(_) => {}
                    None => match new_items.get_idx(non_terminal_idx, prod, 0, current_set_idx) {
                        Some(_) => {}
                        None => {
                            let idx = graph.add_item(non_terminal_idx, prod, 0, current_set_idx);
                            new_items.insert(EarleyItem {
                                idx,
                                non_terminal: non_terminal_idx,
                                production: prod,
                                position: 0,
                                set_idx: current_set_idx,
                            });
                            graph.add_child(*parent_idx, idx);
                        }
                    },
                }
            }
        }
    }

    if new_items.is_empty() {
        return false;
    }

    current_set.merge(new_items);

    println!("=>");
    println!(
        "{:>4}: {}",
        current_set_idx,
        EarleySetDisplay {
            set: current_set,
            grammar
        }
    );
    println!();

    true
}

/// The `completer` rule in the paper "Practical Earley Parsing". Returns whether we updated the
/// set.
///
/// In my words: when we see an item `[X -> ... |, j]` in the current set, we find items
/// `[A -> ... |X ..., k]` in set `j`, and add `[A -> ... X |..., k]` to the current set.
fn completer<A>(
    state: &mut [EarleySet],
    current_set_idx: u32,
    grammar: &Grammar<char, A>,
    graph: &mut EarleyItemGraph,
) -> bool {
    println!("completer({})", current_set_idx);
    for (set_idx, set) in state.iter().enumerate() {
        println!("{:>4}: {}", set_idx, EarleySetDisplay { set, grammar });
    }

    // New items are added here as we iterate the current set
    let mut new_items: EarleySet = Default::default();

    let current_set = &state[current_set_idx as usize];
    for EarleyItem {
        idx: _,
        non_terminal,
        production,
        position,
        set_idx,
    } in current_set.items.iter()
    {
        let prod = grammar.get_production(*non_terminal, *production);
        let prod_syms = prod.symbols();

        // [A -> ... |, i]
        if *position as usize != prod_syms.len() {
            // Not complete, skip
            continue;
        }

        let parent_set = &state[*set_idx as usize];
        for EarleyItem {
            idx: parent_idx,
            non_terminal: parent_non_terminal,
            production: parent_production,
            position: parent_position,
            set_idx: parent_set_idx,
        } in parent_set.items.iter()
        {
            // No need for this check as the "is complete" check below also handles this case
            // if parent_item_idx == item_idx {
            //     continue;
            // }

            let parent_prod = grammar.get_production(*parent_non_terminal, *parent_production);
            let parent_prod_syms = parent_prod.symbols();

            if *parent_position as usize == parent_prod_syms.len() {
                continue;
            }

            // println!(
            //     "current_item_idx={}, parent_item_idx={}, parent_position={}, parent_prod_syms={:?}",
            //     item_idx, parent_item_idx, parent_position, parent_prod_syms
            // );

            if let Symbol::NonTerminal(parent_nt_idx) = parent_prod_syms[*parent_position as usize]
            {
                if parent_nt_idx == *non_terminal {
                    match current_set.get_idx(
                        *parent_non_terminal,
                        *parent_production,
                        parent_position + 1,
                        *parent_set_idx,
                    ) {
                        Some(_) => {}
                        None => match new_items.get_idx(
                            *parent_non_terminal,
                            *parent_production,
                            parent_position + 1,
                            *parent_set_idx,
                        ) {
                            Some(_) => {}
                            None => {
                                let idx = graph.add_item(
                                    *parent_non_terminal,
                                    *parent_production,
                                    parent_position + 1,
                                    *parent_set_idx,
                                );
                                new_items.insert(EarleyItem {
                                    idx,
                                    non_terminal: *parent_non_terminal,
                                    production: *parent_production,
                                    position: parent_position + 1, // skip completed non-terminal
                                    set_idx: *parent_set_idx,
                                });
                                graph.add_child(*parent_idx, idx);
                            }
                        },
                    }
                }
            }
        }
    }

    if new_items.is_empty() {
        return false;
    }

    state[current_set_idx as usize].merge(new_items);

    println!("=>");
    for (set_idx, set) in state.iter().enumerate() {
        println!("{:>4}: {}", set_idx, EarleySetDisplay { set, grammar });
    }
    println!();

    true
}

/// The `scanner` rule in the paper "Practical Earley Parsing". Returns whether we updated the next
/// set. (this rule does not update the current set)
///
/// In my words: when we see an item `[A -> ... |a ..., i]` in the current set, and the character
/// is `a`, we add `[A -> ... a| ..., i]` to the next set.
fn scanner<A>(
    current_set: &mut EarleySet,
    current_set_idx: u32,
    next_set: &mut EarleySet,
    grammar: &Grammar<char, A>,
    graph: &mut EarleyItemGraph,
    token: char,
) -> bool {
    println!("scanner({}, {:?})", current_set_idx, token);
    println!(
        "{:>4}: {}",
        current_set_idx,
        EarleySetDisplay {
            set: current_set,
            grammar
        }
    );
    println!(
        "{:>4}: {}",
        current_set_idx + 1,
        EarleySetDisplay {
            set: next_set,
            grammar
        }
    );

    let mut updated = false;

    for EarleyItem {
        idx: parent_idx,
        non_terminal,
        production,
        position,
        set_idx,
    } in current_set.items.iter()
    {
        let prod = grammar.get_production(*non_terminal, *production);
        let prod_syms = prod.symbols();

        // [A -> ... |, i]
        if *position as usize == prod_syms.len() {
            // `completer` will take care of this
            continue;
        }

        // [A -> ... | 'x' ..., i]
        if let Symbol::Terminal(t) = &prod_syms[*position as usize] {
            if *t == token {
                let idx =
                    match next_set.get_idx(*non_terminal, *production, *position + 1, *set_idx) {
                        Some(idx) => idx,
                        None => {
                            let idx =
                                graph.add_item(*non_terminal, *production, *position + 1, *set_idx);
                            next_set.insert(EarleyItem {
                                idx,
                                non_terminal: *non_terminal,
                                production: *production,
                                position: *position + 1, // skip matched token
                                set_idx: *set_idx,
                            });
                            idx
                        }
                    };
                updated |= graph.add_child(*parent_idx, idx);
            }
        }
    }

    println!("=>");
    println!(
        "{:>4}: {}",
        current_set_idx,
        EarleySetDisplay {
            set: current_set,
            grammar
        }
    );
    println!(
        "{:>4}: {}",
        current_set_idx + 1,
        EarleySetDisplay {
            set: next_set,
            grammar
        }
    );
    println!();

    updated
}

// E -> E + E | n
#[test]
fn simulate1() {
    let mut grammar: Grammar<char, ()> = Grammar::new();
    let e_nt_idx = grammar.add_non_terminal("E".to_owned());
    grammar.add_production(
        e_nt_idx,
        vec![
            Symbol::NonTerminal(e_nt_idx),
            Symbol::Terminal('+'),
            Symbol::NonTerminal(e_nt_idx),
        ],
        (),
    );
    grammar.add_production(e_nt_idx, vec![Symbol::Terminal('n')], ());
    grammar.set_init(e_nt_idx);

    // assert!(simulate(&grammar, &mut "n".chars()));
    // assert!(simulate(&grammar, &mut "n+n".chars()));
    assert!(simulate(&grammar, &mut "n+n+n".chars()));
    // assert!(!simulate(&grammar, &mut "n+n+".chars()));
}

// S -> AAAA
// A -> a
// A -> E
// E -> {empty}
#[test]
fn simulate2() {
    let mut grammar: Grammar<char, ()> = Grammar::new();
    let s_nt_idx = grammar.add_non_terminal("S".to_owned());
    let a_nt_idx = grammar.add_non_terminal("A".to_owned());
    let e_nt_idx = grammar.add_non_terminal("E".to_owned());

    grammar.set_init(s_nt_idx);

    grammar.add_production(
        s_nt_idx,
        vec![
            Symbol::NonTerminal(a_nt_idx),
            Symbol::NonTerminal(a_nt_idx),
            Symbol::NonTerminal(a_nt_idx),
            Symbol::NonTerminal(a_nt_idx),
        ],
        (),
    );
    grammar.add_production(a_nt_idx, vec![Symbol::Terminal('a')], ());
    grammar.add_production(a_nt_idx, vec![Symbol::NonTerminal(e_nt_idx)], ());
    grammar.add_production(e_nt_idx, vec![], ());

    assert!(simulate(&grammar, &mut "a".chars()));
    assert!(simulate(&grammar, &mut "aa".chars()));
    assert!(simulate(&grammar, &mut "aaa".chars()));
    assert!(simulate(&grammar, &mut "aaaa".chars()));
    assert!(!simulate(&grammar, &mut "aaaaa".chars()));
}
