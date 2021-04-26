use crate::earley::{EarleyItem, EarleySet, EarleySetDisplay};
use crate::grammar::{Grammar, Symbol};

use fxhash::FxHashSet;

pub fn simulate<A>(grammar: &Grammar<char, A>, input: &mut dyn Iterator<Item = char>) -> bool {
    let mut state: Vec<EarleySet> = vec![];

    // Create an initial set with productions of the initial non-terminal
    {
        let mut initial_items: FxHashSet<EarleyItem> = Default::default();

        let initial_nt_idx = grammar.get_init();
        let initial_nt = grammar.get_non_terminal(initial_nt_idx);
        for production_idx in initial_nt.productions().iter().copied() {
            let not_exists = initial_items.insert(EarleyItem {
                non_terminal: initial_nt_idx,
                production: production_idx,
                position: 0,
                set_idx: 0,
            });
            assert!(not_exists);
        }

        state.push(EarleySet {
            items: initial_items,
        });
    }

    for (token_idx, token) in input.enumerate() {
        let mut updated = true;
        while updated {
            updated = false;
            updated |= predictor(&mut state[token_idx], token_idx as u32, grammar);
            updated |= completer(&mut state, token_idx as u32, grammar);

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
            updated |= predictor(&mut state[state_idx], state_idx as u32, grammar);
            updated |= completer(&mut state, state_idx as u32, grammar);
        }
    }

    // There should be an item `[S -> ... |, i]` in the last set where `S` is the initial
    // non-terminal
    for EarleyItem {
        non_terminal,
        production,
        position,
        set_idx: _,
    } in &state[state_idx].items
    {
        if *non_terminal == grammar.get_init() {
            let prod = grammar.get_production(*production);
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
    let mut new_items: FxHashSet<EarleyItem> = Default::default();

    for EarleyItem {
        non_terminal: _,
        production,
        position,
        set_idx: _,
    } in current_set.items.iter()
    {
        let prod = grammar.get_production(*production);
        let prod_syms = prod.symbols();
        if *position as usize == prod_syms.len() {
            // `completer` will take care of this
            continue;
        }

        if let Symbol::NonTerminal(non_terminal_idx) = prod_syms[*position as usize] {
            let nt = grammar.get_non_terminal(non_terminal_idx);
            for prod in nt.productions() {
                new_items.insert(EarleyItem {
                    non_terminal: non_terminal_idx,
                    production: *prod,
                    position: 0,
                    set_idx: current_set_idx,
                });
            }
        }
    }

    let mut updated = false;
    for new_item in new_items {
        updated |= current_set.items.insert(new_item);
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
    println!();

    updated
}

/// The `completer` rule in the paper "Practical Earley Parsing". Returns whether we updated the
/// set.
///
/// In my words: when we see an item `[X -> ... |, j]` in the current set, we find items
/// `[A -> ... |X ..., k]` in set `j`, and add `[A -> ... X |..., k]` to the current set.
fn completer<A>(state: &mut [EarleySet], current_set_idx: u32, grammar: &Grammar<char, A>) -> bool {
    println!("completer({})", current_set_idx);
    for (set_idx, set) in state.iter().enumerate() {
        println!("{:>4}: {}", set_idx, EarleySetDisplay { set, grammar });
    }

    // New items are added here as we iterate the current set
    let mut new_items: FxHashSet<EarleyItem> = Default::default();

    let current_set = &state[current_set_idx as usize];
    for EarleyItem {
        non_terminal,
        production,
        position,
        set_idx,
    } in current_set.items.iter()
    {
        let prod = grammar.get_production(*production);
        let prod_syms = prod.symbols();

        if *position as usize != prod_syms.len() {
            // Not complete, skip
            continue;
        }

        let parent_set = &state[*set_idx as usize];
        for EarleyItem {
            non_terminal: parent_non_terminal,
            production: parent_production,
            position: parent_position,
            set_idx: parent_set_idx,
        } in parent_set.items.iter()
        {
            let parent_prod = grammar.get_production(*parent_production);
            let parent_prod_syms = parent_prod.symbols();

            if let Symbol::NonTerminal(parent_nt_idx) = parent_prod_syms[*parent_position as usize]
            {
                if parent_nt_idx == *non_terminal {
                    new_items.insert(EarleyItem {
                        non_terminal: *parent_non_terminal,
                        production: *parent_production,
                        position: parent_position + 1, // skip completed non-terminal
                        set_idx: *parent_set_idx,
                    });
                }
            }
        }
    }

    let mut updated = false;
    for new_item in new_items {
        updated |= state[current_set_idx as usize].items.insert(new_item);
    }

    println!("=>");
    for (set_idx, set) in state.iter().enumerate() {
        println!("{:>4}: {}", set_idx, EarleySetDisplay { set, grammar });
    }
    println!();

    updated
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
        non_terminal,
        production,
        position,
        set_idx,
    } in current_set.items.iter()
    {
        let prod = grammar.get_production(*production);
        let prod_syms = prod.symbols();

        if let Symbol::Terminal(t) = &prod_syms[*position as usize] {
            if *t == token {
                updated |= next_set.items.insert(EarleyItem {
                    non_terminal: *non_terminal,
                    production: *production,
                    position: *position + 1, // skip matched token
                    set_idx: *set_idx,
                });
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

    assert!(simulate(&grammar, &mut "n".chars()));
}
