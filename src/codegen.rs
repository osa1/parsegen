use crate::grammar::{Grammar, NonTerminal, NonTerminalIdx, Production, Symbol};

use proc_macro2::TokenStream;
use quote::quote;

pub fn build_inefficient_recognizer<A>(grammar: Grammar<char, A>) -> TokenStream {
    let mut non_terminal_vec_elems: Vec<TokenStream> = vec![];

    for NonTerminal {
        non_terminal,
        productions,
    } in grammar.non_terminals
    {
        let mut productions_vec_elems: Vec<TokenStream> = vec![];
        for Production { symbols, .. } in productions {
            let mut symbols_vec_elems: Vec<TokenStream> = vec![];
            for symbol in symbols {
                match symbol {
                    Symbol::NonTerminal(NonTerminalIdx(nt_idx)) => {
                        symbols_vec_elems.push(quote!(
                            Symbol::NonTerminal(NonTerminalIdx(#nt_idx))
                        ));
                    }
                    Symbol::Terminal(char) => {
                        symbols_vec_elems.push(quote!(
                            Symbol::Terminal(#char)
                        ));
                    }
                }
            }
            productions_vec_elems.push(quote!(Production {
                symbols: vec![#(#symbols_vec_elems),*]
            }));
        }

        non_terminal_vec_elems.push(quote!(
            NonTerminal {
                name: #non_terminal.to_owned(),
                productions: vec![#(#productions_vec_elems),*],
            }
        ));
    }

    // let initial_nt_idx = grammar.init.unwrap().0;
    let initial_nt_idx: TokenStream = quote!(NonTerminalIdx(0));

    quote!(
        #[derive(std::cmp::PartialEq, std::cmp::Eq, std::cmp::PartialOrd, std::cmp::Ord, std::hash::Hash, std::clone::Clone, std::marker::Copy)]
        struct NonTerminalIdx(u32);

        // NB. Not globally unique, local to the non-terminal!
        #[derive(std::cmp::PartialEq, std::cmp::Eq, std::cmp::PartialOrd, std::cmp::Ord, std::hash::Hash, std::clone::Clone, std::marker::Copy)]
        struct ProductionIdx(u32);

        #[derive(std::cmp::PartialEq, std::cmp::Eq, std::cmp::PartialOrd, std::cmp::Ord, std::hash::Hash, std::clone::Clone, std::marker::Copy)]
        struct EarleyItem {
            /// The non-terminal for the production of this item
            non_terminal: NonTerminalIdx,

            /// The production
            production: ProductionIdx,

            /// Current position in the production
            position: u32,

            /// Index of the Earley set that this item was created from (via the "scanner" rule)
            set_idx: u32,
        }

        #[derive(std::default::Default)]
        struct EarleySet {
            items: fxhash::FxHashSet<EarleyItem>,
        }

        struct EarleySetDisplay<'set, 'non_terminals> {
            set: &'set EarleySet,
            non_terminals: &'non_terminals [NonTerminal],
        }

        struct EarleyItemDisplay<'non_terminals> {
            item: EarleyItem,
            non_terminals: &'non_terminals [NonTerminal],
        }

        impl<'set, 'non_terminals> std::fmt::Display for EarleySetDisplay<'set, 'non_terminals> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{{")?;
                for (item_idx, item) in self.set.items.iter().copied().enumerate() {
                    (EarleyItemDisplay { item, non_terminals: self.non_terminals }).fmt(f)?;
                    if item_idx != self.set.items.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")
            }
        }

        impl<'non_terminals> std::fmt::Display for EarleyItemDisplay<'non_terminals> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(
                    f,
                    "[{} -> ",
                    self.non_terminals[self.item.non_terminal.0 as usize].name,
                )?;

                let production = &self.non_terminals[self.item.non_terminal.0 as usize].productions[self.item.production.0 as usize];
                let production_symbols = &production.symbols;
                for (symbol_idx, symbol) in production_symbols.iter().enumerate() {
                    if symbol_idx == self.item.position as usize {
                        write!(f, "|")?;
                    }
                    match symbol {
                        Symbol::NonTerminal(nt_idx) => {
                            let nt = &self.non_terminals[nt_idx.0 as usize];
                            <str as std::fmt::Display>::fmt(&nt.name, f)?;
                        }
                        Symbol::Terminal(char) => {
                            <char as std::fmt::Debug>::fmt(char, f)?;
                        }
                    }
                    if symbol_idx != production_symbols.len() - 1 {
                        write!(f, " ")?;
                    }
                }
                if self.item.position as usize == production_symbols.len() {
                    write!(f, "|")?;
                }
                write!(f, ",{}]", self.item.set_idx)
            }
        }

        struct NonTerminal {
            name: String,
            productions: Vec<Production>,
        }

        struct Production {
            symbols: Vec<Symbol>,
        }

        enum Symbol {
            NonTerminal(NonTerminalIdx),
            Terminal(char),
        }

        fn recognize(input: &mut dyn Iterator<Item = char>) -> bool {
            let non_terminals: Vec<NonTerminal> = vec![#(#non_terminal_vec_elems),*];

            let mut state: Vec<EarleySet> = vec![];

            // Create an initial set with productions of the initial non-terminal
            {
                let mut initial_items: fxhash::FxHashSet<EarleyItem> = std::default::Default::default();

                let initial_nt = &non_terminals[0];
                for (production_idx, _) in initial_nt.productions.iter().enumerate() {
                    let not_exists = initial_items.insert(EarleyItem {
                        non_terminal: #initial_nt_idx,
                        production: ProductionIdx(production_idx as u32),
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
                    updated |= predictor(&mut state[token_idx], token_idx as u32, &non_terminals);
                    updated |= completer(&mut state, token_idx as u32, &non_terminals);

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
                        &non_terminals,
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
                    updated |= predictor(&mut state[state_idx], state_idx as u32, &non_terminals);
                    updated |= completer(&mut state, state_idx as u32, &non_terminals);
                }
            }

            println!("Final states:");
            for (set_idx, set) in state.iter().enumerate() {
                println!("{:>4}: {}", set_idx, EarleySetDisplay { set, non_terminals: &non_terminals });
            }

            // There should be an item `[S -> ... |, i]` in the last set where `S` is the initial
            // non-terminal
            for EarleyItem {
                non_terminal,
                production,
                position,
                set_idx,
            } in &state[state_idx].items
            {
                if *set_idx == 0 && *non_terminal == NonTerminalIdx(0) {
                    let prod = &non_terminals[non_terminal.0 as usize].productions[production.0 as usize];
                    if *position as usize == prod.symbols.len() {
                        println!("Solution={}", EarleyItemDisplay {
                            item: EarleyItem {
                                non_terminal: *non_terminal,
                                production: *production,
                                position: *position,
                                set_idx: *set_idx
                            }, non_terminals: &non_terminals });
                        return true;
                    }
                }
            }

            false
        }


        fn predictor(
            current_set: &mut EarleySet,
            current_set_idx: u32,
            non_terminals: &[NonTerminal],
        ) -> bool {
            println!("predictor({})", current_set_idx);
            println!(
                "{:>4}: {}",
                current_set_idx,
                EarleySetDisplay {
                    set: current_set,
                    non_terminals,
                }
            );

            // New items are added here as we iterate the current items
            let mut new_items: fxhash::FxHashSet<EarleyItem> = Default::default();

            for EarleyItem {
                non_terminal,
                production,
                position,
                set_idx: _,
            } in current_set.items.iter()
            {
                let prod = &non_terminals[non_terminal.0 as usize].productions[production.0 as usize];
                let prod_syms = &prod.symbols;
                if *position as usize == prod_syms.len() {
                    // `completer` will take care of this
                    continue;
                }

                if let Symbol::NonTerminal(non_terminal_idx) = prod_syms[*position as usize] {
                    let nt = &non_terminals[non_terminal_idx.0 as usize];
                    for (prod_idx, _) in nt.productions.iter().enumerate() {
                        new_items.insert(EarleyItem {
                            non_terminal: non_terminal_idx,
                            production: ProductionIdx(prod_idx as u32),
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
                    non_terminals,
                }
            );
            println!();

            updated
        }


        fn completer(state: &mut [EarleySet], current_set_idx: u32, non_terminals: &[NonTerminal]) -> bool {
            println!("completer({})", current_set_idx);
            for (set_idx, set) in state.iter().enumerate() {
                println!("{:>4}: {}", set_idx, EarleySetDisplay { set, non_terminals });
            }

            // New items are added here as we iterate the current set
            let mut new_items: fxhash::FxHashSet<EarleyItem> = Default::default();

            let current_set = &state[current_set_idx as usize];
            for EarleyItem {
                non_terminal,
                production,
                position,
                set_idx,
            } in current_set.items.iter()
            {
                let prod = &non_terminals[non_terminal.0 as usize].productions[production.0 as usize];
                let prod_syms = &prod.symbols;

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
                    // No need for this check as the "is complete" check below also handles this case
                    // if parent_item_idx == item_idx {
                    //     continue;
                    // }

                    let parent_prod = &non_terminals[parent_non_terminal.0 as usize].productions[parent_production.0 as usize];
                    let parent_prod_syms = &parent_prod.symbols;

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
                println!("{:>4}: {}", set_idx, EarleySetDisplay { set, non_terminals });
            }
            println!();

            updated
        }

        fn scanner(
            current_set: &mut EarleySet,
            current_set_idx: u32,
            next_set: &mut EarleySet,
            non_terminals: &[NonTerminal],
            token: char,
        ) -> bool {
            println!("scanner({}, {:?})", current_set_idx, token);
            println!(
                "{:>4}: {}",
                current_set_idx,
                EarleySetDisplay {
                    set: current_set,
                    non_terminals,
                }
            );
            println!(
                "{:>4}: {}",
                current_set_idx + 1,
                EarleySetDisplay {
                    set: next_set,
                    non_terminals,
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
                let prod = &non_terminals[non_terminal.0 as usize].productions[production.0 as usize];
                let prod_syms = &prod.symbols;

                if *position as usize == prod_syms.len() {
                    // `completer` will take care of this
                    continue;
                }

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
                    non_terminals,
                }
            );
            println!(
                "{:>4}: {}",
                current_set_idx + 1,
                EarleySetDisplay {
                    set: next_set,
                    non_terminals,
                }
            );
            println!();

            updated
        }

    )
    .into()
}
