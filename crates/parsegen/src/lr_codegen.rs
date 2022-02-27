use crate::ast::TokenEnum;
use crate::codegen::generate_token_kind_type;
use crate::first::generate_first_table;
use crate::grammar::{Grammar, NonTerminalIdx, TerminalIdx};
use crate::lr1::{build_lr1_table, generate_lr1_automaton};
use crate::lr_common::{LRAction, StateIdx};

use fxhash::FxHashMap;
use proc_macro2::{Span, TokenStream};
use quote::quote;

pub fn generate_lr1_parser(grammar: Grammar, tokens: &TokenEnum) -> TokenStream {
    let (token_kind_type_name, token_kind_type_decl) = generate_token_kind_type(tokens);

    let n_terminals = grammar.n_terminals();
    let token_type = &tokens.type_name;

    let first_table = generate_first_table(&grammar);
    let (lr1_automaton, nt_state_indices) = generate_lr1_automaton(&grammar, &first_table);

    let token_lifetimes = &tokens.type_lifetimes;

    // println!(
    //     "{}",
    //     crate::lr1::LR1AutomatonDisplay {
    //         automaton: &lr1_automaton,
    //         grammar: &grammar
    //     }
    // );

    let lr1_table = build_lr1_table(&grammar, &lr1_automaton);

    // println!(
    //     "{}",
    //     crate::lr_common::LRTableDisplay::new(&lr1_table, &grammar)
    // );

    let action_vec = action_table_vec(&grammar, lr1_table.get_action_table(), lr1_table.n_states());

    let action_array_code =
        generate_action_array(&grammar, &action_vec, lr1_table.n_states(), n_terminals);

    let goto_vec = generate_goto_vec(
        lr1_table.get_goto_table(),
        lr1_table.n_states(),
        grammar.n_non_terminals(),
    );

    let goto_array_code =
        generate_goto_array(&goto_vec, lr1_table.n_states(), grammar.non_terminals.len());

    let (token_kind_fn_name, token_kind_fn_decl) =
        crate::codegen::token_kind_fn(&token_kind_type_name, tokens);

    let token_full_type = quote!(#token_type<#(#token_lifetimes,)*>);

    // struct NonTerminal;
    // impl NonTerminal { fn parse() { ... } }
    let parser_structs: Vec<TokenStream> = nt_state_indices
        .into_iter()
        .map(|(non_terminal_idx, parser_state)| {
            let parser_state = u32::try_from(parser_state.as_usize()).unwrap();
            let non_terminal = grammar.get_non_terminal(non_terminal_idx);
            let non_terminal_name_id =
                syn::Ident::new(&non_terminal.non_terminal, Span::call_site());

            quote!(
                pub struct #non_terminal_name_id;

                impl #non_terminal_name_id {
                    pub fn parse<#(#token_lifetimes,)* E: ::std::fmt::Debug + Clone>(
                        arena: &mut NodeArena<#token_full_type, usize>,
                        mut input: impl ::parsegen_util::ArenaIter<#token_full_type, usize, Item=Result<::parsegen_util::NodeIdx, E>>
                    ) -> Result<::parsegen_util::NodeIdx, ParseError_<E>>
                    {
                        parse_generic(
                            arena,
                            input,
                            #parser_state,
                        )
                    }
                }
            )
        })
        .collect();

    quote!(
        #[derive(Clone, Copy)]
        enum LRAction {
            Shift {
                next_state: u32,
            },
            Reduce {
                non_terminal_idx: u16,
                production_idx: u16,
                n_symbols: u16,
            },
            Accept,
        }

        // static ACTION: [[Option<LRAction>; ...]; ...]
        #action_array_code

        // static GOTO: [[Option<u32>; ...]; ...]
        #goto_array_code

        #[derive(Debug, Clone, PartialEq, Eq)]
        pub enum ParseError_<E> {
            Other(E),
        }

        // enum TokenKind { ... }
        #token_kind_type_decl

        // fn token_kind(token: &Token) -> TokenKind { ... }
        #token_kind_fn_decl

        fn parse_generic<#(#token_lifetimes,)* E: ::std::fmt::Debug + Clone>(
            arena: &mut ::parsegen_util::NodeArena<#token_full_type, usize>,
            mut input: impl ::parsegen_util::ArenaIter<#token_full_type, usize, Item=Result<::parsegen_util::NodeIdx, E>>,
            init_state: u32,
        ) -> Result<::parsegen_util::NodeIdx, ParseError_<E>>
        {
            let mut state_stack: Vec<u32> = vec![init_state];
            let mut value_stack: Vec<::parsegen_util::NodeIdx> = vec![];

            let mut node_idx: Option<Result<::parsegen_util::NodeIdx, E>> =
                input.next(arena);

            let mut verifying = false;

            loop {
                debug_assert_eq!(state_stack.len(), value_stack.len() + 1);

                let mut state = *state_stack.last().unwrap() as usize;

                println!("state = {}, node_idx = {:?}", state, node_idx);

                let terminal_idx = match node_idx {
                    None => #n_terminals,
                    Some(Err(err)) => return Err(ParseError_::Other(err.clone())),
                    Some(Ok(node_idx_)) => {
                        if arena.get(node_idx_).is_terminal() {
                            let token = arena.get_terminal(node_idx_);
                            #token_kind_fn_name(token) as usize
                        } else {
                            // Non-terminal
                            if arena.get(node_idx_).changed {
                                input.left_breakdown(arena, node_idx_);
                                node_idx = input.next(arena);
                                continue;
                            } else {
                                // Apply reductions first
                                if let Some(lookahead) = arena.leftmost_terminal(node_idx_) {
                                    let lookahead = arena.get_terminal(lookahead);
                                    let lookahead_kind = #token_kind_fn_name(lookahead) as usize;
                                    while let Some(LRAction::Reduce {
                                        non_terminal_idx,
                                        production_idx,
                                        n_symbols
                                    }) = ACTION[state][lookahead_kind]
                                    {
                                        let nt_value =
                                            arena.new_node(::parsegen_util::NodeKind::NonTerminal(
                                                non_terminal_idx as usize
                                            ));

                                        // Remove last `n_symbols` values
                                        let mut child_iter = value_stack.drain(value_stack.len() - (n_symbols as usize)..);

                                        if let Some(first_child) = child_iter.next() {
                                            arena.get_mut(nt_value).child = Some(first_child);
                                            arena.get_mut(first_child).parent = Some(nt_value);

                                            let mut last_child = first_child;

                                            for child in child_iter {
                                                arena.get_mut(last_child).next = Some(child);
                                                arena.get_mut(child).prev = Some(last_child);
                                                last_child = child;
                                            }
                                        } else {
                                            drop(child_iter);
                                        }

                                        value_stack.push(nt_value);

                                        // Remove last `n_symbols` states
                                        for _ in 0 .. n_symbols {
                                            state_stack.pop();
                                        }

                                        state = *state_stack.last().unwrap() as usize;
                                        match GOTO[state][non_terminal_idx as usize] {
                                            None => panic!("Stuck! (2)"),
                                            Some(next_state) => state_stack.push(next_state),
                                        }
                                    }
                                }

                                // Shift the non-terminal, enter verification mode
                                let nt = *arena.get_non_terminal(node_idx_);

                                match GOTO[state][nt] {
                                    None => {
                                        todo!();
                                    }
                                    Some(next_state) => {
                                        state_stack.push(next_state);
                                        value_stack.push(node_idx_);
                                        verifying = true;
                                        node_idx = input.next(arena);
                                        println!("Shifted non-terminal {}", nt);
                                        continue;
                                    }
                                }
                            }
                        }
                    }
                };
                match ACTION[state][terminal_idx] {
                    None => {
                        if verifying {
                            todo!("right breakdown")
                        }
                        panic!("Stuck! (1) state={}, terminal={}", state, terminal_idx)
                    }
                    Some(LRAction::Shift { next_state }) => {
                        verifying = false;

                        state_stack.push(next_state);
                        let value = node_idx.unwrap().unwrap();

                        value_stack.push(value);

                        node_idx = input.next(arena);
                    }
                    Some(LRAction::Reduce {
                        non_terminal_idx,
                        production_idx,
                        n_symbols,
                    }) => {
                        let nt_value =
                            arena.new_node(::parsegen_util::NodeKind::NonTerminal(
                                non_terminal_idx as usize
                            ));

                        // Remove last `n_symbols` values
                        let mut child_iter = value_stack.drain(value_stack.len() - (n_symbols as usize)..);

                        if let Some(first_child) = child_iter.next() {
                            arena.get_mut(nt_value).child = Some(first_child);
                            arena.get_mut(first_child).parent = Some(nt_value);

                            let mut last_child = first_child;

                            for child in child_iter {
                                arena.get_mut(last_child).next = Some(child);
                                arena.get_mut(child).prev = Some(last_child);
                                last_child = child;
                            }
                        } else {
                            drop(child_iter);
                        }

                        value_stack.push(nt_value);

                        // Remove last `n_symbols` states
                        for _ in 0 .. n_symbols {
                            state_stack.pop();
                        }

                        let state = *state_stack.last().unwrap() as usize;
                        match GOTO[state][non_terminal_idx as usize] {
                            None => panic!("Stuck! (2)"),
                            Some(next_state) => state_stack.push(next_state),
                        }
                    }
                    Some(LRAction::Accept) => break,
                }
            }

            Ok(value_stack.pop().unwrap())
        }

        fn right_breakdown<#(#token_lifetimes,)* E: ::std::fmt::Debug + Clone>(
            arena: &::parsegen_util::NodeArena<#token_full_type, usize>,
            state_stack: &mut Vec<u32>,
            value_stack: &mut Vec<::parsegen_util::NodeIdx>,
        ) -> Result<(), ParseError_<E>> {
            let mut node = value_stack.pop().unwrap();
            state_stack.pop().unwrap();

            while arena.get(node).is_non_terminal() {
                let node_info = arena.get(node);
                let mut child = node_info.child;
                while let Some(child_) = child {
                    // TODO: need parse table with non-terminal keys
                    child = arena.get(child_).next;
                }
                node = value_stack.pop().unwrap();
                state_stack.pop();
            }

            // TODO: shift terminal
            todo!()
        }

        #(#parser_structs)*
    )
}

/// Generates array representation of the action table. Reminder: EOF = last terminal.
fn action_table_vec(
    grammar: &Grammar,
    action_table: &FxHashMap<StateIdx, FxHashMap<Option<TerminalIdx>, LRAction>>,
    n_states: usize,
) -> Vec<Vec<Option<LRAction>>> {
    let n_terminals = grammar.n_terminals();

    let mut state_to_terminal_to_action: Vec<Vec<Option<LRAction>>> =
        Vec::with_capacity(n_terminals);
    for state in 0..n_states {
        let state_idx = StateIdx(state);
        // +1 for EOF
        let mut terminal_to_action: Vec<Option<LRAction>> = Vec::with_capacity(n_terminals + 1);
        for terminal in grammar.terminal_indices() {
            terminal_to_action.push(
                action_table
                    .get(&state_idx)
                    .and_then(|action| action.get(&Some(terminal)))
                    .copied(),
            );
        }
        terminal_to_action.push(
            action_table
                .get(&state_idx)
                .and_then(|action| action.get(&None))
                .copied(),
        );
        state_to_terminal_to_action.push(terminal_to_action);
    }

    state_to_terminal_to_action
}

/// Generates array representation of the goto table.
fn generate_goto_vec(
    goto_table: &FxHashMap<StateIdx, FxHashMap<NonTerminalIdx, StateIdx>>,
    n_states: usize,
    n_non_terminals: usize,
) -> Vec<Vec<Option<StateIdx>>> {
    let mut state_to_non_terminal_to_state: Vec<Vec<Option<StateIdx>>> =
        Vec::with_capacity(n_states);
    for state in 0..n_states {
        let state_idx = StateIdx(state);
        let mut non_terminal_to_state: Vec<Option<StateIdx>> = Vec::with_capacity(n_non_terminals);
        for non_terminal in 0..n_non_terminals {
            let non_terminal_idx = NonTerminalIdx::from_usize(non_terminal);
            non_terminal_to_state.push(
                goto_table
                    .get(&state_idx)
                    .and_then(|goto| goto.get(&non_terminal_idx))
                    .copied(),
            );
        }
        state_to_non_terminal_to_state.push(non_terminal_to_state);
    }
    state_to_non_terminal_to_state
}

fn generate_action_array(
    grammar: &Grammar,
    table: &[Vec<Option<LRAction>>],
    n_states: usize,
    n_terminals: usize,
) -> TokenStream {
    assert_eq!(table.len(), n_states);

    let mut state_array: Vec<TokenStream> = Vec::with_capacity(n_states);

    for state in table {
        // +1 for EOF
        assert_eq!(state.len(), n_terminals + 1);
        let mut terminal_array: Vec<TokenStream> = Vec::with_capacity(n_terminals + 1);
        for action in state {
            match action {
                Some(action) => {
                    let action_code = match action {
                        LRAction::Shift(next_state) => {
                            let next_state: u32 = u32::try_from(next_state.as_usize()).unwrap();
                            quote!(LRAction::Shift { next_state: #next_state })
                        }
                        LRAction::Reduce(non_terminal_idx, production_idx) => {
                            let n_symbols: u16 = u16::try_from(
                                grammar
                                    .get_production(*non_terminal_idx, *production_idx)
                                    .symbols()
                                    .len(),
                            )
                            .unwrap();

                            let non_terminal_idx: u16 =
                                u16::try_from(non_terminal_idx.as_usize()).unwrap();

                            let production_idx: u16 =
                                u16::try_from(production_idx.as_usize()).unwrap();

                            quote!(LRAction::Reduce {
                                non_terminal_idx: #non_terminal_idx,
                                production_idx: #production_idx,
                                n_symbols: #n_symbols,
                            })
                        }
                        LRAction::Accept => quote!(LRAction::Accept),
                    };
                    terminal_array.push(quote!(Some(#action_code)));
                }
                None => terminal_array.push(quote!(None)),
            }
        }
        state_array.push(quote!(
            [#(#terminal_array),*]
        ));
    }

    // +1 for EOF
    let terminal_array_len = n_terminals + 1;
    quote!(
        static ACTION: [[Option<LRAction>; #terminal_array_len]; #n_states] = [
            #(#state_array),*
        ];
    )
}

fn generate_goto_array(
    table: &[Vec<Option<StateIdx>>],
    n_states: usize,
    n_non_terminals: usize,
) -> TokenStream {
    let mut state_array: Vec<TokenStream> = Vec::with_capacity(n_states);

    for state in table {
        let mut non_terminal_array: Vec<TokenStream> = Vec::with_capacity(n_non_terminals);
        for next_state in state {
            non_terminal_array.push(match next_state {
                Some(next_state) => {
                    let next_state: u32 = u32::try_from(next_state.as_usize()).unwrap();
                    quote!(Some(#next_state))
                }
                None => quote!(None),
            });
        }
        state_array.push(quote!(
            [#(#non_terminal_array),*]
        ));
    }

    quote!(
        static GOTO: [[Option<u32>; #n_non_terminals]; #n_states] = [
            #(#state_array),*
        ];
    )
}
