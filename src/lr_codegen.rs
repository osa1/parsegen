use crate::ast::TokenEnum;
use crate::codegen::{
    generate_semantic_action_table, semantic_action_result_type, token_value_fn, SemanticActionIdx,
};
use crate::first::generate_first_table;
use crate::grammar::{Grammar, NonTerminalIdx};
use crate::lr1::{build_lr1_table, generate_lr1_automaton};
use crate::lr_common::{LRAction, StateIdx};
use crate::terminal::{TerminalReprArena, TerminalReprIdx};

use std::convert::TryFrom;

use fxhash::FxHashMap;
use proc_macro2::TokenStream;
use quote::quote;

pub fn generate_lr1_parser(
    grammar: Grammar<TerminalReprIdx, syn::Expr>,
    tokens: &TokenEnum,
    terminals: &TerminalReprArena,
    token_kind_type_name: &syn::Ident,
    token_kind_type_decl: &TokenStream,
) -> TokenStream {
    let n_terminals = terminals.n_terminals();
    let token_type = &tokens.type_name;

    let first_table = generate_first_table(&grammar);
    let lr1_automaton = generate_lr1_automaton(&grammar, &first_table);

    let (
        semantic_action_result_type_name,
        semantic_action_result_type_decl,
        non_terminal_action_variant_name,
    ) = semantic_action_result_type(&grammar, &tokens.conversions, &tokens.type_lifetimes);

    // Generate semantic action table, replace semantic actions in the grammar with their indices
    // in the table
    let (semantic_action_table, grammar) = generate_semantic_action_table(
        grammar,
        &non_terminal_action_variant_name,
        &tokens.type_lifetimes,
    );

    println!(
        "{}",
        crate::lr1::LR1AutomatonDisplay {
            automaton: &lr1_automaton,
            grammar: &grammar
        }
    );

    let lr1_table = build_lr1_table(&grammar, &lr1_automaton, n_terminals);

    let action_vec = action_table_vec(
        lr1_table.get_action_table(),
        lr1_table.n_states(),
        terminals,
    );

    let action_array_code =
        generate_action_array(&grammar, &action_vec, lr1_table.n_states(), n_terminals);

    let goto_vec = generate_goto_vec(
        lr1_table.get_goto_table(),
        lr1_table.n_states(),
        grammar.non_terminals.len(),
    );

    let goto_array_code =
        generate_goto_array(&goto_vec, lr1_table.n_states(), grammar.non_terminals.len());

    let (_token_kind_fn_name, token_kind_fn_decl) =
        crate::codegen::token_kind_fn(token_kind_type_name, tokens);

    let (_token_value_fn_name, token_value_fn_decl) = token_value_fn(
        &tokens.conversions,
        &tokens.type_name,
        &tokens.type_lifetimes,
        &semantic_action_result_type_name,
    );

    quote!(
        #[derive(Clone, Copy)]
        enum LRAction {
            Shift {
                next_state: u32,
            },
            Reduce {
                non_terminal_idx: u16,
                n_symbols: u16,
                semantic_action_idx: u16,
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

        // fn token_value(token: &Token) -> SemanticActionResult { ... }
        #token_value_fn_decl

        // enum SemanticActionResult { ... } + an impl for extracting fields
        #semantic_action_result_type_decl

        // static SEMANTIC_ACTIONS: [fn(&mut Vec<SemanticActionResult); ...] = [ ... ]
        // + the functions
        #(#semantic_action_table)*

        pub fn recognize<E: Clone>(
            mut input: impl Iterator<Item=Result<#token_type, E>>
        ) -> Result<(), ParseError_<E>>
        {
            let mut stack: Vec<u32> = vec![0];

            let mut token = input.next();

            loop {
                let state = *stack.last().unwrap() as usize;
                let terminal_idx = match &token {
                    None => #n_terminals,
                    Some(Err(err)) => return Err(ParseError_::Other(err.clone())),
                    Some(Ok(token)) => token_kind(&token) as usize,
                };
                match ACTION[state][terminal_idx] {
                    None => panic!("Stuck! (1) state={}, terminal={}", state, terminal_idx),
                    Some(LRAction::Shift { next_state }) => {
                        stack.push(next_state);
                        token = input.next();
                    }
                    Some(LRAction::Reduce { non_terminal_idx, n_symbols, semantic_action_idx }) => {
                        for _ in 0 .. n_symbols {
                            stack.pop().unwrap();
                        }
                        let state = *stack.last().unwrap() as usize;
                        match GOTO[state][non_terminal_idx as usize] {
                            None => panic!("Stuck! (2)"),
                            Some(next_state) => stack.push(next_state),
                        }
                    }
                    Some(LRAction::Accept) => return Ok(()),
                }
            }
        }
    )
}

/// Generates array representation of the action table. Reminder: EOF = last terminal.
fn action_table_vec<A: Copy>(
    action_table: &FxHashMap<StateIdx, FxHashMap<Option<TerminalReprIdx>, LRAction<A>>>,
    n_states: usize,
    terminals: &TerminalReprArena,
) -> Vec<Vec<Option<LRAction<A>>>> {
    let n_terminals = terminals.n_terminals();

    let mut state_to_terminal_to_action: Vec<Vec<Option<LRAction<A>>>> =
        Vec::with_capacity(n_terminals);
    for state in 0..n_states {
        let state_idx = StateIdx(state);
        // +1 for EOF
        let mut terminal_to_action: Vec<Option<LRAction<A>>> = Vec::with_capacity(n_terminals + 1);
        for terminal in terminals.terminal_indices() {
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

fn generate_action_array<A>(
    grammar: &Grammar<TerminalReprIdx, A>,
    table: &[Vec<Option<LRAction<SemanticActionIdx>>>],
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
                        LRAction::Reduce(non_terminal_idx, production_idx, semantic_action_idx) => {
                            let n_symbols: u16 = u16::try_from(
                                grammar
                                    .get_production(*non_terminal_idx, *production_idx)
                                    .symbols()
                                    .len(),
                            )
                            .unwrap();
                            let non_terminal_idx: u16 =
                                u16::try_from(non_terminal_idx.as_usize()).unwrap();
                            let semantic_action_idx = semantic_action_idx.as_u16();
                            quote!(LRAction::Reduce {
                                non_terminal_idx: #non_terminal_idx,
                                n_symbols: #n_symbols,
                                semantic_action_idx: #semantic_action_idx,
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
