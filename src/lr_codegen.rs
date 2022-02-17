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

    println!(
        "{}",
        crate::lr_common::LRTableDisplay::new(&lr1_table, &grammar)
    );

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
                        mut input: impl Iterator<Item=Result<#token_type<#(#token_lifetimes,)*>, E>>
                    ) -> Result<Node, ParseError_<E>>
                    {
                        parse_generic(
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

        #[derive(Debug)]
        enum Kind<#(#token_lifetimes,)*> {
            NonTerminal(usize),
            Terminal(#token_type<#(#token_lifetimes,)*>),
        }

        #[derive(Debug)]
        pub struct Node<#(#token_lifetimes,)*> {
            kind: Kind<#(#token_lifetimes,)*>,
            span: (usize, usize),
            children: Vec<Node>,
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
            mut input: impl Iterator<Item=Result<#token_type<#(#token_lifetimes,)*>, E>>,
            init_state: u32,
        ) -> Result<Node, ParseError_<E>>
        {
            let mut state_stack: Vec<u32> = vec![init_state];
            let mut value_stack: Vec<Node> = vec![];

            let mut token: Option<Result<#token_type<#(#token_lifetimes,)*>, E>> =
                input.next();

            loop {
                let state = *state_stack.last().unwrap() as usize;
                let terminal_idx = match &token {
                    None => #n_terminals,
                    Some(Err(err)) => return Err(ParseError_::Other(err.clone())),
                    Some(Ok(token)) => #token_kind_fn_name(&token) as usize,
                };
                match ACTION[state][terminal_idx] {
                    None => panic!("Stuck! (1) state={}, terminal={}", state, terminal_idx),
                    Some(LRAction::Shift { next_state }) => {
                        state_stack.push(next_state);
                        let token_ = token.unwrap().unwrap();
                        value_stack.push(Node {
                            kind: Kind::Terminal(token_),
                            span: (0, 0),
                            children: Vec::new(),
                        });
                        token = input.next();
                    }
                    Some(LRAction::Reduce {
                        non_terminal_idx,
                        production_idx,
                        n_symbols,
                    }) => {
                        let children: Vec<Node> =
                            value_stack.drain(value_stack.len() - (n_symbols as usize)..).collect();

                        value_stack.push(Node {
                            kind: Kind::NonTerminal(non_terminal_idx as usize),
                            span: (0, 0),
                            children,
                        });

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
