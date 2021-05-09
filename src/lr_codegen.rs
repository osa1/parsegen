use crate::first::{generate_first_table, FirstTable};
use crate::grammar::{Grammar, NonTerminalIdx};
use crate::lr1::{build_lr1_table, generate_lr1_automaton};
use crate::lr_common::{LRAction, LRTable, StateIdx};
use crate::terminal::{TerminalReprArena, TerminalReprIdx};

use fxhash::FxHashMap;
use proc_macro2::{Span, TokenStream};
use quote::quote;

pub fn generate_lr1_parser(
    grammar: &Grammar<TerminalReprIdx, syn::Expr>,
    terminals: &TerminalReprArena,
) -> TokenStream {
    let first_table = generate_first_table(grammar);
    let lr1_automaton = generate_lr1_automaton(&grammar, &first_table);
    let lr1_table = build_lr1_table(grammar, &lr1_automaton, terminals.n_terminals());

    let action_vec = action_table_vec(
        lr1_table.get_action_table(),
        lr1_table.n_states(),
        terminals,
    );

    let goto_vec = generate_goto_vec(
        lr1_table.get_goto_table(),
        lr1_table.n_states(),
        grammar.non_terminals.len(),
    );

    todo!()
}

/// Generates array representation of the action table. Reminder: EOF = last terminal.
fn action_table_vec(
    action_table: &FxHashMap<(StateIdx, Option<TerminalReprIdx>), LRAction>,
    n_states: usize,
    terminals: &TerminalReprArena,
) -> Vec<Vec<Option<LRAction>>> {
    let n_terminals = terminals.n_terminals();

    let mut state_to_terminal_to_action: Vec<Vec<Option<LRAction>>> =
        Vec::with_capacity(n_terminals);
    for state in 0..n_states {
        let state_idx = StateIdx(state);
        // +1 for EOF
        let mut terminal_to_action: Vec<Option<LRAction>> = Vec::with_capacity(n_terminals + 1);
        for terminal in terminals.terminal_indices() {
            terminal_to_action.push(action_table.get(&(state_idx, Some(terminal))).copied());
        }
        terminal_to_action.push(action_table.get(&(state_idx, None)).copied());
        state_to_terminal_to_action.push(terminal_to_action);
    }

    state_to_terminal_to_action
}

/// Generates array representation of the goto table.
fn generate_goto_vec(
    goto_table: &FxHashMap<(StateIdx, NonTerminalIdx), StateIdx>,
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
            non_terminal_to_state.push(goto_table.get(&(state_idx, non_terminal_idx)).copied());
        }
        state_to_non_terminal_to_state.push(non_terminal_to_state);
    }
    state_to_non_terminal_to_state
}
