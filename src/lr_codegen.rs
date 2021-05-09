use crate::first::{generate_first_table, FirstTable};
use crate::grammar::Grammar;
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
    let lr1_table = build_lr1_table(grammar, &lr1_automaton);

    let action_vec = action_table_vec(lr1_table.get_action_table(), terminals);

    todo!()
}

/// Generates array representation of the action table. Reminder: EOF = last terminal.
fn action_table_vec(
    action_table: &FxHashMap<(StateIdx, Option<TerminalReprIdx>), LRAction>,
    terminals: &TerminalReprArena,
) -> Vec<Vec<Option<LRAction>>> {
    let n_terminals = terminals.n_terminals();
    let n_states: usize = action_table
        .keys()
        .map(|(state_idx, _)| state_idx.as_usize())
        .max()
        .unwrap()
        + 1;

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
