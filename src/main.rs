#[macro_use]
mod maplit;

mod ast;
mod codegen;
mod collections;
mod first;
mod grammar;
mod item;
mod lalr1;
mod lane_table;
mod lane_tracer;
mod lower;
mod lr1;
mod lr_codegen;
mod lr_common;
mod state;
mod state_graph;
mod utils;

// mod follow;
mod lr0;

#[cfg(test)]
mod test_grammars;

use crate::collections::{Map, Set};
use crate::grammar::TerminalIdx;
use crate::lane_tracer::ConflictIdx;

use std::str::FromStr;

use proc_macro2::TokenStream;
use syn::parse::Parser;

pub fn main() {
    let file = std::env::args().nth(1).unwrap();
    let file_contents = std::fs::read_to_string(file).unwrap();
    let token_stream = TokenStream::from_str(&file_contents).unwrap();
    let parser = syn::parse2::<ast::Parser>(token_stream).unwrap();

    let mut non_terminals: Vec<ast::NonTerminal> = vec![];
    let mut token_enum: Option<ast::TokenEnum> = None;

    for item in parser.items {
        match item {
            ast::GrammarItem::TypeSynonym(_) => { /*TODO*/ }
            ast::GrammarItem::TokenEnum(token_enum_) => {
                if token_enum.is_some() {
                    panic!("`enum Token` is declared multiple times");
                }
                token_enum = Some(token_enum_);
            }
            ast::GrammarItem::NonTerminal(nt) => {
                non_terminals.push(nt);
            }
        }
    }

    let token_enum = token_enum.expect("Token type (`enum Token`) is not defined");

    let grammar = lower::lower(non_terminals, &token_enum.conversions);

    println!("-- Grammar: ------------------------------------------------------");
    print!("{}", grammar);
    println!("------------------------------------------------------------------");
    println!();

    let (lr0_automaton, mut state_graph) = lr0::compute_lr0_automaton(&grammar);

    println!("-- LR0 automaton: ------------------------------------------------");
    print!(
        "{}",
        lr0::LR0AutomatonDisplay {
            grammar: &grammar,
            automaton: &lr0_automaton
        }
    );

    println!();
    println!("Successor graph:");
    println!("{}", state_graph);

    println!("------------------------------------------------------------------");
    println!();

    let lr0_conflicts = lr0::find_conflicts(&lr0_automaton, &grammar);
    let dot = lr0::lr0_dot(&lr0_automaton, &grammar, &lr0_conflicts);
    std::fs::write("lr0.dot", dot).unwrap();
    // println!("-- LR0 automaton graphviz: ---------------------------------------");
    // print!("{}", lr0::lr0_dot(&lr0_automaton, &grammar));
    // println!("------------------------------------------------------------------");
    // println!();

    let mut lalr_states: Vec<lalr1::LALR1State> = lalr1::lr0_states_to_lalr1(&lr0_automaton.states);

    let first_table = crate::first::generate_first_table(&grammar);

    let mut lane_table = lane_table::LaneTable::default();
    let mut lalr_conflicts: Vec<Set<usize>> = vec![Default::default(); lr0_automaton.states.len()];
    for (state_idx, state) in lr0_automaton.states.iter().enumerate() {
        let conflicts: &Set<usize> = match lr0_conflicts.get(&lr_common::StateIdx(state_idx)) {
            Some(conflicts) => {
                assert!(!conflicts.is_empty());
                conflicts
            }
            None => continue,
        };

        for (conflict_idx, conflict_item_idx) in conflicts.iter().enumerate() {
            let conflict_idx = lane_tracer::ConflictIdx(conflict_idx as u32);

            lane_tracer::lane_trace(
                &lr0_automaton.states,
                &first_table,
                &mut state_graph,
                &mut lane_table,
                &mut Default::default(),
                &grammar,
                conflict_idx,
                lr_common::StateIdx(state_idx),
                *state.items.iter().nth(*conflict_item_idx).unwrap(),
            );
        }

        println!(
            "-- Lane table for state {}: --------------------------------------",
            state_idx
        );
        println!(
            "{}",
            lane_table::LaneTableDisplay {
                lane_table: &lane_table,
                grammar: &grammar,
            }
        );
        println!(
            "-- Conflict lookaheads for state {}: -----------------------------",
            state_idx
        );
        let lookaheads: Map<ConflictIdx, Set<Option<TerminalIdx>>> = lane_table.merge_lookaheads();
        print!(
            "{}",
            lane_table::ConflictLookaheadDisplay {
                set: &lookaheads,
                grammar: &grammar
            }
        );
        println!("------------------------------------------------------------------");

        let lalr_state: &mut lalr1::LALR1State = &mut lalr_states[state_idx];

        for (conflict_idx, conflict_item_idx) in conflicts.iter().enumerate() {
            let conflict_lookaheads = lookaheads
                .get(&lane_tracer::ConflictIdx(conflict_idx as u32))
                .unwrap();

            lalr_state.items[*conflict_item_idx].lookahead =
                conflict_lookaheads.iter().cloned().collect();
        }

        // Finished processing the state, generate new conflicts
        lalr_conflicts[state_idx] = lalr1::find_conflicts(lalr_state, &grammar);
    }

    println!("-- LALR1 states: -------------------------------------------------",);
    for (state_idx, state) in lalr_states.iter().enumerate() {
        println!("{}: {{", state_idx);
        println!(
            "{}",
            lalr1::LALR1StateDisplay {
                state,
                grammar: &grammar,
                conflicts: &lalr_conflicts[state_idx],
            }
        );
        println!("}}");
    }

    /*
    // let n_terminals = grammar.n_terminals();
    let (lr1_automaton, _nt_state_indices) =
        crate::lr1::generate_lr1_automaton(&grammar, &first_table);

    let dot = lr1::lr1_dot(&lr1_automaton, &grammar);
    std::fs::write("lr1.dot", dot).unwrap();
    */

    /*
    println!("-- LR1 automaton: ------------------------------------------------");
    println!(
        "{}",
        crate::lr1::LR1AutomatonDisplay {
            automaton: &lr1_automaton,
            grammar: &grammar
        }
    );
    println!("------------------------------------------------------------------");
    println!();
    */

    /*
    let lr1_table = crate::lr1::build_lr1_table(&grammar, &lr1_automaton);

    println!(
        "{}",
        crate::lr_common::LRTableDisplay::new(&lr1_table, &grammar)
    );
    */
}
