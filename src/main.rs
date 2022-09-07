#[macro_use]
mod maplit;

mod ast;
mod codegen;
mod first;
mod grammar;
mod lower;
mod lr1;
mod lr_codegen;
mod lr_common;

mod follow;
mod lr0;

#[cfg(test)]
mod test_grammars;

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

    let lr0_automaton = lr0::compute_lr0_automaton(&grammar);

    println!("-- LR0 automaton: ------------------------------------------------");
    print!(
        "{}",
        lr0::LR0AutomatonDisplay {
            grammar: &grammar,
            automaton: &lr0_automaton
        }
    );
    println!("------------------------------------------------------------------");
    println!();

    let n_terminals = grammar.n_terminals();
    let first_table = crate::first::generate_first_table(&grammar);
    let (lr1_automaton, nt_state_indices) =
        crate::lr1::generate_lr1_automaton(&grammar, &first_table);

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

    /*
    let lr1_table = crate::lr1::build_lr1_table(&grammar, &lr1_automaton);

    println!(
        "{}",
        crate::lr_common::LRTableDisplay::new(&lr1_table, &grammar)
    );
    */
}
