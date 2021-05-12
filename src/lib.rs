#![allow(dead_code)]

#[macro_use]
mod maplit;

mod ast;
mod codegen;
mod first;
mod follow;
mod grammar;
mod lower;
mod lr0;
mod lr1;
mod lr_codegen;
mod lr_common;
mod parse_table;
mod terminal;

// mod earley;
// mod simulate;
// mod graphviz;

#[cfg(test)]
mod test_grammars;

use proc_macro::TokenStream;
use quote::quote;

#[proc_macro]
pub fn parser(input: TokenStream) -> TokenStream {
    let parser = syn::parse_macro_input!(input as ast::Parser);

    let mut non_terminals: Vec<ast::NonTerminal> = vec![];
    let mut token_enum: Option<ast::TokenEnum> = None;
    let mut entry: Option<String> = None;

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
                // TODO: For now first pub non-terminal is the entry
                if nt.visibility.is_pub() && entry.is_none() {
                    if nt.productions.len() != 1 {
                        // TODO
                        panic!("Entry non-terminal should have 1 production");
                    }
                    entry = Some(nt.name.to_string());
                }
                non_terminals.push(nt);
            }
        }
    }

    let token_enum = token_enum.expect("Token type (`enum Token`) is not defined");

    // Generate the token kind enum type before lowering the grammar, to avoid mapping terminal
    // strings in the parser definition to token kind enum variants multiple times.
    let (token_kind_type_name, token_kind_type_decl, terminal_repr_arena) =
        codegen::token_kind_type(&token_enum);

    let entry = entry.expect("Grammar needs to have one `pub` non-terminal");

    let grammar = lower::lower(&entry, non_terminals, &terminal_repr_arena);
    // println!("Grammar:");
    // println!("{:#?}", grammar);

    // let ll1_parser = codegen::generate_ll1_parser(
    //     grammar.clone(),
    //     &token_enum,
    //     &token_kind_type_name,
    //     &token_kind_type_decl,
    //     &terminal_repr_arena,
    // );

    let lr1_parser = lr_codegen::generate_lr1_parser(
        grammar,
        &token_enum,
        &terminal_repr_arena,
        &token_kind_type_name,
        &token_kind_type_decl,
    );

    quote!(
        // #ll1_parser
        #lr1_parser
    )
    .into()
}
