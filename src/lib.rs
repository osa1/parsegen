#[macro_use]
mod maplit;

pub mod ast;
mod codegen;
mod first;
mod grammar;
mod lower;
mod lr1;
mod lr_codegen;
mod lr_common;

// mod follow;
// mod lr0;

#[cfg(test)]
mod test_grammars;

use proc_macro2::TokenStream;
use quote::quote;

pub fn parser(parser: ast::Parser) -> TokenStream {
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
    // println!("Grammar:");
    // println!("{}", grammar);

    let lr1_parser = lr_codegen::generate_lr1_parser(grammar, &token_enum);

    quote!(
        #lr1_parser
    )
    .into()
}
