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
mod terminal;

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

    // Generate the token kind enum type before lowering the grammar, to avoid mapping terminal
    // strings in the parser definition to token kind enum variants multiple times.
    let (token_kind_type_name, token_kind_type_decl, terminal_repr_arena) =
        codegen::token_kind_type(&token_enum);

    let grammar = lower::lower(non_terminals, &terminal_repr_arena);
    // println!("Grammar:");
    // println!("{}", grammar);

    let lr1_parser = lr_codegen::generate_lr1_parser(
        grammar,
        &token_enum,
        &terminal_repr_arena,
        &token_kind_type_name,
        &token_kind_type_decl,
    );

    quote!(
        #lr1_parser
    )
    .into()
}
