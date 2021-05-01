#![allow(dead_code)]

mod ast;
mod codegen;
mod earley;
mod first;
mod follow;
mod grammar;
mod graphviz;
mod ll1;
mod lower;
mod simulate;
mod terminal;

#[cfg(test)]
mod test_grammars;

use proc_macro::TokenStream;

#[proc_macro]
pub fn parser(input: TokenStream) -> TokenStream {
    let parser = syn::parse_macro_input!(input as ast::Parser);

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
    println!("Grammar:");
    println!("{:#?}", grammar);

    codegen::generate_ll1_parser(
        grammar,
        &token_enum,
        token_kind_type_name,
        token_kind_type_decl,
        &terminal_repr_arena,
    )
    .into()
}
