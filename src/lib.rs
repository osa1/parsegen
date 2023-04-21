#[macro_use]
mod maplit;

mod ast;
mod codegen;
mod collections;
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

use proc_macro::TokenStream;

#[proc_macro]
pub fn parser(input: TokenStream) -> TokenStream {
    let parser = syn::parse_macro_input!(input as ast::Parser);

    let mut non_terminals: Vec<ast::NonTerminal> = vec![];
    let mut token_enum: Option<ast::TokenEnum> = None;
    let mut state_type: Option<syn::Type> = None;

    for item in parser.items {
        match item {
            ast::GrammarItem::TypeDef(typedef) => match typedef.kind {
                ast::TypeDefKind::State => {
                    if state_type.replace(typedef.ty).is_some() {
                        panic!("`type State` is declared multiple times");
                    }
                }
            },

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

    lr_codegen::generate_lr1_parser(grammar, &token_enum).into()
}
