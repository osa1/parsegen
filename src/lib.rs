#![allow(dead_code)]

mod ast;
mod earley;
mod grammar;
mod lower;
mod simulate;

use ast::Parser;

use proc_macro::TokenStream;

#[proc_macro]
pub fn parser(input: TokenStream) -> TokenStream {
    let parser = syn::parse_macro_input!(input as Parser);

    let grammar = lower::lower(parser);
    println!("Grammar:");
    println!("{:#?}", grammar);

    TokenStream::new()
}
