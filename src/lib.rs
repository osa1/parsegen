#![allow(dead_code)]

mod ast;
mod codegen;
mod earley;
mod grammar;
mod graphviz;
mod ll1;
mod lower;
mod simulate;

#[cfg(test)]
mod test_grammars;

use ast::Parser;

use proc_macro::TokenStream;

#[proc_macro]
pub fn parser(input: TokenStream) -> TokenStream {
    let parser = syn::parse_macro_input!(input as Parser);

    let grammar = lower::lower(parser);
    println!("Grammar:");
    println!("{:#?}", grammar);

    codegen::build_inefficient_recognizer(grammar).into()
}
