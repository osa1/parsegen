mod ast;

use ast::Parser;

use proc_macro::TokenStream;

#[proc_macro]
pub fn parser(input: TokenStream) -> TokenStream {
    let Parser { items } = syn::parse_macro_input!(input as Parser);

    println!("{:#?}", items);

    TokenStream::new()
}
