use parsegen::ast::Parser;
use parsegen::parser;

fn main() {
    let file = std::env::args().nth(1).unwrap();
    let contents = std::fs::read_to_string(file).unwrap();
    let parser_ast = syn::parse_str::<Parser>(&contents).unwrap();
    parser(parser_ast);
}
