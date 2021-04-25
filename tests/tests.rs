use parsegen::parser;

#[test]
fn test_simple() {
    parser! {
        // Type synonyms
        type Location = usize;
        type Error = LexerError;

        // Token definition
        enum Token {
            "id" => Token::Id(<RcStr>),
        }

        // Nonterminals
        pub Pgm: Vec<SpannedExpr> = {
            <exprs:Expr+> => exprs,
        };
    }
}
