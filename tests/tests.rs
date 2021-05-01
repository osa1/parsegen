use parsegen::parser;

#[test]
fn simple_1() {
    enum Token {
        X,
    }

    parser! {
        enum Token {
            "X" => Token::X,
        }
    }
}
