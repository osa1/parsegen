use parsegen::parser;

#[test]
fn simple_1() {
    enum Token {
        LParen,
        Str(String),
        Int(i64),
    }

    parser! {
        enum Token {
            "a" => Token::LParen,
            "str" => Token::Str(<String>),
            "int" => Token::Int(<i64>),
        }
    }
}
