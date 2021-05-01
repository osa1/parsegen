use parsegen::parser;

#[test]
fn simple_1() {
    enum Token {
        LParen,
        RParen,
        Str(String),
        Int(i64),
    }

    parser! {
        enum Token {
            "(" => Token::LParen,
            ")" => Token::RParen,
            "str" => Token::Str(<String>),
            "int" => Token::Int(<i64>),
        }

        pub Test1: () = {
            <l:"("> <r:")"> => (),
        };

        pub Test2: String = {
            <s:"str"> => s,
        };
    }
}
