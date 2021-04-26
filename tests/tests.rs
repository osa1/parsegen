use parsegen::parser;

// Figure 1 in "Practical Earley Parsing"
#[test]
fn test_figure_1() {
    enum Token {
        N,
        Plus,
    }

    parser! {
        type Location = usize;
        type Error = LexerError;

        enum Token {
            "n" => Token::N,
            "+" => Token::Plus,
        }

        pub S: () = {
            <e:E> => e,
        };

        pub E: () = {
            <e1:E> "+" <e2:E> => (),
            "n" => (),
        };
    }

    let input = vec![Token::N, Token::Plus, Token::N];
}

#[test]
fn test_figure_2() {
    enum Token {
        A
    }

    parser! {
        type Location = usize;
        type Error = LexerError;

        enum Token {
            "a" => Token::A,
        }

        pub S: () = {
            A A A A => (),
        };

        pub A: () = {
            "a" => (),
            E => (),
        };

        pub E: () = {
             // empty
        };
    }

    let input = vec![Token::A];
}
