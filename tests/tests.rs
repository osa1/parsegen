use lexgen::lexer;
use parsegen::parser;

#[test]
fn balanced_parens() {
    #[derive(Debug, PartialEq, Eq)]
    enum Token {
        LParen,
        RParen,
    }

    lexer! {
        Lexer -> Token;

        rule Init {
            "(" => |lexer| lexer.return_(Token::LParen),
            ")" => |lexer| lexer.return_(Token::RParen),
        }
    }

    parser! {
        enum Token {
            "(" => Token::LParen,
            ")" => Token::RParen,
        }

        pub Test: usize = {
            "(" <t:Test> ")" => t + 1,
            => 0,
        };
    }

    let mut lexer = Lexer::new("()");
    assert_eq!(parse(&mut lexer), Ok(1));

    let mut lexer = Lexer::new("((()))");
    assert_eq!(parse(&mut lexer), Ok(3));

    let mut lexer = Lexer::new("((())");
    assert!(parse(&mut lexer).is_err());
}

#[test]
fn token_lifetimes() {
    #[derive(Debug, PartialEq, Eq)]
    enum Token<'input> {
        Id(&'input str),
    }

    lexer! {
        Lexer -> Token<'input>;

        [' ' '\t' '\n']+,

        ['a'-'z']+ => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Id(match_))
        },
    }

    {
        parser! {
            enum Token<'input> {
                "id" => Token::Id(<&'input str>),
            }

            pub Test: &'input str = {
                <s:"id"> => {
                    s
                },
            };
        }

        let mut lexer = Lexer::new("abc");
        assert_eq!(parse(&mut lexer), Ok("abc"));
    }

    {
        parser! {
            enum Token<'input> {
                "id" => Token::Id(<&'input str>),
            }

            pub Test: String = {
                <i1:"id"> <i2:"id"> <i3:"id"> => {
                    i1.to_owned() + i2 + i3
                },
            };
        }

        let mut lexer = Lexer::new("a b c");
        assert_eq!(parse(&mut lexer), Ok("abc".to_owned()));
    }
}
