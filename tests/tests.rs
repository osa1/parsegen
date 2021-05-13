use lexgen::lexer;
use parsegen::parser;

#[test]
fn balanced_parens() {
    #[derive(Debug, PartialEq, Eq)]
    pub enum Token {
        LParen,
        RParen,
    }

    lexer! {
        Lexer -> Token;

        rule Init {
            "(" = Token::LParen,
            ")" = Token::RParen,
        }
    }

    parser! {
        enum Token {
            "(" => Token::LParen,
            ")" => Token::RParen,
        }

        pub Entry: usize = {
            <test:Test> => test,
        };

        Test: usize = {
            "(" <t:Test> ")" => t + 1,
            => 0,
        };
    }

    let lexer = Lexer::new("");
    let mut iter = lexer.map(|r| r.map(|(_, t, _)| t));
    assert_eq!(Entry::parse(&mut iter), Ok(0));

    let lexer = Lexer::new("()");
    let mut iter = lexer.map(|r| r.map(|(_, t, _)| t));
    assert_eq!(Entry::parse(&mut iter), Ok(1));

    let lexer = Lexer::new("(())");
    let mut iter = lexer.map(|r| r.map(|(_, t, _)| t));
    assert_eq!(Entry::parse(&mut iter), Ok(2));
}

#[test]
fn token_lifetimes() {
    #[derive(Debug, PartialEq, Eq)]
    pub enum Token<'input> {
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

        let lexer = Lexer::new("abc");
        let mut iter = lexer.map(|r| r.map(|(_, t, _)| t));
        assert_eq!(Test::parse(&mut iter), Ok("abc"));
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

        let lexer = Lexer::new("a b c");
        let mut iter = lexer.map(|r| r.map(|(_, t, _)| t));
        assert_eq!(Test::parse(&mut iter), Ok("abc".to_owned()));
    }
}

// This used to cause a compile-time panic caused by not adding a non-terminal to the follow table
// because it's never used in any of the production RHSs.
#[test]
fn bug_1() {
    #![allow(dead_code)]
    // TODO: Add this in macro expansion
    #![allow(unreachable_code)]

    #[derive(Debug)]
    pub enum Token {
        A,
    }

    parser! {
        enum Token {
            "a" => Token::A,
        }

        pub Test1: Token = {
            => todo!(),
        };

        pub Test2: Token = {
            => todo!(),
        };
    }
}

// Similar to the above, but the panic would occur because of a missing non-terminal in the first
// set.
#[test]
fn bug_2() {
    #![allow(dead_code)]
    // TODO: Add this in macro expansion
    #![allow(unreachable_code)]

    #[derive(Debug)]
    pub enum Token {
        A,
    }

    parser! {
        enum Token {
            "a" => Token::A,
        }

        pub Test1: Token = {
            Test2 Test1 => todo!(),
        };

        pub Test2: Token = {
            => todo!(),
        };
    }
}

#[test]
fn test_grammar_5() {
    #[derive(Debug)]
    enum Token {
        Plus,
        Star,
        Int(u64),
        LParen,
        RParen,
    }

    lexer! {
        Lexer -> Token;

        rule Init {
            "(" = Token::LParen,
            ")" = Token::RParen,
            "+" = Token::Plus,
            "*" = Token::Star,
            ['0'-'9']+ => |lexer| {
                let match_ = lexer.match_();
                lexer.return_(Token::Int(str::parse::<u64>(match_).unwrap()))
            },
        }
    }

    parser! {
        enum Token {
            "(" => Token::LParen,
            ")" => Token::RParen,
            "+" => Token::Plus,
            "*" => Token::Star,
            "int" => Token::Int(<u64>),
        }

        pub E: () = {
            T E1 => (),
        };

        E1: () = {
            "+" T E1 => (),
            => (),
        };

        T: () = {
            F T1 => (),
        };

        T1: () = {
            "*" F T1 => (),
            => (),
        };

        F: () = {
            "(" E ")" => (),
            "int" => (),
        };
    }

    let lexer = Lexer::new("1");
    let mut iter = lexer.map(|r| r.map(|(_, t, _)| t));
    assert_eq!(E::parse(&mut iter), Ok(()));

    let lexer = Lexer::new("1 + 2");
    let mut iter = lexer.map(|r| r.map(|(_, t, _)| t));
    assert_eq!(E::parse(&mut iter), Ok(()));

    let lexer = Lexer::new("1 + 2 * 3");
    let mut iter = lexer.map(|r| r.map(|(_, t, _)| t));
    assert_eq!(E::parse(&mut iter), Ok(()));

    let lexer = Lexer::new("4 * 1 * 5 + 2 * 3");
    let mut iter = lexer.map(|r| r.map(|(_, t, _)| t));
    assert_eq!(E::parse(&mut iter), Ok(()));
}
