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
    pub enum Token {
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

    let lexer = Lexer::new("1+2");
    let mut iter = lexer.map(|r| r.map(|(_, t, _)| t));
    assert_eq!(E::parse(&mut iter), Ok(()));

    let lexer = Lexer::new("1+2*3");
    let mut iter = lexer.map(|r| r.map(|(_, t, _)| t));
    assert_eq!(E::parse(&mut iter), Ok(()));

    let lexer = Lexer::new("4*1*5+2*3");
    let mut iter = lexer.map(|r| r.map(|(_, t, _)| t));
    assert_eq!(E::parse(&mut iter), Ok(()));
}

#[test]
fn test_grammar_6() {
    // TODO: Token and lexer duplicated from prev test
    #[derive(Debug)]
    pub enum Token {
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

        pub E: u64 = {
            <e:E1> => e,
        };

        E1: u64 = {
            <e:E1> "+" <t:T> => e + t,
            <t:T> => t,
        };

        T: u64 = {
            <t:T> "*" <f:F> => t * f,
            <f:F> => f,
        };

        F: u64 = {
            "(" <e:E> ")" => e,
            <i:"int"> => i,
        };
    }

    let lexer = Lexer::new("1");
    let mut iter = lexer.map(|r| r.map(|(_, t, _)| t));
    assert_eq!(E::parse(&mut iter), Ok(1));

    let lexer = Lexer::new("1+2");
    let mut iter = lexer.map(|r| r.map(|(_, t, _)| t));
    assert_eq!(E::parse(&mut iter), Ok(3));

    let lexer = Lexer::new("1+2*3");
    let mut iter = lexer.map(|r| r.map(|(_, t, _)| t));
    assert_eq!(E::parse(&mut iter), Ok(7));

    let lexer = Lexer::new("4*1*5+2*3");
    let mut iter = lexer.map(|r| r.map(|(_, t, _)| t));
    assert_eq!(E::parse(&mut iter), Ok(26));
}

#[test]
fn test_grammar_7() {
    #[derive(Debug)]
    pub enum Token {
        Eq,
        Star,
        Id,
    }

    parser! {
        enum Token {
            "*" => Token::Star,
            "=" => Token::Eq,
            "n" => Token::Id,
        }

        pub S0: () = {
            S => (),
        };

        S: () = {
            L "=" R => (),
            R => (),
        };

        L: () = {
            "*" R => (),
            "n" => (),
        };

        R: () = {
            L => (),
        };
    }

    use Token::*;
    assert_eq!(
        S0::parse(vec![Ok::<Token, ()>(Id), Ok(Eq), Ok(Id)].into_iter()),
        Ok(())
    );
    assert_eq!(
        S0::parse(vec![Ok::<Token, ()>(Id), Ok(Eq), Ok(Star), Ok(Id)].into_iter()),
        Ok(())
    );
    assert_eq!(
        S0::parse(vec![Ok::<Token, ()>(Star), Ok(Id), Ok(Eq), Ok(Id)].into_iter()),
        Ok(())
    );
}

// An example with:
// - juxtaposition application syntax
// - unary `-`
// - binary `-`
// - multiple entry points
#[test]
fn expr_example() {
    #[derive(Debug, PartialEq, Eq)]
    pub enum Expr {
        Id,
        Neg(Box<Expr>),
        Sub(Box<Expr>, Box<Expr>),
        App(Box<Expr>, Box<Expr>),
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum Token {
        Id,
        Minus,
    }

    parser! {
        enum Token {
            "n" => Token::Id,
            "-" => Token::Minus,
        }

        pub BinOpExpr: Expr = {
            <expr:UnOpExpr> =>
                expr,

            <expr1:BinOpExpr> "-" <expr2:UnOpExpr> =>
                Expr::Sub(Box::new(expr1), Box::new(expr2)),
        };

        pub UnOpExpr: Expr = {
            <expr:AppExpr> =>
                expr,

            "-" <expr:AppExpr> =>
                Expr::Neg(Box::new(expr)),
        };

        pub AppExpr: Expr = {
            <expr:SimpleExpr> =>
                expr,

            <expr1:SimpleExpr> <expr2:SimpleExpr> =>
                Expr::App(Box::new(expr1), Box::new(expr2)),
        };

        pub SimpleExpr: Expr = {
            "n" =>
                Expr::Id,
        };
    }

    use Expr::*;

    assert_eq!(
        BinOpExpr::parse(
            vec![Token::Id, Token::Minus, Token::Id]
                .into_iter()
                .map(Ok::<Token, ()>)
        ),
        Ok(Sub(Box::new(Id), Box::new(Id)))
    );
    assert_eq!(
        BinOpExpr::parse(vec![Token::Id, Token::Id].into_iter().map(Ok::<Token, ()>)),
        Ok(App(Box::new(Id), Box::new(Id)))
    );
    assert_eq!(
        BinOpExpr::parse(
            vec![Token::Minus, Token::Id, Token::Id]
                .into_iter()
                .map(Ok::<Token, ()>)
        ),
        Ok(Neg(Box::new(App(Box::new(Id), Box::new(Id)))))
    );
    assert_eq!(
        BinOpExpr::parse(
            vec![Token::Minus, Token::Id, Token::Minus, Token::Id]
                .into_iter()
                .map(Ok::<Token, ()>)
        ),
        Ok(Sub(Box::new(Neg(Box::new(Id))), Box::new(Id)))
    );
    assert_eq!(
        BinOpExpr::parse(
            vec![Token::Id, Token::Minus, Token::Minus, Token::Id]
                .into_iter()
                .map(Ok::<Token, ()>)
        ),
        Ok(Sub(Box::new(Id), Box::new(Neg(Box::new(Id)))))
    );

    // Try other entry points
    assert_eq!(
        UnOpExpr::parse(
            vec![Token::Minus, Token::Id]
                .into_iter()
                .map(Ok::<Token, ()>)
        ),
        Ok(Neg(Box::new(Id)))
    );
    assert_eq!(
        AppExpr::parse(vec![Token::Id, Token::Id].into_iter().map(Ok::<Token, ()>)),
        Ok(App(Box::new(Id), Box::new(Id)))
    );
    assert_eq!(
        SimpleExpr::parse(vec![Token::Id].into_iter().map(Ok::<Token, ()>)),
        Ok(Id)
    );
}

#[test]
fn token_values() {
    #[derive(Debug, PartialEq, Eq)]
    pub struct Id;

    pub enum Token {
        Id(Id),
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum Expr {
        Id(Id),
    }

    parser! {
        enum Token {
            "id" => Token::Id(<Id>),
        }

        pub Entry: Expr = {
            <id:"id"> => Expr::Id(id),
        };
    }

    assert_eq!(
        Entry::parse(vec![Token::Id(Id)].into_iter().map(Ok::<Token, ()>)),
        Ok(Expr::Id(Id))
    );
}

#[test]
fn lookahead_example_1() {
    // G0 from http://smallcultfollowing.com/babysteps/blog/2017/03/17/the-lane-table-algorithm/
    // This grammar is LALR(1) (not LR(0))

    #[derive(Debug)]
    pub enum Token {
        E,
        C,
        D,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum Expr {
        C(usize),
        D(usize),
    }

    parser! {
        enum Token {
            "e" => Token::E,
            "c" => Token::C,
            "d" => Token::D,
        }

        pub Entry: Expr = {
            <x:X> "c" => Expr::C(x),
            <y:Y> "d" => Expr::D(y),
        };

        X: usize = {
            "e" <x:X> => x + 1,
            "e" => 1,
        };

        Y: usize = {
            "e" <y:Y> => y + 1,
            "e" => 1,
        };
    }

    assert_eq!(
        Entry::parse(
            vec![Token::E, Token::E, Token::C]
                .into_iter()
                .map(Ok::<Token, ()>)
        ),
        Ok(Expr::C(2))
    );
    assert_eq!(
        Entry::parse(
            vec![Token::E, Token::E, Token::D]
                .into_iter()
                .map(Ok::<Token, ()>)
        ),
        Ok(Expr::D(2))
    );
}

#[test]
fn lookahead_example_2() {
    // G1 from http://smallcultfollowing.com/babysteps/blog/2017/03/17/the-lane-table-algorithm/
    // This grammar is LR(1) (not LALR(1))

    #[derive(Debug)]
    pub enum Token {
        A,
        B,
        C,
        D,
        E,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum Expr {
        E1(usize),
        E2(usize),
        E3(usize),
        E4(usize),
    }

    parser! {
        enum Token {
            "a" => Token::A,
            "b" => Token::B,
            "c" => Token::C,
            "d" => Token::D,
            "e" => Token::E,
        }

        pub Entry: Expr = {
            "a" <x:X> "d" => Expr::E1(x),
            "a" <y:Y> "c" => Expr::E2(y),
            "b" <x:X> "c" => Expr::E3(x),
            "b" <y:Y> "d" => Expr::E4(y),
        };

        X: usize = {
            "e" <x:X> => x + 1,
            "e" => 1,
        };

        Y: usize = {
            "e" <y:Y> => y + 1,
            "e" => 1,
        };
    }

    assert_eq!(
        Entry::parse(
            vec![Token::A, Token::E, Token::E, Token::D]
                .into_iter()
                .map(Ok::<Token, ()>)
        ),
        Ok(Expr::E1(2))
    );
    assert_eq!(
        Entry::parse(
            vec![Token::A, Token::E, Token::E, Token::C]
                .into_iter()
                .map(Ok::<Token, ()>)
        ),
        Ok(Expr::E2(2))
    );
    assert_eq!(
        Entry::parse(
            vec![Token::B, Token::E, Token::E, Token::C]
                .into_iter()
                .map(Ok::<Token, ()>)
        ),
        Ok(Expr::E3(2))
    );
    assert_eq!(
        Entry::parse(
            vec![Token::B, Token::E, Token::E, Token::D]
                .into_iter()
                .map(Ok::<Token, ()>)
        ),
        Ok(Expr::E4(2))
    );
}

#[test]
fn token_ordering() {
    #[derive(Debug, PartialEq, Eq)]
    pub enum Token<'input> {
        Id(&'input str),
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum Expr {
        IdA,
        Id,
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
                "id_a" => Token::Id("a"),
                "id" => Token::Id(<&'input str>),
            }

            pub Exprs : Vec<Expr> = {
                <e:Expr0> <mut es:Exprs> => {
                    es.insert(0, e);
                    es
                },

                => vec![],
            };

            Expr0 : Expr = {
                "id" => Expr::Id,
                "id_a" => Expr::IdA,
            };
        }

        let lexer = Lexer::new("a b");
        let mut iter = lexer.map(|r| r.map(|(_, t, _)| t));
        assert_eq!(Exprs::parse(&mut iter), Ok(vec![Expr::IdA, Expr::Id]));
    }

    {
        parser! {
            enum Token<'input> {
                "id" => Token::Id(<&'input str>),
                "id_a" => Token::Id("a"), // never matched
            }

            pub Exprs : Vec<Expr> = {
                <e:Expr0> <mut es:Exprs> => {
                    es.insert(0, e);
                    es
                },

                => vec![],
            };

            Expr0 : Expr = {
                "id" => Expr::Id,
                "id_a" => Expr::IdA,
            };
        }

        let lexer = Lexer::new("a b");
        let mut iter = lexer.map(|r| r.map(|(_, t, _)| t));
        assert_eq!(Exprs::parse(&mut iter), Ok(vec![Expr::Id, Expr::Id]));
    }
}
