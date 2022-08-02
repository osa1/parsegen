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

// An example with (1) juxtaposition application syntax (2) unary `-` (3) binary `-`
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

        UnOpExpr: Expr = {
            <expr:AppExpr> =>
                expr,

            "-" <expr:AppExpr> =>
                Expr::Neg(Box::new(expr)),
        };

        AppExpr: Expr = {
            <expr:SimpleExpr> =>
                expr,

            <expr1:SimpleExpr> <expr2:SimpleExpr> =>
                Expr::App(Box::new(expr1), Box::new(expr2)),
        };

        SimpleExpr: Expr = {
            "n" =>
                Expr::Id,
        };
    }

    use Expr::*;

    assert_eq!(
        BinOpExpr::parse(
            vec![Token::Id, Token::Minus, Token::Id]
                .into_iter()
                .map(|t| Ok::<Token, ()>(t))
        ),
        Ok(Sub(Box::new(Id), Box::new(Id)))
    );
    assert_eq!(
        BinOpExpr::parse(
            vec![Token::Id, Token::Id]
                .into_iter()
                .map(|t| Ok::<Token, ()>(t))
        ),
        Ok(App(Box::new(Id), Box::new(Id)))
    );
    assert_eq!(
        BinOpExpr::parse(
            vec![Token::Minus, Token::Id, Token::Id]
                .into_iter()
                .map(|t| Ok::<Token, ()>(t))
        ),
        Ok(Neg(Box::new(App(Box::new(Id), Box::new(Id)))))
    );
    assert_eq!(
        BinOpExpr::parse(
            vec![Token::Minus, Token::Id, Token::Minus, Token::Id]
                .into_iter()
                .map(|t| Ok::<Token, ()>(t))
        ),
        Ok(Sub(Box::new(Neg(Box::new(Id))), Box::new(Id)))
    );
    assert_eq!(
        BinOpExpr::parse(
            vec![Token::Id, Token::Minus, Token::Minus, Token::Id]
                .into_iter()
                .map(|t| Ok::<Token, ()>(t))
        ),
        Ok(Sub(Box::new(Id), Box::new(Neg(Box::new(Id)))))
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
        Entry::parse(vec![Token::Id(Id)].into_iter().map(|t| Ok::<Token, ()>(t))),
        Ok(Expr::Id(Id))
    );
}

#[test]
fn associativity_right() {
    pub enum Token {
        A,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum Expr {
        A,
        Bin(Box<Expr>, Box<Expr>),
    }

    parser! {
        enum Token {
            "a" => Token::A,
        }

        pub Entry : Expr = {
            #[shift]
            <left:Entry> <right:Entry> =>
                Expr::Bin(Box::new(left), Box::new(right)),

            "a" =>
                Expr::A,
        };
    }

    assert_eq!(
        Entry::parse(
            vec![Token::A, Token::A, Token::A]
                .into_iter()
                .map(|t| Ok::<Token, ()>(t))
        ),
        Ok(Expr::Bin(
            Box::new(Expr::A),
            Box::new(Expr::Bin(Box::new(Expr::A), Box::new(Expr::A)))
        ))
    );
}

#[test]
fn associativity_left() {
    pub enum Token {
        A,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum Expr {
        A,
        Bin(Box<Expr>, Box<Expr>),
    }

    parser! {
        enum Token {
            "a" => Token::A,
        }

        pub Entry : Expr = {
            #[reduce]
            <left:Entry> <right:Entry> =>
                Expr::Bin(Box::new(left), Box::new(right)),

            "a" =>
                Expr::A,
        };
    }

    assert_eq!(
        Entry::parse(
            vec![Token::A, Token::A, Token::A]
                .into_iter()
                .map(|t| Ok::<Token, ()>(t))
        ),
        Ok(Expr::Bin(
            Box::new(Expr::Bin(Box::new(Expr::A), Box::new(Expr::A))),
            Box::new(Expr::A),
        ))
    );
}
