use lexgen::lexer;
use parsegen::parser;

#[test]
fn balanced_parens_0() {
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

/*
#[test]
fn simple_1() {
    #[derive(Debug)]
    enum Token {
        LParen,
        RParen,
        Plus,
        Minus,
        Star,
        Slash,
        Int(i64),
    }

    lexer! {
        Lexer -> Token;

        rule Init {
            "(" => |lexer| lexer.return_(Token::LParen),
            ")" => |lexer| lexer.return_(Token::RParen),
            "+" => |lexer| lexer.return_(Token::Plus),
            "-" => |lexer| lexer.return_(Token::Minus),
            "*" => |lexer| lexer.return_(Token::Star),
            "/" => |lexer| lexer.return_(Token::Slash),
            ['0'-'9']+ => |lexer| {
                let str = lexer.match_();
                lexer.return_(Token::Int(str::parse::<i64>(str).unwrap()))
            },
        }
    }

    #[derive(Debug)]
    enum Op {
        Add,
        Sub,
        Mul,
        Div,
    }

    #[derive(Debug)]
    enum Expr {
        BinOp(Op, Box<Expr>, Box<Expr>),
        Int(i64),
    }

    parser! {
        enum Token {
            "(" => Token::LParen,
            ")" => Token::RParen,
            "+" => Token::Plus,
            "-" => Token::Minus,
            "*" => Token::Star,
            "/" => Token::Slash,
            "int" => Token::Int(<i64>),
        }

        pub Test1: () = {
            <l:"("> <r:")"> => (),
        };

        //pub Test2: String = {
        //    <s:"str"> => s,
        //};
    }
}
*/
