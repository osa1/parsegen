use parsegen::parser;

// Figure 1 in "Practical Earley Parsing"
#[test]
fn test_figure_1() {
    parser! {
        pub S: () = {
            <e:E> => e,
        };

        pub E: () = {
            <e1:E> "+" <e2:E> => (),
            "n" => (),
        };
    }

    assert!(recognize(&mut "n".chars()));
    assert!(recognize(&mut "n+n".chars()));
    assert!(recognize(&mut "n+n+n".chars()));
    assert!(recognize(&mut "n+n+n+n".chars()));
    assert!(!recognize(&mut "n+n+n+n+".chars()));
}

// Figure 2 in "Practical Earley Parsing"
#[test]
fn test_figure_2() {
    parser! {
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

    assert!(recognize(&mut "a".chars()));
    assert!(recognize(&mut "aa".chars()));
    assert!(recognize(&mut "aaa".chars()));
    assert!(recognize(&mut "aaaa".chars()));
    assert!(!recognize(&mut "aaaaa".chars()));
}
