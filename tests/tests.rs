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

// First example in "An Efficient Context-Free Parsing Algorithm" by Earley
#[test]
fn earley_test() {
    parser! {
        pub E: () = {
            T => (),
            E "+" T => (),
        };

        pub T: () = {
            P => (),
            T "*" P => (),
        };

        pub P: () = {
            "a" => (),
        };
    }

    assert!(recognize(&mut "a".chars()));
    assert!(recognize(&mut "a+a".chars()));
    assert!(recognize(&mut "a+a*a".chars()));
    assert!(recognize(&mut "a*a+a".chars()));
    assert!(recognize(&mut "a*a+a+a*a".chars()));
}
