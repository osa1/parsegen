use crate::grammar::{Grammar, Symbol};

// E -> E + E | n
pub fn grammar1() -> Grammar<char, ()> {
    let mut grammar: Grammar<char, ()> = Grammar::new();
    let e_nt_idx = grammar.add_non_terminal("E".to_owned());
    grammar.add_production(
        e_nt_idx,
        vec![
            Symbol::NonTerminal(e_nt_idx),
            Symbol::Terminal('+'),
            Symbol::NonTerminal(e_nt_idx),
        ],
        (),
    );
    grammar.add_production(e_nt_idx, vec![Symbol::Terminal('n')], ());
    grammar.set_init(e_nt_idx);
    grammar
}

// S -> AAAA
// A -> a
// A -> E
// E -> {empty}
pub fn grammar2() -> Grammar<char, ()> {
    let mut grammar: Grammar<char, ()> = Grammar::new();
    let s_nt_idx = grammar.add_non_terminal("S".to_owned());
    let a_nt_idx = grammar.add_non_terminal("A".to_owned());
    let e_nt_idx = grammar.add_non_terminal("E".to_owned());

    grammar.set_init(s_nt_idx);

    grammar.add_production(
        s_nt_idx,
        vec![
            Symbol::NonTerminal(a_nt_idx),
            Symbol::NonTerminal(a_nt_idx),
            Symbol::NonTerminal(a_nt_idx),
            Symbol::NonTerminal(a_nt_idx),
        ],
        (),
    );
    grammar.add_production(a_nt_idx, vec![Symbol::Terminal('a')], ());
    grammar.add_production(a_nt_idx, vec![Symbol::NonTerminal(e_nt_idx)], ());
    grammar.add_production(e_nt_idx, vec![], ());
    grammar
}

// S -> A
// A -> Ba | Bb | Cab | Ad
// B -> a
// C -> a
pub fn grammar3() -> Grammar<char, ()> {
    let mut grammar: Grammar<char, ()> = Grammar::new();
    let s_nt_idx = grammar.add_non_terminal("S".to_owned());
    let a_nt_idx = grammar.add_non_terminal("A".to_owned());
    let b_nt_idx = grammar.add_non_terminal("B".to_owned());
    let c_nt_idx = grammar.add_non_terminal("C".to_owned());

    grammar.set_init(s_nt_idx);

    // S -> A
    grammar.add_production(s_nt_idx, vec![Symbol::NonTerminal(a_nt_idx)], ());
    // A -> Ba
    grammar.add_production(
        a_nt_idx,
        vec![Symbol::NonTerminal(b_nt_idx), Symbol::Terminal('a')],
        (),
    );
    // A -> Bb
    grammar.add_production(
        a_nt_idx,
        vec![Symbol::NonTerminal(b_nt_idx), Symbol::Terminal('b')],
        (),
    );
    // A -> Cab
    grammar.add_production(
        a_nt_idx,
        vec![
            Symbol::NonTerminal(c_nt_idx),
            Symbol::Terminal('a'),
            Symbol::Terminal('b'),
        ],
        (),
    );
    // A -> Ad
    grammar.add_production(
        a_nt_idx,
        vec![Symbol::NonTerminal(a_nt_idx), Symbol::Terminal('d')],
        (),
    );
    // B -> a
    grammar.add_production(b_nt_idx, vec![Symbol::Terminal('a')], ());
    // C -> a
    grammar.add_production(c_nt_idx, vec![Symbol::Terminal('a')], ());

    grammar
}

// S -> ST | a
// B -> (empty)
// T -> aB | a
pub fn grammar4() -> Grammar<char, ()> {
    let mut grammar: Grammar<char, ()> = Grammar::new();
    let s_nt_idx = grammar.add_non_terminal("S".to_owned());
    let b_nt_idx = grammar.add_non_terminal("B".to_owned());
    let t_nt_idx = grammar.add_non_terminal("T".to_owned());

    grammar.set_init(s_nt_idx);

    // S -> ST
    grammar.add_production(
        s_nt_idx,
        vec![Symbol::NonTerminal(s_nt_idx), Symbol::NonTerminal(t_nt_idx)],
        (),
    );
    // S -> a
    grammar.add_production(s_nt_idx, vec![Symbol::Terminal('a')], ());
    // B -> (empty)
    grammar.add_production(b_nt_idx, vec![], ());
    // T -> aB
    grammar.add_production(
        t_nt_idx,
        vec![Symbol::Terminal('a'), Symbol::NonTerminal(b_nt_idx)],
        (),
    );
    // T -> a
    grammar.add_production(t_nt_idx, vec![Symbol::Terminal('a')], ());

    grammar
}
