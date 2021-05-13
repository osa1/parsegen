use crate::grammar::{Grammar, NonTerminalIdx, Symbol, SymbolKind};

fn nt<T>(idx: NonTerminalIdx) -> Symbol<T> {
    Symbol {
        binder: None,
        kind: SymbolKind::NonTerminal(idx),
    }
}

fn t<T>(token: T) -> Symbol<T> {
    Symbol {
        binder: None,
        kind: SymbolKind::Terminal(token),
    }
}

fn add_non_terminal<T>(
    grammar: &mut Grammar<T, ()>,
    non_terminal: &str,
    public: bool,
) -> NonTerminalIdx {
    grammar.add_non_terminal(
        non_terminal.to_owned(),
        syn::Type::Tuple(syn::TypeTuple {
            paren_token: syn::token::Paren {
                span: proc_macro2::Span::call_site(),
            },
            elems: syn::punctuated::Punctuated::new(),
        }),
        public,
    )
}

// E0 -> E
// E -> E + E | n
pub fn grammar1() -> Grammar<char, ()> {
    let mut grammar: Grammar<char, ()> = Grammar::new();

    let e0_nt_idx = add_non_terminal(&mut grammar, "E0", true);
    let e_nt_idx = add_non_terminal(&mut grammar, "E", false);

    // E0 -> E
    grammar.add_production(e0_nt_idx, vec![nt(e_nt_idx)], ());

    // E -> E + E
    grammar.add_production(e_nt_idx, vec![nt(e_nt_idx), t('+'), nt(e_nt_idx)], ());

    // E -> n
    grammar.add_production(e_nt_idx, vec![t('n')], ());

    grammar
}

// S0 -> S
// S -> AAAA
// A -> a
// A -> E
// E -> {empty}
pub fn grammar2() -> Grammar<char, ()> {
    let mut grammar: Grammar<char, ()> = Grammar::new();

    let s0_nt_idx = add_non_terminal(&mut grammar, "S0", true);
    let s_nt_idx = add_non_terminal(&mut grammar, "S", false);
    let a_nt_idx = add_non_terminal(&mut grammar, "A", false);
    let e_nt_idx = add_non_terminal(&mut grammar, "E", false);

    // S0 -> S
    grammar.add_production(s0_nt_idx, vec![nt(s_nt_idx)], ());

    // S -> AAAA
    grammar.add_production(
        s_nt_idx,
        vec![nt(a_nt_idx), nt(a_nt_idx), nt(a_nt_idx), nt(a_nt_idx)],
        (),
    );

    // A -> a
    grammar.add_production(a_nt_idx, vec![t('a')], ());

    // A -> E
    grammar.add_production(a_nt_idx, vec![nt(e_nt_idx)], ());

    // E -> {empty}
    grammar.add_production(e_nt_idx, vec![], ());

    grammar
}

// S -> A
// A -> Ba | Bb | Cab | Ad
// B -> a
// C -> a
pub fn grammar3() -> Grammar<char, ()> {
    let mut grammar: Grammar<char, ()> = Grammar::new();

    let s_nt_idx = add_non_terminal(&mut grammar, "S", true);
    let a_nt_idx = add_non_terminal(&mut grammar, "A", false);
    let b_nt_idx = add_non_terminal(&mut grammar, "B", false);
    let c_nt_idx = add_non_terminal(&mut grammar, "C", false);

    // S -> A
    grammar.add_production(s_nt_idx, vec![nt(a_nt_idx)], ());

    // A -> Ba
    grammar.add_production(a_nt_idx, vec![nt(b_nt_idx), t('a')], ());

    // A -> Bb
    grammar.add_production(a_nt_idx, vec![nt(b_nt_idx), t('b')], ());

    // A -> Cab
    grammar.add_production(a_nt_idx, vec![nt(c_nt_idx), t('a'), t('b')], ());

    // A -> Ad
    grammar.add_production(a_nt_idx, vec![nt(a_nt_idx), t('d')], ());

    // B -> a
    grammar.add_production(b_nt_idx, vec![t('a')], ());

    // C -> a
    grammar.add_production(c_nt_idx, vec![t('a')], ());

    grammar
}

// S0 -> S
// S -> ST | a
// B -> (empty)
// T -> aB | a
pub fn grammar4() -> Grammar<char, ()> {
    let mut grammar: Grammar<char, ()> = Grammar::new();

    let s0_nt_idx = add_non_terminal(&mut grammar, "S0", true);
    let s_nt_idx = add_non_terminal(&mut grammar, "S", false);
    let b_nt_idx = add_non_terminal(&mut grammar, "B", false);
    let t_nt_idx = add_non_terminal(&mut grammar, "T", false);

    // S0 -> S
    grammar.add_production(s0_nt_idx, vec![nt(s_nt_idx)], ());

    // S -> ST
    grammar.add_production(s_nt_idx, vec![nt(s_nt_idx), nt(t_nt_idx)], ());

    // S -> a
    grammar.add_production(s_nt_idx, vec![t('a')], ());

    // B -> (empty)
    grammar.add_production(b_nt_idx, vec![], ());

    // T -> aB
    grammar.add_production(t_nt_idx, vec![t('a'), nt(b_nt_idx)], ());

    // T -> a
    grammar.add_production(t_nt_idx, vec![t('a')], ());

    grammar
}

// E0 -> E
// E  -> T E'
// E' -> + T E' | (empty)
// T  -> F T'
// T' -> * F T' | (empty)
// F -> ( E ) | n
pub fn grammar5() -> Grammar<char, ()> {
    let mut grammar: Grammar<char, ()> = Grammar::new();

    let e0_nt_idx = add_non_terminal(&mut grammar, "E0", true);
    let e_nt_idx = add_non_terminal(&mut grammar, "E", false);
    let e1_nt_idx = add_non_terminal(&mut grammar, "E'", false);
    let t_nt_idx = add_non_terminal(&mut grammar, "T", false);
    let t1_nt_idx = add_non_terminal(&mut grammar, "T'", false);
    let f_nt_idx = add_non_terminal(&mut grammar, "F", false);

    // E0 -> E
    grammar.add_production(e0_nt_idx, vec![nt(e_nt_idx)], ());

    // E -> T E'
    grammar.add_production(e_nt_idx, vec![nt(t_nt_idx), nt(e1_nt_idx)], ());

    // E' -> + T E'
    grammar.add_production(e1_nt_idx, vec![t('+'), nt(t_nt_idx), nt(e1_nt_idx)], ());

    // E' -> (empty)
    grammar.add_production(e1_nt_idx, vec![], ());

    // T -> F T'
    grammar.add_production(t_nt_idx, vec![nt(f_nt_idx), nt(t1_nt_idx)], ());

    // T' -> * F T'
    grammar.add_production(t1_nt_idx, vec![t('*'), nt(f_nt_idx), nt(t1_nt_idx)], ());

    // T' -> (empty)
    grammar.add_production(t1_nt_idx, vec![], ());

    // F -> ( E )
    grammar.add_production(f_nt_idx, vec![t('('), nt(e_nt_idx), t(')')], ());

    // F -> n
    grammar.add_production(f_nt_idx, vec![t('n')], ());

    grammar
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum Grammar6Token {
    LParen,
    RParen,
    Plus,
    Star,
    Id,
}

// Figure 4.1 in dragon book. grammar5 is the left-factored version of this grammar.
//
// E0 -> E
// E -> E + T | T
// T -> T * F | F
// F -> ( E ) | id
pub fn grammar6() -> Grammar<Grammar6Token, ()> {
    let mut grammar: Grammar<Grammar6Token, ()> = Grammar::new();

    let e0_nt_idx = add_non_terminal(&mut grammar, "E0", true);
    let e_nt_idx = add_non_terminal(&mut grammar, "E", false);
    let t_nt_idx = add_non_terminal(&mut grammar, "T", false);
    let f_nt_idx = add_non_terminal(&mut grammar, "F", false);

    // E0 -> E
    grammar.add_production(e0_nt_idx, vec![nt(e_nt_idx)], ());

    // E -> E + T
    grammar.add_production(
        e_nt_idx,
        vec![nt(e_nt_idx), t(Grammar6Token::Plus), nt(t_nt_idx)],
        (),
    );

    // E -> T
    grammar.add_production(e_nt_idx, vec![nt(t_nt_idx)], ());

    // T -> T * F
    grammar.add_production(
        t_nt_idx,
        vec![nt(t_nt_idx), t(Grammar6Token::Star), nt(f_nt_idx)],
        (),
    );

    // T -> F
    grammar.add_production(t_nt_idx, vec![nt(f_nt_idx)], ());

    // F -> ( E )
    grammar.add_production(
        f_nt_idx,
        vec![
            t(Grammar6Token::LParen),
            nt(e_nt_idx),
            t(Grammar6Token::RParen),
        ],
        (),
    );

    // F -> id
    grammar.add_production(f_nt_idx, vec![t(Grammar6Token::Id)], ());

    grammar
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum Grammar7Token {
    Eq,
    Star,
    Id,
}

// Figure 4.49 (assignment with lvalues)
//
// S0 -> S
// S -> L = R | R
// L -> * R | id
// R -> L
//
// Not ambiguous, but not SLR(1)!
pub fn grammar7() -> Grammar<Grammar7Token, ()> {
    let mut grammar: Grammar<Grammar7Token, ()> = Grammar::new();

    let s0_nt_idx = add_non_terminal(&mut grammar, "S0", true);
    let s_nt_idx = add_non_terminal(&mut grammar, "S", false);
    let l_nt_idx = add_non_terminal(&mut grammar, "R", false);
    let r_nt_idx = add_non_terminal(&mut grammar, "R", false);

    // S0 -> S
    grammar.add_production(s0_nt_idx, vec![nt(s_nt_idx)], ());

    // S -> L = R
    grammar.add_production(
        s_nt_idx,
        vec![nt(l_nt_idx), t(Grammar7Token::Eq), nt(r_nt_idx)],
        (),
    );

    // S -> R
    grammar.add_production(s_nt_idx, vec![nt(r_nt_idx)], ());

    // L -> * R
    grammar.add_production(l_nt_idx, vec![t(Grammar7Token::Star), nt(r_nt_idx)], ());

    // L -> id
    grammar.add_production(l_nt_idx, vec![t(Grammar7Token::Id)], ());

    // R -> L
    grammar.add_production(r_nt_idx, vec![nt(l_nt_idx)], ());

    grammar
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Grammar8Token {
    C,
    D,
}

// Custom Debug impl to print in lowercase, otherwise things get confusing as non-terminals in this
// grammar have same names as terminals
impl std::fmt::Debug for Grammar8Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Grammar8Token::C => write!(f, "c"),
            Grammar8Token::D => write!(f, "d"),
        }
    }
}

// Figure 4.55
//
// S0 -> S
// S -> C C
// C -> c C | d
//
pub fn grammar8() -> Grammar<Grammar8Token, ()> {
    let mut grammar: Grammar<Grammar8Token, ()> = Grammar::new();

    let s0_nt_idx = add_non_terminal(&mut grammar, "S0", true);
    let s_nt_idx = add_non_terminal(&mut grammar, "S", false);
    let c_nt_idx = add_non_terminal(&mut grammar, "C", false);

    // S0 -> S
    grammar.add_production(s0_nt_idx, vec![nt(s_nt_idx)], ());

    // S -> C C
    grammar.add_production(s_nt_idx, vec![nt(c_nt_idx), nt(c_nt_idx)], ());

    // C -> c C
    grammar.add_production(c_nt_idx, vec![t(Grammar8Token::C), nt(c_nt_idx)], ());

    // C -> d
    grammar.add_production(c_nt_idx, vec![t(Grammar8Token::D)], ());

    grammar
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Grammar9Token {
    LParen,
    RParen,
}

// S0 -> S
// S -> ( S ) | (empty)
pub fn grammar9() -> Grammar<Grammar9Token, ()> {
    let mut grammar = Grammar::new();

    let s0_nt_idx = add_non_terminal(&mut grammar, "S0", true);
    let s_nt_idx = add_non_terminal(&mut grammar, "S", false);

    // S0 -> S
    grammar.add_production(s0_nt_idx, vec![nt(s_nt_idx)], ());

    // S -> ( S )
    grammar.add_production(
        s_nt_idx,
        vec![
            t(Grammar9Token::LParen),
            nt(s_nt_idx),
            t(Grammar9Token::RParen),
        ],
        (),
    );

    // S -> (empty)
    grammar.add_production(s_nt_idx, vec![], ());

    grammar
}
