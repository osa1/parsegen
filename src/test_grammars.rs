use crate::grammar::{Grammar, NonTerminalIdx, Symbol, SymbolKind};
use crate::terminal::TerminalIdx;

fn nt(idx: NonTerminalIdx) -> Symbol {
    Symbol {
        binder: None,
        kind: SymbolKind::NonTerminal(idx),
    }
}

fn t(token: TerminalIdx) -> Symbol {
    Symbol {
        binder: None,
        kind: SymbolKind::Terminal(token),
    }
}

fn add_non_terminal(grammar: &mut Grammar<()>, non_terminal: &str, public: bool) -> NonTerminalIdx {
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
//
// ambiguous
#[allow(unused)]
pub fn grammar1() -> Grammar<()> {
    let mut grammar: Grammar<()> = Grammar::new();

    let e0_nt_idx = add_non_terminal(&mut grammar, "E0", true);
    let e_nt_idx = add_non_terminal(&mut grammar, "E", false);

    let plus = TerminalIdx::from_usize(0);
    let n = TerminalIdx::from_usize(1);

    // E0 -> E
    grammar.add_production(e0_nt_idx, vec![nt(e_nt_idx)], ());

    // E -> E + E
    grammar.add_production(e_nt_idx, vec![nt(e_nt_idx), t(plus), nt(e_nt_idx)], ());

    // E -> n
    grammar.add_production(e_nt_idx, vec![t(n)], ());

    grammar
}

// S0 -> S
// S -> AAAA
// A -> a
// A -> E
// E -> {empty}
//
// ambiguous
#[allow(unused)]
pub fn grammar2() -> Grammar<()> {
    let mut grammar: Grammar<()> = Grammar::new();

    let s0_nt_idx = add_non_terminal(&mut grammar, "S0", true);
    let s_nt_idx = add_non_terminal(&mut grammar, "S", false);
    let a_nt_idx = add_non_terminal(&mut grammar, "A", false);
    let e_nt_idx = add_non_terminal(&mut grammar, "E", false);

    let a = TerminalIdx::from_usize(0);

    // S0 -> S
    grammar.add_production(s0_nt_idx, vec![nt(s_nt_idx)], ());

    // S -> AAAA
    grammar.add_production(
        s_nt_idx,
        vec![nt(a_nt_idx), nt(a_nt_idx), nt(a_nt_idx), nt(a_nt_idx)],
        (),
    );

    // A -> a
    grammar.add_production(a_nt_idx, vec![t(a)], ());

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
//
// ambiguous: try "aab"
#[allow(unused)]
pub fn grammar3() -> Grammar<()> {
    let mut grammar: Grammar<()> = Grammar::new();

    let s_nt_idx = add_non_terminal(&mut grammar, "S", true);
    let a_nt_idx = add_non_terminal(&mut grammar, "A", false);
    let b_nt_idx = add_non_terminal(&mut grammar, "B", false);
    let c_nt_idx = add_non_terminal(&mut grammar, "C", false);

    let a = TerminalIdx::from_usize(0);
    let b = TerminalIdx::from_usize(1);
    let d = TerminalIdx::from_usize(2);

    // S -> A
    grammar.add_production(s_nt_idx, vec![nt(a_nt_idx)], ());

    // A -> Ba
    grammar.add_production(a_nt_idx, vec![nt(b_nt_idx), t(a)], ());

    // A -> Bb
    grammar.add_production(a_nt_idx, vec![nt(b_nt_idx), t(b)], ());

    // A -> Cab
    grammar.add_production(a_nt_idx, vec![nt(c_nt_idx), t(a), t(b)], ());

    // A -> Ad
    grammar.add_production(a_nt_idx, vec![nt(a_nt_idx), t(d)], ());

    // B -> a
    grammar.add_production(b_nt_idx, vec![t(a)], ());

    // C -> a
    grammar.add_production(c_nt_idx, vec![t(a)], ());

    grammar
}

// S0 -> S
// S -> ST | a
// B -> (empty)
// T -> aB | a
#[allow(unused)]
pub fn grammar4() -> Grammar<()> {
    let mut grammar: Grammar<()> = Grammar::new();

    let s0_nt_idx = add_non_terminal(&mut grammar, "S0", true);
    let s_nt_idx = add_non_terminal(&mut grammar, "S", false);
    let b_nt_idx = add_non_terminal(&mut grammar, "B", false);
    let t_nt_idx = add_non_terminal(&mut grammar, "T", false);

    let a = TerminalIdx::from_usize(0);

    // S0 -> S
    grammar.add_production(s0_nt_idx, vec![nt(s_nt_idx)], ());

    // S -> ST
    grammar.add_production(s_nt_idx, vec![nt(s_nt_idx), nt(t_nt_idx)], ());

    // S -> a
    grammar.add_production(s_nt_idx, vec![t(a)], ());

    // B -> (empty)
    grammar.add_production(b_nt_idx, vec![], ());

    // T -> aB
    grammar.add_production(t_nt_idx, vec![t(a), nt(b_nt_idx)], ());

    // T -> a
    grammar.add_production(t_nt_idx, vec![t(a)], ());

    grammar
}

pub const GRAMMAR5_PLUS: TerminalIdx = TerminalIdx::from_usize(0);
pub const GRAMMAR5_STAR: TerminalIdx = TerminalIdx::from_usize(1);
pub const GRAMMAR5_LPAREN: TerminalIdx = TerminalIdx::from_usize(2);
pub const GRAMMAR5_RPAREN: TerminalIdx = TerminalIdx::from_usize(3);
pub const GRAMMAR5_N: TerminalIdx = TerminalIdx::from_usize(4);

// E0 -> E
// E  -> T E'
// E' -> + T E' | (empty)
// T  -> F T'
// T' -> * F T' | (empty)
// F -> ( E ) | n
pub fn grammar5() -> Grammar<()> {
    let mut grammar: Grammar<()> = Grammar::new();

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
    grammar.add_production(
        e1_nt_idx,
        vec![t(GRAMMAR5_PLUS), nt(t_nt_idx), nt(e1_nt_idx)],
        (),
    );

    // E' -> (empty)
    grammar.add_production(e1_nt_idx, vec![], ());

    // T -> F T'
    grammar.add_production(t_nt_idx, vec![nt(f_nt_idx), nt(t1_nt_idx)], ());

    // T' -> * F T'
    grammar.add_production(
        t1_nt_idx,
        vec![t(GRAMMAR5_STAR), nt(f_nt_idx), nt(t1_nt_idx)],
        (),
    );

    // T' -> (empty)
    grammar.add_production(t1_nt_idx, vec![], ());

    // F -> ( E )
    grammar.add_production(
        f_nt_idx,
        vec![t(GRAMMAR5_LPAREN), nt(e_nt_idx), t(GRAMMAR5_RPAREN)],
        (),
    );

    // F -> n
    grammar.add_production(f_nt_idx, vec![t(GRAMMAR5_N)], ());

    grammar
}

pub const GRAMMAR6_PLUS: TerminalIdx = TerminalIdx::from_usize(0);
pub const GRAMMAR6_STAR: TerminalIdx = TerminalIdx::from_usize(1);
pub const GRAMMAR6_LPAREN: TerminalIdx = TerminalIdx::from_usize(2);
pub const GRAMMAR6_RPAREN: TerminalIdx = TerminalIdx::from_usize(3);
pub const GRAMMAR6_ID: TerminalIdx = TerminalIdx::from_usize(4);

// Figure 4.1 in dragon book. grammar5 is the left-factored version of this grammar.
//
// E0 -> E
// E -> E + T | T
// T -> T * F | F
// F -> ( E ) | id
pub fn grammar6() -> Grammar<()> {
    let mut grammar: Grammar<()> = Grammar::new();

    let e0_nt_idx = add_non_terminal(&mut grammar, "E0", true);
    let e_nt_idx = add_non_terminal(&mut grammar, "E", false);
    let t_nt_idx = add_non_terminal(&mut grammar, "T", false);
    let f_nt_idx = add_non_terminal(&mut grammar, "F", false);

    // E0 -> E
    grammar.add_production(e0_nt_idx, vec![nt(e_nt_idx)], ());

    // E -> E + T
    grammar.add_production(
        e_nt_idx,
        vec![nt(e_nt_idx), t(GRAMMAR6_PLUS), nt(t_nt_idx)],
        (),
    );

    // E -> T
    grammar.add_production(e_nt_idx, vec![nt(t_nt_idx)], ());

    // T -> T * F
    grammar.add_production(
        t_nt_idx,
        vec![nt(t_nt_idx), t(GRAMMAR6_STAR), nt(f_nt_idx)],
        (),
    );

    // T -> F
    grammar.add_production(t_nt_idx, vec![nt(f_nt_idx)], ());

    // F -> ( E )
    grammar.add_production(
        f_nt_idx,
        vec![t(GRAMMAR6_LPAREN), nt(e_nt_idx), t(GRAMMAR6_RPAREN)],
        (),
    );

    // F -> id
    grammar.add_production(f_nt_idx, vec![t(GRAMMAR6_ID)], ());

    grammar
}

pub const GRAMMAR7_EQ: TerminalIdx = TerminalIdx::from_usize(0);
pub const GRAMMAR7_STAR: TerminalIdx = TerminalIdx::from_usize(1);
pub const GRAMMAR7_ID: TerminalIdx = TerminalIdx::from_usize(2);

// Figure 4.49 (assignment with lvalues)
//
// S0 -> S
// S -> L = R | R
// L -> * R | id
// R -> L
//
// Not ambiguous, but not SLR(1)!
pub fn grammar7() -> Grammar<()> {
    let mut grammar: Grammar<()> = Grammar::new();

    let s0_nt_idx = add_non_terminal(&mut grammar, "S0", true);
    let s_nt_idx = add_non_terminal(&mut grammar, "S", false);
    let l_nt_idx = add_non_terminal(&mut grammar, "R", false);
    let r_nt_idx = add_non_terminal(&mut grammar, "R", false);

    // S0 -> S
    grammar.add_production(s0_nt_idx, vec![nt(s_nt_idx)], ());

    // S -> L = R
    grammar.add_production(
        s_nt_idx,
        vec![nt(l_nt_idx), t(GRAMMAR7_EQ), nt(r_nt_idx)],
        (),
    );

    // S -> R
    grammar.add_production(s_nt_idx, vec![nt(r_nt_idx)], ());

    // L -> * R
    grammar.add_production(l_nt_idx, vec![t(GRAMMAR7_STAR), nt(r_nt_idx)], ());

    // L -> id
    grammar.add_production(l_nt_idx, vec![t(GRAMMAR7_ID)], ());

    // R -> L
    grammar.add_production(r_nt_idx, vec![nt(l_nt_idx)], ());

    grammar
}

pub const GRAMMAR8_C: TerminalIdx = TerminalIdx::from_usize(0);
pub const GRAMMAR8_D: TerminalIdx = TerminalIdx::from_usize(1);

// Figure 4.55
//
// S0 -> S
// S -> C C
// C -> c C | d
//
pub fn grammar8() -> Grammar<()> {
    let mut grammar: Grammar<()> = Grammar::new();

    let s0_nt_idx = add_non_terminal(&mut grammar, "S0", true);
    let s_nt_idx = add_non_terminal(&mut grammar, "S", false);
    let c_nt_idx = add_non_terminal(&mut grammar, "C", false);

    // S0 -> S
    grammar.add_production(s0_nt_idx, vec![nt(s_nt_idx)], ());

    // S -> C C
    grammar.add_production(s_nt_idx, vec![nt(c_nt_idx), nt(c_nt_idx)], ());

    // C -> c C
    grammar.add_production(c_nt_idx, vec![t(GRAMMAR8_C), nt(c_nt_idx)], ());

    // C -> d
    grammar.add_production(c_nt_idx, vec![t(GRAMMAR8_D)], ());

    grammar
}

pub const GRAMMAR9_LPAREN: TerminalIdx = TerminalIdx::from_usize(0);
pub const GRAMMAR9_RPAREN: TerminalIdx = TerminalIdx::from_usize(1);

// S0 -> S
// S -> ( S ) | (empty)
pub fn grammar9() -> Grammar<()> {
    let mut grammar = Grammar::new();

    // NB: Deliberately inserted in reverse order. This breaks stuff.
    let s0_nt_idx = add_non_terminal(&mut grammar, "S0", true);
    let s_nt_idx = add_non_terminal(&mut grammar, "S", false);

    // S0 -> S
    grammar.add_production(s0_nt_idx, vec![nt(s_nt_idx)], ());

    // S -> ( S )
    grammar.add_production(
        s_nt_idx,
        vec![t(GRAMMAR9_LPAREN), nt(s_nt_idx), t(GRAMMAR9_RPAREN)],
        (),
    );

    // S -> (empty)
    grammar.add_production(s_nt_idx, vec![], ());

    grammar
}
