use crate::grammar::{Grammar, NonTerminalIdx, BoundSymbol, Symbol, TerminalIdx};

use fxhash::FxHashMap;

pub struct TestGrammar<A> {
    grammar: Grammar<A>,
    terminals: FxHashMap<&'static str, TerminalIdx>,
}

impl<A> TestGrammar<A> {
    pub fn t(&self, terminal: &'static str) -> TerminalIdx {
        *self.terminals.get(terminal).unwrap()
    }

    pub fn get_grammar(&self) -> &Grammar<A> {
        &self.grammar
    }

    fn new() -> TestGrammar<A> {
        TestGrammar {
            grammar: Grammar::new(),
            terminals: Default::default(),
        }
    }

    fn add_t(&mut self, terminal: &'static str) -> TerminalIdx {
        let idx = self.grammar.new_terminal(terminal.to_owned());
        let old = self.terminals.insert(terminal, idx);
        assert_eq!(old, None);
        idx
    }

    fn add_nt(&mut self, non_terminal: &str, public: bool) -> NonTerminalIdx {
        self.grammar.add_non_terminal(
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

    fn add_p(&mut self, non_terminal: NonTerminalIdx, symbols: Vec<BoundSymbol>, action: A) {
        self.grammar.add_production(non_terminal, symbols, action);
    }
}

fn nt(idx: NonTerminalIdx) -> BoundSymbol {
    BoundSymbol {
        binder: None,
        symbol: Symbol::NonTerminal(idx),
    }
}

fn t(token: TerminalIdx) -> BoundSymbol {
    BoundSymbol {
        binder: None,
        symbol: Symbol::Terminal(token),
    }
}

// E0 -> E
// E -> E + E | n
//
// ambiguous
#[allow(unused)]
pub fn grammar1() -> TestGrammar<()> {
    let mut g = TestGrammar::new();

    let e0_nt_idx = g.add_nt("E0", true);
    let e_nt_idx = g.add_nt("E", false);

    let plus = g.add_t("+");
    let n = g.add_t("n");

    // E0 -> E
    g.add_p(e0_nt_idx, vec![nt(e_nt_idx)], ());

    // E -> E + E
    g.add_p(e_nt_idx, vec![nt(e_nt_idx), t(plus), nt(e_nt_idx)], ());

    // E -> n
    g.add_p(e_nt_idx, vec![t(n)], ());

    g
}

// S0 -> S
// S -> AAAA
// A -> a
// A -> E
// E -> {empty}
//
// ambiguous
#[allow(unused)]
pub fn grammar2() -> TestGrammar<()> {
    let mut g = TestGrammar::new();

    let s0_nt_idx = g.add_nt("S0", true);
    let s_nt_idx = g.add_nt("S", false);
    let a_nt_idx = g.add_nt("A", false);
    let e_nt_idx = g.add_nt("E", false);

    let a = g.add_t("a");

    // S0 -> S
    g.add_p(s0_nt_idx, vec![nt(s_nt_idx)], ());

    // S -> AAAA
    g.add_p(
        s_nt_idx,
        vec![nt(a_nt_idx), nt(a_nt_idx), nt(a_nt_idx), nt(a_nt_idx)],
        (),
    );

    // A -> a
    g.add_p(a_nt_idx, vec![t(a)], ());

    // A -> E
    g.add_p(a_nt_idx, vec![nt(e_nt_idx)], ());

    // E -> {empty}
    g.add_p(e_nt_idx, vec![], ());

    g
}

// S -> A
// A -> Ba | Bb | Cab | Ad
// B -> a
// C -> a
//
// ambiguous: try "aab"
#[allow(unused)]
pub fn grammar3() -> TestGrammar<()> {
    let mut g = TestGrammar::new();

    let s_nt_idx = g.add_nt("S", true);
    let a_nt_idx = g.add_nt("A", false);
    let b_nt_idx = g.add_nt("B", false);
    let c_nt_idx = g.add_nt("C", false);

    let a = g.add_t("a");
    let b = g.add_t("b");
    let d = g.add_t("d");

    // S -> A
    g.add_p(s_nt_idx, vec![nt(a_nt_idx)], ());

    // A -> Ba
    g.add_p(a_nt_idx, vec![nt(b_nt_idx), t(a)], ());

    // A -> Bb
    g.add_p(a_nt_idx, vec![nt(b_nt_idx), t(b)], ());

    // A -> Cab
    g.add_p(a_nt_idx, vec![nt(c_nt_idx), t(a), t(b)], ());

    // A -> Ad
    g.add_p(a_nt_idx, vec![nt(a_nt_idx), t(d)], ());

    // B -> a
    g.add_p(b_nt_idx, vec![t(a)], ());

    // C -> a
    g.add_p(c_nt_idx, vec![t(a)], ());

    g
}

// S0 -> S
// S -> ST | a
// B -> (empty)
// T -> aB | a
#[allow(unused)]
pub fn grammar4() -> TestGrammar<()> {
    let mut g = TestGrammar::new();

    let s0_nt_idx = g.add_nt("S0", true);
    let s_nt_idx = g.add_nt("S", false);
    let b_nt_idx = g.add_nt("B", false);
    let t_nt_idx = g.add_nt("T", false);

    let a = g.add_t("a");

    // S0 ->grammar.next_terminal_idx();
    g.add_p(s0_nt_idx, vec![nt(s_nt_idx)], ());

    // S -> ST
    g.add_p(s_nt_idx, vec![nt(s_nt_idx), nt(t_nt_idx)], ());

    // S -> a
    g.add_p(s_nt_idx, vec![t(a)], ());

    // B -> (empty)
    g.add_p(b_nt_idx, vec![], ());

    // T -> aB
    g.add_p(t_nt_idx, vec![t(a), nt(b_nt_idx)], ());

    // T -> a
    g.add_p(t_nt_idx, vec![t(a)], ());

    g
}

// E0 -> E
// E  -> T E'
// E' -> + T E' | (empty)
// T  -> F T'
// T' -> * F T' | (empty)
// F -> ( E ) | n
pub fn grammar5() -> TestGrammar<()> {
    let mut g = TestGrammar::new();

    let e0_nt_idx = g.add_nt("E0", true);
    let e_nt_idx = g.add_nt("E", false);
    let e1_nt_idx = g.add_nt("E'", false);
    let t_nt_idx = g.add_nt("T", false);
    let t1_nt_idx = g.add_nt("T'", false);
    let f_nt_idx = g.add_nt("F", false);

    let plus = g.add_t("+");
    let star = g.add_t("*");
    let lparen = g.add_t("(");
    let rparen = g.add_t(")");
    let n = g.add_t("n");

    // E0 -> E
    g.add_p(e0_nt_idx, vec![nt(e_nt_idx)], ());

    // E -> T E'
    g.add_p(e_nt_idx, vec![nt(t_nt_idx), nt(e1_nt_idx)], ());

    // E' -> + T E'
    g.add_p(e1_nt_idx, vec![t(plus), nt(t_nt_idx), nt(e1_nt_idx)], ());

    // E' -> (empty)
    g.add_p(e1_nt_idx, vec![], ());

    // T -> F T'
    g.add_p(t_nt_idx, vec![nt(f_nt_idx), nt(t1_nt_idx)], ());

    // T' -> * F T'
    g.add_p(t1_nt_idx, vec![t(star), nt(f_nt_idx), nt(t1_nt_idx)], ());

    // T' -> (empty)
    g.add_p(t1_nt_idx, vec![], ());

    // F -> ( E )
    g.add_p(f_nt_idx, vec![t(lparen), nt(e_nt_idx), t(rparen)], ());

    // F -> n
    g.add_p(f_nt_idx, vec![t(n)], ());

    g
}

// Figure 4.1 in dragon book. grammar5 is the left-factored version of this grammar.
//
// E0 -> E
// E -> E + T | T
// T -> T * F | F
// F -> ( E ) | id
pub fn grammar6() -> TestGrammar<()> {
    let mut g = TestGrammar::new();

    let e0_nt_idx = g.add_nt("E0", true);
    let e_nt_idx = g.add_nt("E", false);
    let t_nt_idx = g.add_nt("T", false);
    let f_nt_idx = g.add_nt("F", false);

    let plus = g.add_t("+");
    let star = g.add_t("*");
    let lparen = g.add_t("(");
    let rparen = g.add_t(")");
    let id = g.add_t("id");

    // E0 -> E
    g.add_p(e0_nt_idx, vec![nt(e_nt_idx)], ());

    // E -> E + T
    g.add_p(e_nt_idx, vec![nt(e_nt_idx), t(plus), nt(t_nt_idx)], ());

    // E -> T
    g.add_p(e_nt_idx, vec![nt(t_nt_idx)], ());

    // T -> T * F
    g.add_p(t_nt_idx, vec![nt(t_nt_idx), t(star), nt(f_nt_idx)], ());

    // T -> F
    g.add_p(t_nt_idx, vec![nt(f_nt_idx)], ());

    // F -> ( E )
    g.add_p(f_nt_idx, vec![t(lparen), nt(e_nt_idx), t(rparen)], ());

    // F -> id
    g.add_p(f_nt_idx, vec![t(id)], ());

    g
}

// Figure 4.49 (assignment with lvalues)
//
// S0 -> S
// S -> L = R | R
// L -> * R | id
// R -> L
//
// Not ambiguous, but not SLR(1)!
pub fn grammar7() -> TestGrammar<()> {
    let mut g = TestGrammar::new();

    let s0_nt_idx = g.add_nt("S0", true);
    let s_nt_idx = g.add_nt("S", false);
    let l_nt_idx = g.add_nt("R", false);
    let r_nt_idx = g.add_nt("R", false);

    let eq = g.add_t("=");
    let star = g.add_t("*");
    let id = g.add_t("id");

    // S0 -> S
    g.add_p(s0_nt_idx, vec![nt(s_nt_idx)], ());

    // S -> L = R
    g.add_p(s_nt_idx, vec![nt(l_nt_idx), t(eq), nt(r_nt_idx)], ());

    // S -> R
    g.add_p(s_nt_idx, vec![nt(r_nt_idx)], ());

    // L -> * R
    g.add_p(l_nt_idx, vec![t(star), nt(r_nt_idx)], ());

    // L -> id
    g.add_p(l_nt_idx, vec![t(id)], ());

    // R -> L
    g.add_p(r_nt_idx, vec![nt(l_nt_idx)], ());

    g
}

// Figure 4.55
//
// S0 -> S
// S -> C C
// C -> c C | d
//
pub fn grammar8() -> TestGrammar<()> {
    let mut g = TestGrammar::new();

    let s0_nt_idx = g.add_nt("S0", true);
    let s_nt_idx = g.add_nt("S", false);
    let c_nt_idx = g.add_nt("C", false);

    let c = g.add_t("c");
    let d = g.add_t("d");

    // S0 -> S
    g.add_p(s0_nt_idx, vec![nt(s_nt_idx)], ());

    // S -> C C
    g.add_p(s_nt_idx, vec![nt(c_nt_idx), nt(c_nt_idx)], ());

    // C -> c C
    g.add_p(c_nt_idx, vec![t(c), nt(c_nt_idx)], ());

    // C -> d
    g.add_p(c_nt_idx, vec![t(d)], ());

    g
}

// S0 -> S
// S -> ( S ) | (empty)
pub fn grammar9() -> TestGrammar<()> {
    let mut g = TestGrammar::new();

    // NB: Deliberately inserted in reverse order. This breaks stuff.
    let s0_nt_idx = g.add_nt("S0", true);
    let s_nt_idx = g.add_nt("S", false);

    let lparen = g.add_t("(");
    let rparen = g.add_t(")");

    // S0 -> S
    g.add_p(s0_nt_idx, vec![nt(s_nt_idx)], ());

    // S -> ( S )
    g.add_p(s_nt_idx, vec![t(lparen), nt(s_nt_idx), t(rparen)], ());

    // S -> (empty)
    g.add_p(s_nt_idx, vec![], ());

    g
}

// S0 -> S1
// S1 -> A S1 c
// S1 -> b
// A -> (empty)
//
// Generates (empty)^n b c^n. Not LR(1): in state 0 we have a conflict between shifting `b` vs.
// reducing `A`.
#[allow(unused)]
pub fn hidden_left_recursion() -> TestGrammar<()> {
    let mut g = TestGrammar::new();

    let s0_nt = g.add_nt("S0", true);
    let s1_nt = g.add_nt("S1", false);
    let a_nt = g.add_nt("A", false);

    let b = g.add_t("b");
    let c = g.add_t("c");

    // S0 -> S1
    g.add_p(s0_nt, vec![nt(s1_nt)], ());

    // S1 -> A S1 c
    g.add_p(s1_nt, vec![nt(a_nt), nt(s1_nt), t(c)], ());

    // S1 -> b
    g.add_p(s1_nt, vec![t(b)], ());

    // A -> (empty)
    g.add_p(a_nt, vec![], ());

    g
}
