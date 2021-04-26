//! A lowered representation of grammars

use fxhash::FxHashSet;

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord, Hash)]
pub struct NonTerminalIdx(u32);

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord, Hash)]
pub struct ProductionIdx(u32);

/// Grammar type parameterized over terminals and user actions.
///
/// Note that the grammar is "augmented" with an initial non-terminal with no productions. When you
/// call `set_init` for setting the initial non-terminal we add a production to the original
/// initial non-terminal to the `set_init` argument.
#[derive(Debug)]
pub struct Grammar<T, A> {
    // Initial non-terminal
    init: Option<NonTerminalIdx>,

    // Indexed by `NonTerminalIdx`
    non_terminals: Vec<NonTerminal>,

    // Indexed by `ProductionIdx`
    productions: Vec<Production<T, A>>,
}

#[derive(Debug)]
pub struct NonTerminal {
    non_terminal: String,
    productions: FxHashSet<ProductionIdx>,
}

#[derive(Debug)]
pub struct Production<T, A> {
    symbols: Vec<Symbol<T>>,
    action: A,
}

#[derive(Debug)]
pub enum Symbol<T> {
    NonTerminal(NonTerminalIdx),
    Terminal(T),
}

impl<T, A> Grammar<T, A> {
    pub fn new() -> Self {
        Grammar {
            init: None,
            non_terminals: vec![],
            productions: vec![],
        }
    }

    pub fn set_init(&mut self, idx: NonTerminalIdx) {
        assert_eq!(self.init, None);
        self.init = Some(idx);
    }

    pub fn get_init(&self) -> NonTerminalIdx {
        self.init.unwrap()
    }

    pub fn add_non_terminal(&mut self, non_terminal: String) -> NonTerminalIdx {
        let idx = self.non_terminals.len();
        self.non_terminals.push(NonTerminal {
            non_terminal,
            productions: Default::default(),
        });
        NonTerminalIdx(idx as u32)
    }

    pub fn get_non_terminal(&self, idx: NonTerminalIdx) -> &NonTerminal {
        &self.non_terminals[idx.0 as usize]
    }

    pub fn add_production(
        &mut self,
        non_terminal: NonTerminalIdx,
        symbols: Vec<Symbol<T>>,
        action: A,
    ) -> ProductionIdx {
        let idx = self.productions.len();
        self.productions.push(Production { symbols, action });
        ProductionIdx(idx as u32)
    }

    pub fn get_production(&self, idx: ProductionIdx) -> &Production<T, A> {
        &self.productions[idx.0 as usize]
    }
}

impl NonTerminal {
    pub fn name(&self) -> &str {
        &self.non_terminal
    }

    pub fn productions(&self) -> &FxHashSet<ProductionIdx> {
        &self.productions
    }
}

impl<T, A> Production<T, A> {
    pub fn symbols(&self) -> &[Symbol<T>] {
        &self.symbols
    }
}
