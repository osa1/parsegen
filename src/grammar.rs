//! A lowered representation of grammars

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord, Hash)]
pub struct NonTerminalIdx(pub u32);

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord, Hash)]
pub struct ProductionIdx(pub u32);

/// Grammar type parameterized over terminals and user actions.
///
/// Note that the grammar is "augmented" with an initial non-terminal with no productions. When you
/// call `set_init` for setting the initial non-terminal we add a production to the original
/// initial non-terminal to the `set_init` argument.
#[derive(Debug)]
pub struct Grammar<T, A> {
    // Initial non-terminal
    pub init: Option<NonTerminalIdx>,

    // Indexed by `NonTerminalIdx`
    pub non_terminals: Vec<NonTerminal<T, A>>,
}

#[derive(Debug)]
pub struct NonTerminal<T, A> {
    pub non_terminal: String,
    // Indexed by `ProductionIdx`
    pub productions: Vec<Production<T, A>>,
}

#[derive(Debug)]
pub struct Production<T, A> {
    pub symbols: Vec<Symbol<T>>,
    pub action: A,
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

    pub fn get_non_terminal(&self, idx: NonTerminalIdx) -> &NonTerminal<T, A> {
        &self.non_terminals[idx.0 as usize]
    }

    pub fn add_production(
        &mut self,
        non_terminal: NonTerminalIdx,
        symbols: Vec<Symbol<T>>,
        action: A,
    ) -> ProductionIdx {
        let non_terminal = &mut self.non_terminals[non_terminal.0 as usize];
        let prod_idx = non_terminal.productions.len();
        non_terminal
            .productions
            .push(Production { symbols, action });
        ProductionIdx(prod_idx as u32)
    }

    pub fn get_production(
        &self,
        nt_idx: NonTerminalIdx,
        prod_idx: ProductionIdx,
    ) -> &Production<T, A> {
        &self.non_terminals[nt_idx.0 as usize].productions[prod_idx.0 as usize]
    }
}

impl<T, A> NonTerminal<T, A> {
    pub fn name(&self) -> &str {
        &self.non_terminal
    }

    pub fn productions(&self) -> &[Production<T, A>] {
        &self.productions
    }

    pub fn production_indices(&self) -> impl Iterator<Item = ProductionIdx> {
        (0..self.productions.len()).map(|i| ProductionIdx(i as u32))
    }
}

impl<T, A> Production<T, A> {
    pub fn symbols(&self) -> &[Symbol<T>] {
        &self.symbols
    }
}
