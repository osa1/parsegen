//! A lowered representation of grammars

use crate::ast;

use std::convert::TryFrom;

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord, Hash)]
pub struct NonTerminalIdx(pub u32);

impl NonTerminalIdx {
    pub fn as_usize(self) -> usize {
        self.0 as usize
    }

    pub fn from_usize(i: usize) -> Self {
        Self(u32::try_from(i).unwrap())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord, Hash)]
pub struct ProductionIdx(pub u32);

impl ProductionIdx {
    pub fn as_usize(self) -> usize {
        self.0 as usize
    }

    pub fn from_usize(i: usize) -> Self {
        Self(u32::try_from(i).unwrap())
    }
}

/// Grammar type parameterized over terminals and user actions.
#[derive(Debug, Clone)]
pub struct Grammar<T, A> {
    // Indexed by `NonTerminalIdx`
    pub non_terminals: Vec<NonTerminal<T, A>>,
}

#[derive(Debug, Clone)]
pub struct NonTerminal<T, A> {
    pub non_terminal: String,
    // Indexed by `ProductionIdx`
    pub productions: Vec<Production<T, A>>,
    pub return_ty: syn::Type,
    pub public: bool,
}

#[derive(Clone)]
pub struct Production<T, A> {
    pub symbols: Vec<Symbol<T>>,
    pub action: A,
}

impl<T: std::fmt::Debug, A> std::fmt::Debug for Production<T, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Production")
            .field("symbols", &self.symbols)
            .field("action", &"...")
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct Symbol<T> {
    pub binder: Option<ast::Name>,
    pub kind: SymbolKind<T>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum SymbolKind<T> {
    NonTerminal(NonTerminalIdx),
    Terminal(T),
}

impl<T, A> Grammar<T, A> {
    pub fn new() -> Self {
        Grammar {
            non_terminals: vec![],
        }
    }

    pub fn add_non_terminal(
        &mut self,
        non_terminal: String,
        return_ty: syn::Type,
        public: bool,
    ) -> NonTerminalIdx {
        let idx = self.non_terminals.len();
        self.non_terminals.push(NonTerminal {
            non_terminal,
            productions: Default::default(),
            return_ty,
            public,
        });
        NonTerminalIdx(idx as u32)
    }

    pub fn get_non_terminal(&self, idx: NonTerminalIdx) -> &NonTerminal<T, A> {
        &self.non_terminals[idx.0 as usize]
    }

    pub fn get_non_terminal_idx(&self, name: &str) -> Option<NonTerminalIdx> {
        self.non_terminals
            .iter()
            .enumerate()
            .find_map(|(idx, non_terminal)| {
                if non_terminal.non_terminal == name {
                    Some(NonTerminalIdx::from_usize(idx))
                } else {
                    None
                }
            })
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

    pub fn get_production(&self, production_idx: ProductionIdx) -> &Production<T, A> {
        &self.productions[production_idx.0 as usize]
    }
}

impl<T, A> Production<T, A> {
    pub fn symbols(&self) -> &[Symbol<T>] {
        &self.symbols
    }
}

struct ProductionIndicesIter<'grammar, T, A> {
    grammar: &'grammar Grammar<T, A>,
    non_terminal_idx: NonTerminalIdx,
    production_idx: ProductionIdx,
}

struct NonTerminalIndicesIter<'grammar, T, A> {
    grammar: &'grammar Grammar<T, A>,
    non_terminal_idx: NonTerminalIdx,
}

struct NonTerminalProductionIndicesIter<'grammar, T, A> {
    grammar: &'grammar Grammar<T, A>,
    non_terminal_idx: NonTerminalIdx,
    production_idx: ProductionIdx,
}

impl<'grammar, T, A> Iterator for ProductionIndicesIter<'grammar, T, A> {
    type Item = (NonTerminalIdx, ProductionIdx, &'grammar Production<T, A>);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self
                .grammar
                .non_terminals
                .get(self.non_terminal_idx.as_usize())
            {
                None => return None,
                Some(non_terminal) => {
                    match non_terminal.productions.get(self.production_idx.as_usize()) {
                        None => {
                            self.non_terminal_idx = NonTerminalIdx(self.non_terminal_idx.0 + 1);
                            self.production_idx = ProductionIdx(0);
                        }
                        Some(production) => {
                            let production_idx = self.production_idx;
                            self.production_idx = ProductionIdx(self.production_idx.0 + 1);
                            return Some((self.non_terminal_idx, production_idx, production));
                        }
                    }
                }
            }
        }
    }
}

impl<'grammar, T, A> Iterator for NonTerminalIndicesIter<'grammar, T, A> {
    type Item = (NonTerminalIdx, &'grammar NonTerminal<T, A>);

    fn next(&mut self) -> Option<Self::Item> {
        self.grammar
            .non_terminals
            .get(self.non_terminal_idx.as_usize())
            .map(|non_terminal| {
                let idx = self.non_terminal_idx;
                self.non_terminal_idx = NonTerminalIdx(self.non_terminal_idx.0 + 1);
                (idx, non_terminal)
            })
    }
}

impl<'grammar, T, A> Iterator for NonTerminalProductionIndicesIter<'grammar, T, A> {
    type Item = (ProductionIdx, &'grammar Production<T, A>);

    fn next(&mut self) -> Option<Self::Item> {
        let production_idx = self.production_idx;
        self.grammar.non_terminals[self.non_terminal_idx.as_usize()]
            .productions
            .get(production_idx.as_usize())
            .map(|production| {
                self.production_idx = ProductionIdx(production_idx.0 + 1);
                (production_idx, production)
            })
    }
}

impl<T, A> Grammar<T, A> {
    pub fn production_indices(
        &self,
    ) -> impl Iterator<Item = (NonTerminalIdx, ProductionIdx, &Production<T, A>)> {
        ProductionIndicesIter {
            grammar: self,
            non_terminal_idx: NonTerminalIdx(0),
            production_idx: ProductionIdx(0),
        }
    }

    pub fn non_terminals(&self) -> &[NonTerminal<T, A>] {
        &self.non_terminals
    }

    pub fn non_terminal_indices(
        &self,
    ) -> impl Iterator<Item = (NonTerminalIdx, &NonTerminal<T, A>)> {
        NonTerminalIndicesIter {
            grammar: self,
            non_terminal_idx: NonTerminalIdx(0),
        }
    }

    pub fn non_terminal_production_indices(
        &self,
        non_terminal: NonTerminalIdx,
    ) -> impl Iterator<Item = (ProductionIdx, &Production<T, A>)> {
        NonTerminalProductionIndicesIter {
            grammar: self,
            non_terminal_idx: non_terminal,
            production_idx: ProductionIdx(0),
        }
    }
}

pub struct SymbolKindDisplay<'a, 'b, T, A> {
    symbol: &'a SymbolKind<T>,
    grammar: &'b Grammar<T, A>,
}

impl<'a, 'b, T, A> SymbolKindDisplay<'a, 'b, T, A> {
    pub fn new(symbol: &'a SymbolKind<T>, grammar: &'b Grammar<T, A>) -> Self {
        Self { symbol, grammar }
    }
}

pub struct ProductionDisplay<'a, 'b, T, A> {
    production: &'a Production<T, A>,
    grammar: &'b Grammar<T, A>,
}

impl<'a, 'b, T, A> ProductionDisplay<'a, 'b, T, A> {
    pub fn new(production: &'a Production<T, A>, grammar: &'b Grammar<T, A>) -> Self {
        Self {
            production,
            grammar,
        }
    }
}

use std::fmt;

impl<T: fmt::Debug, A> fmt::Display for Grammar<T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (nt_idx, nt) in self.non_terminals.iter().enumerate() {
            writeln!(
                f,
                "{}: {} {} = {{",
                nt_idx,
                if nt.public { "pub" } else { "" },
                nt.non_terminal
            )?;

            for (p_idx, p) in nt.productions.iter().enumerate() {
                writeln!(
                    f,
                    "  {}: {}",
                    p_idx,
                    ProductionDisplay {
                        production: p,
                        grammar: self
                    }
                )?;
            }

            writeln!(f, "}}")?;
        }

        Ok(())
    }
}

impl<'a, 'b, T: fmt::Debug, A> fmt::Display for ProductionDisplay<'a, 'b, T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (symbol_idx, symbol) in self.production.symbols().iter().enumerate() {
            match &symbol.kind {
                SymbolKind::NonTerminal(nt) => {
                    let nt = self.grammar.get_non_terminal(*nt);
                    write!(f, "{}", nt.non_terminal)?;
                }
                SymbolKind::Terminal(t) => {
                    write!(f, "{:?}", t)?;
                }
            }
            if symbol_idx != self.production.symbols().len() - 1 {
                write!(f, " ")?;
            }
        }
        Ok(())
    }
}

impl<'a, 'b, T: Clone + fmt::Debug, A> fmt::Display for SymbolKindDisplay<'a, 'b, T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.symbol {
            SymbolKind::NonTerminal(nt) => {
                let nt = self.grammar.get_non_terminal(*nt);
                write!(f, "{}", nt.non_terminal)
            }
            SymbolKind::Terminal(t) => write!(f, "{:?}", t),
        }
    }
}
