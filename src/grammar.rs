//! A lowered representation of grammars

use crate::ast;

use std::convert::TryFrom;

/// Grammar type parameterized over terminals and user actions.
#[derive(Debug, Clone)]
pub struct Grammar<A> {
    // Non-terminals, indexed by `NonTerminalIdx`
    pub non_terminals: Vec<NonTerminal<A>>,

    // Number of terminals in the grammar
    pub n_terminals: u32,
}

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
pub struct TerminalIdx(u32);

impl TerminalIdx {
    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord, Hash)]
pub struct ProductionIdx(pub u32);

impl ProductionIdx {
    pub fn as_usize(self) -> usize {
        self.0 as usize
    }

    #[cfg(test)]
    pub fn from_usize(i: usize) -> Self {
        Self(u32::try_from(i).unwrap())
    }
}

#[derive(Debug, Clone)]
pub struct NonTerminal<A> {
    pub non_terminal: String,
    // Indexed by `ProductionIdx`
    pub productions: Vec<Production<A>>,
    pub return_ty: syn::Type,
    pub public: bool,
}

#[derive(Clone)]
pub struct Production<A> {
    pub symbols: Vec<Symbol>,
    pub action: A,
}

impl<A> std::fmt::Debug for Production<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Production")
            .field("symbols", &self.symbols)
            .field("action", &"...")
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub binder: Option<ast::Name>,
    pub kind: SymbolKind,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum SymbolKind {
    NonTerminal(NonTerminalIdx),
    Terminal(TerminalIdx),
}

impl<A> Grammar<A> {
    pub fn new() -> Self {
        Grammar {
            non_terminals: vec![],
            n_terminals: 0,
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

    pub fn next_terminal_idx(&mut self) -> TerminalIdx {
        let idx = TerminalIdx(self.n_terminals);
        self.n_terminals += 1;
        idx
    }

    pub fn get_non_terminal(&self, idx: NonTerminalIdx) -> &NonTerminal<A> {
        &self.non_terminals[idx.0 as usize]
    }

    /*
    #[cfg(test)]
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
    */

    pub fn add_production(
        &mut self,
        non_terminal: NonTerminalIdx,
        symbols: Vec<Symbol>,
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
    ) -> &Production<A> {
        &self.non_terminals[nt_idx.0 as usize].productions[prod_idx.0 as usize]
    }
}

impl<A> NonTerminal<A> {
    pub fn productions(&self) -> &[Production<A>] {
        &self.productions
    }

    pub fn production_indices(&self) -> impl Iterator<Item = (ProductionIdx, &Production<A>)> {
        self.productions
            .iter()
            .enumerate()
            .map(|(i, p)| (ProductionIdx(i as u32), p))
    }

    pub fn get_production(&self, production_idx: ProductionIdx) -> &Production<A> {
        &self.productions[production_idx.0 as usize]
    }
}

impl<A> Production<A> {
    pub fn symbols(&self) -> &[Symbol] {
        &self.symbols
    }
}

struct ProductionIndicesIter<'grammar, A> {
    grammar: &'grammar Grammar<A>,
    non_terminal_idx: NonTerminalIdx,
    production_idx: ProductionIdx,
}

struct NonTerminalIndicesIter<'grammar, A> {
    grammar: &'grammar Grammar<A>,
    non_terminal_idx: NonTerminalIdx,
}

struct NonTerminalProductionIndicesIter<'grammar, A> {
    grammar: &'grammar Grammar<A>,
    non_terminal_idx: NonTerminalIdx,
    production_idx: ProductionIdx,
}

impl<'grammar, A> Iterator for ProductionIndicesIter<'grammar, A> {
    type Item = (NonTerminalIdx, ProductionIdx, &'grammar Production<A>);

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

impl<'grammar, A> Iterator for NonTerminalIndicesIter<'grammar, A> {
    type Item = (NonTerminalIdx, &'grammar NonTerminal<A>);

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

impl<'grammar, A> Iterator for NonTerminalProductionIndicesIter<'grammar, A> {
    type Item = (ProductionIdx, &'grammar Production<A>);

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

impl<A> Grammar<A> {
    pub fn non_terminals(&self) -> &[NonTerminal<A>] {
        &self.non_terminals
    }

    pub fn non_terminal_indices(&self) -> impl Iterator<Item = (NonTerminalIdx, &NonTerminal<A>)> {
        NonTerminalIndicesIter {
            grammar: self,
            non_terminal_idx: NonTerminalIdx(0),
        }
    }

    pub fn non_terminal_production_indices(
        &self,
        non_terminal: NonTerminalIdx,
    ) -> impl Iterator<Item = (ProductionIdx, &Production<A>)> {
        NonTerminalProductionIndicesIter {
            grammar: self,
            non_terminal_idx: non_terminal,
            production_idx: ProductionIdx(0),
        }
    }

    pub fn terminal_indices(&self) -> impl Iterator<Item = TerminalIdx> {
        (0..self.n_terminals).map(TerminalIdx)
    }
}

pub struct SymbolKindDisplay<'a, 'b, A> {
    symbol: &'a SymbolKind,
    grammar: &'b Grammar<A>,
}

impl<'a, 'b, A> SymbolKindDisplay<'a, 'b, A> {
    pub fn new(symbol: &'a SymbolKind, grammar: &'b Grammar<A>) -> Self {
        Self { symbol, grammar }
    }
}

pub struct ProductionDisplay<'a, 'b, A> {
    production: &'a Production<A>,
    grammar: &'b Grammar<A>,
}

impl<'a, 'b, A> ProductionDisplay<'a, 'b, A> {
    pub fn new(production: &'a Production<A>, grammar: &'b Grammar<A>) -> Self {
        Self {
            production,
            grammar,
        }
    }
}

use std::fmt;

impl<A> fmt::Display for Grammar<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (nt_idx, nt) in self.non_terminals.iter().enumerate() {
            writeln!(
                f,
                "{}: {} {} = {{",
                nt_idx,
                if nt.public { "pub" } else { "" },
                nt.non_terminal
            )?;

            for (p_idx, p) in nt.production_indices() {
                writeln!(
                    f,
                    "  {}: {}",
                    p_idx.as_usize(),
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

impl<'a, 'b, A> fmt::Display for ProductionDisplay<'a, 'b, A> {
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

impl<'a, 'b, A> fmt::Display for SymbolKindDisplay<'a, 'b, A> {
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
