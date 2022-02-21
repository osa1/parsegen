//! A lowered representation of grammars

use crate::ast;

/// Grammar type parameterized over terminals and user actions.
#[derive(Debug, Clone)]
pub struct Grammar {
    // Non-terminals, indexed by `NonTerminalIdx`
    pub non_terminals: Vec<NonTerminal>,

    // Maps terminals to their user-written names (in `enum Token { ... }`). The strings are only
    // used for debugging purposes, but we use the length of this vector to generate
    // `TerminalIdx`s.
    pub terminals: Vec<String>,
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
pub struct NonTerminal {
    pub non_terminal: String,
    // Indexed by `ProductionIdx`
    pub productions: Vec<Production>,
    pub return_ty: syn::Type,
    pub public: bool,
}

#[derive(Debug, Clone)]
pub struct Production {
    pub symbols: Vec<Symbol>,
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

impl Grammar {
    pub fn new() -> Self {
        Grammar {
            non_terminals: vec![],
            terminals: vec![],
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

    pub fn new_terminal(&mut self, str: String) -> TerminalIdx {
        let idx = TerminalIdx(u32::try_from(self.terminals.len()).unwrap());
        self.terminals.push(str);
        idx
    }

    pub fn get_non_terminal(&self, idx: NonTerminalIdx) -> &NonTerminal {
        &self.non_terminals[idx.0 as usize]
    }

    pub fn get_terminal(&self, idx: TerminalIdx) -> &str {
        &self.terminals[idx.0 as usize]
    }

    pub fn n_non_terminals(&self) -> usize {
        self.non_terminals.len()
    }

    pub fn n_terminals(&self) -> usize {
        self.terminals.len()
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
    ) -> ProductionIdx {
        let non_terminal = &mut self.non_terminals[non_terminal.0 as usize];
        let prod_idx = non_terminal.productions.len();
        non_terminal.productions.push(Production { symbols });
        ProductionIdx(prod_idx as u32)
    }

    pub fn get_production(&self, nt_idx: NonTerminalIdx, prod_idx: ProductionIdx) -> &Production {
        &self.non_terminals[nt_idx.0 as usize].productions[prod_idx.0 as usize]
    }
}

impl NonTerminal {
    pub fn productions(&self) -> &[Production] {
        &self.productions
    }

    pub fn production_indices(&self) -> impl Iterator<Item = (ProductionIdx, &Production)> {
        self.productions
            .iter()
            .enumerate()
            .map(|(i, p)| (ProductionIdx(i as u32), p))
    }

    pub fn get_production(&self, production_idx: ProductionIdx) -> &Production {
        &self.productions[production_idx.0 as usize]
    }
}

impl Production {
    pub fn symbols(&self) -> &[Symbol] {
        &self.symbols
    }
}

struct ProductionIndicesIter<'grammar> {
    grammar: &'grammar Grammar,
    non_terminal_idx: NonTerminalIdx,
    production_idx: ProductionIdx,
}

struct NonTerminalIndicesIter<'grammar> {
    grammar: &'grammar Grammar,
    non_terminal_idx: NonTerminalIdx,
}

struct NonTerminalProductionIndicesIter<'grammar> {
    grammar: &'grammar Grammar,
    non_terminal_idx: NonTerminalIdx,
    production_idx: ProductionIdx,
}

impl<'grammar> Iterator for ProductionIndicesIter<'grammar> {
    type Item = (NonTerminalIdx, ProductionIdx, &'grammar Production);

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

impl<'grammar> Iterator for NonTerminalIndicesIter<'grammar> {
    type Item = (NonTerminalIdx, &'grammar NonTerminal);

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

impl<'grammar> Iterator for NonTerminalProductionIndicesIter<'grammar> {
    type Item = (ProductionIdx, &'grammar Production);

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

impl Grammar {
    pub fn non_terminals(&self) -> &[NonTerminal] {
        &self.non_terminals
    }

    pub fn non_terminal_indices(&self) -> impl Iterator<Item = (NonTerminalIdx, &NonTerminal)> {
        NonTerminalIndicesIter {
            grammar: self,
            non_terminal_idx: NonTerminalIdx(0),
        }
    }

    pub fn non_terminal_production_indices(
        &self,
        non_terminal: NonTerminalIdx,
    ) -> impl Iterator<Item = (ProductionIdx, &Production)> {
        NonTerminalProductionIndicesIter {
            grammar: self,
            non_terminal_idx: non_terminal,
            production_idx: ProductionIdx(0),
        }
    }

    pub fn terminal_indices(&self) -> impl Iterator<Item = TerminalIdx> {
        (0..self.terminals.len()).map(|idx| TerminalIdx(u32::try_from(idx).unwrap()))
    }
}

pub struct SymbolKindDisplay<'a, 'b> {
    symbol: &'a SymbolKind,
    grammar: &'b Grammar,
}

impl<'a, 'b> SymbolKindDisplay<'a, 'b> {
    pub fn new(symbol: &'a SymbolKind, grammar: &'b Grammar) -> Self {
        Self { symbol, grammar }
    }
}

pub struct ProductionDisplay<'a, 'b> {
    production: &'a Production,
    grammar: &'b Grammar,
}

impl<'a, 'b> ProductionDisplay<'a, 'b> {
    pub fn new(production: &'a Production, grammar: &'b Grammar) -> Self {
        Self {
            production,
            grammar,
        }
    }
}

use std::fmt;

impl fmt::Display for Grammar {
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

impl<'a, 'b> fmt::Display for ProductionDisplay<'a, 'b> {
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

impl<'a, 'b> fmt::Display for SymbolKindDisplay<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.symbol {
            SymbolKind::NonTerminal(nt) => {
                let nt = self.grammar.get_non_terminal(*nt);
                write!(f, "{}", nt.non_terminal)
            }
            SymbolKind::Terminal(t) => {
                let t = &self.grammar.terminals[t.0 as usize];
                write!(f, "{:?}", t)
            }
        }
    }
}
