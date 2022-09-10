use crate::collections::Set;
use crate::grammar::{Grammar, NonTerminalIdx, ProductionIdx, Symbol, TerminalIdx};

use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Item<LA> {
    pub non_terminal_idx: NonTerminalIdx,
    pub production_idx: ProductionIdx,
    pub cursor: usize,
    pub lookahead: LA,
}

pub trait LookaheadDisplay {
    fn fmt<A>(&self, f: &mut fmt::Formatter<'_>, grammar: &Grammar<A>) -> fmt::Result;

    fn fmt_dot<A>(&self, f: &mut dyn fmt::Write, grammar: &Grammar<A>) -> fmt::Result;
}

impl LookaheadDisplay for () {
    fn fmt<A>(&self, _f: &mut fmt::Formatter<'_>, _grammar: &Grammar<A>) -> fmt::Result {
        Ok(())
    }

    fn fmt_dot<A>(&self, _f: &mut dyn fmt::Write, _grammar: &Grammar<A>) -> fmt::Result {
        Ok(())
    }
}

impl LookaheadDisplay for Option<TerminalIdx> {
    fn fmt<A>(&self, f: &mut fmt::Formatter<'_>, grammar: &Grammar<A>) -> fmt::Result {
        match self {
            Some(t) => write!(f, ", {}]", grammar.get_terminal(*t)),
            None => write!(f, ", $]"),
        }
    }

    fn fmt_dot<A>(&self, f: &mut dyn fmt::Write, grammar: &Grammar<A>) -> fmt::Result {
        hashset!(*self).fmt_dot(f, grammar)
    }
}

impl LookaheadDisplay for Set<Option<TerminalIdx>> {
    fn fmt<A>(&self, f: &mut fmt::Formatter, grammar: &Grammar<A>) -> fmt::Result {
        write!(f, ", {{")?;

        for (i, la) in self.iter().enumerate() {
            match la {
                Some(la) => write!(f, "{}", grammar.get_terminal(*la))?,
                None => write!(f, "$")?,
            }

            if i != self.len() - 1 {
                write!(f, ",")?;
            }
        }

        write!(f, "}}")
    }

    fn fmt_dot<A>(&self, f: &mut dyn fmt::Write, grammar: &Grammar<A>) -> fmt::Result {
        write!(f, " \\{{")?;

        for (t_idx, t) in self.iter().enumerate() {
            match t {
                Some(t) => write!(f, "{}", grammar.get_terminal(*t))?,
                None => write!(f, "$")?,
            }
            if t_idx != self.len() - 1 {
                write!(f, ",")?;
            }
        }

        write!(f, "\\}}")
    }
}

pub struct ItemDisplay<'a, 'b, A, LA: LookaheadDisplay> {
    pub item: &'a Item<LA>,
    pub grammar: &'b Grammar<A>,
}

impl<'a, 'b, A, LA: LookaheadDisplay> fmt::Display for ItemDisplay<'a, 'b, A, LA> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let non_terminal = self.grammar.get_non_terminal(self.item.non_terminal_idx);
        write!(f, "[{} -> ", non_terminal.non_terminal)?;

        let production = self
            .grammar
            .get_production(self.item.non_terminal_idx, self.item.production_idx);
        for (symbol_idx, symbol) in production.symbols().iter().enumerate() {
            if symbol_idx == self.item.cursor {
                write!(f, "|")?;
            }
            match &symbol.symbol {
                Symbol::NonTerminal(nt) => {
                    write!(f, "{}", self.grammar.get_non_terminal(*nt).non_terminal)?;
                }
                Symbol::Terminal(t) => {
                    write!(f, "{}", self.grammar.get_terminal(*t))?;
                }
            }
            if symbol_idx != production.symbols().len() - 1 {
                write!(f, " ")?;
            }
        }

        if self.item.cursor == production.symbols().len() {
            write!(f, "|")?;
        }

        self.item.lookahead.fmt(f, self.grammar)?;

        write!(f, "]")
    }
}

impl<'a, 'b, A, LA: LookaheadDisplay> ItemDisplay<'a, 'b, A, LA> {
    pub fn fmt_dot(&self, item_idx: usize, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(
            f,
            "{}: {} ➔ ",
            item_idx,
            self.grammar
                .get_non_terminal(self.item.non_terminal_idx)
                .non_terminal
        )?;

        let production = self
            .grammar
            .get_production(self.item.non_terminal_idx, self.item.production_idx);

        for (symbol_idx, symbol) in production.symbols().iter().enumerate() {
            if symbol_idx == self.item.cursor {
                write!(f, "• ")?;
            }
            match &symbol.symbol {
                Symbol::NonTerminal(nt) => {
                    write!(f, "{}", &self.grammar.get_non_terminal(*nt).non_terminal)?;
                }
                Symbol::Terminal(t) => {
                    write!(f, "{}", &self.grammar.get_terminal(*t))?;
                }
            }
            if symbol_idx != production.symbols().len() - 1 {
                write!(f, " ")?;
            }
        }

        if self.item.cursor == production.symbols().len() {
            write!(f, " •")?;
        }

        self.item.lookahead.fmt_dot(f, self.grammar)
    }
}
