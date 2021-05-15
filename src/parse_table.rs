use crate::first::FirstTable;
use crate::follow::FollowTable;
use crate::grammar::{Grammar, NonTerminalIdx, ProductionIdx, SymbolKind};
use crate::terminal::{TerminalReprArena, TerminalReprIdx};

use fxhash::FxHashMap;

/// Predictive parse table
#[derive(Debug, Default)]
pub struct ParseTable {
    // Informally: if I'm parsing the non-terminal `NT` and next token is `c`, then `(NT, c)` in
    // the table tells me which production to expect. If there isn't an entry for `(NT, c)` then we
    // have an error in the input. During building, if we try to add multiple productions to a
    // non-terminal, token pair, that means we have an ambiguous, or left-recursive grammar.
    pub table: FxHashMap<(NonTerminalIdx, TerminalReprIdx), ProductionIdx>,

    // Same as `table`, but for `$`
    pub end: FxHashMap<NonTerminalIdx, ProductionIdx>,
}

impl ParseTable {
    fn add<A>(
        &mut self,
        grammar: &Grammar<TerminalReprIdx, A>,
        terminals: &TerminalReprArena,
        non_terminal_idx: NonTerminalIdx,
        token: TerminalReprIdx,
        production_idx: ProductionIdx,
    ) {
        let old = self.table.insert((non_terminal_idx, token), production_idx);

        if let Some(old) = old {
            let non_terminal = grammar.get_non_terminal(non_terminal_idx);
            let token_user_name = terminals.get_terminal_user_name(token);
            panic!(
                "Multiple productions defined for ({}, {:?}): {{p{}, p{}}}",
                non_terminal.name(),
                token_user_name,
                old.as_usize(),
                production_idx.as_usize(),
            );
        }
    }

    /// Same as `add`, except the token is EOF (`$`)
    fn add_end(&mut self, non_terminal_idx: NonTerminalIdx, production_idx: ProductionIdx) {
        let old = self.end.insert(non_terminal_idx, production_idx);
        assert_eq!(old, None);
    }

    fn get(
        &self,
        non_terminal_idx: NonTerminalIdx,
        token: TerminalReprIdx,
    ) -> Option<ProductionIdx> {
        self.table.get(&(non_terminal_idx, token)).copied()
    }

    pub fn get_end(&self, non_terminal_idx: NonTerminalIdx) -> Option<ProductionIdx> {
        self.end.get(&non_terminal_idx).copied()
    }
}

pub fn generate_parse_table<A>(
    grammar: &Grammar<TerminalReprIdx, A>,
    terminals: &TerminalReprArena,
    first_table: &FirstTable<TerminalReprIdx>,
    follow_table: &FollowTable<TerminalReprIdx>,
) -> ParseTable {
    let mut table: ParseTable = Default::default();

    for (non_terminal_idx, production_idx, production) in grammar.production_indices() {
        let mut all_empty = true;
        for symbol in production.symbols() {
            match &symbol.kind {
                SymbolKind::NonTerminal(nt_idx) => {
                    let nt_firsts = first_table.get_first(*nt_idx);
                    for terminal in nt_firsts.terminals() {
                        table.add(
                            grammar,
                            terminals,
                            non_terminal_idx,
                            terminal.clone(),
                            production_idx,
                        );
                    }
                    if !nt_firsts.has_empty() {
                        all_empty = false;
                        break;
                    }
                }
                SymbolKind::Terminal(terminal) => {
                    table.add(
                        grammar,
                        terminals,
                        non_terminal_idx,
                        terminal.clone(),
                        production_idx,
                    );
                    all_empty = false;
                    break;
                }
            }
        }
        if all_empty {
            // When a non-terminal never appears in a RHS it won't have an entry in the follow
            // table. This often happens during development of a parser.
            let nt_follows = follow_table.get_follow(non_terminal_idx);
            for terminal in nt_follows.terminals() {
                table.add(
                    grammar,
                    terminals,
                    non_terminal_idx,
                    terminal.clone(),
                    production_idx,
                );
            }
            if nt_follows.has_end() {
                table.add_end(non_terminal_idx, production_idx);
            }
        }
    }

    table
}
