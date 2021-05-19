//! Implements an arena for terminal representations

use std::fmt;

use fxhash::FxHashMap;

#[derive(Debug)]
pub struct TerminalReprArena {
    /// Where we allocate terminal representations
    arena: Vec<TerminalRepr>,

    /// Maps user-written terminal names (in `enum Token { ... }`) to their indices in the arena
    map: FxHashMap<String, TerminalIdx>,

    /// Name of the enum type generated for terminal kinds
    kind_type_name: syn::Ident,
}

#[derive(Debug)]
struct TerminalRepr {
    /// Variant name of the terminal in the token kind enum
    ident: syn::Ident,
    /// User-written name of the terminal
    name: String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct TerminalIdx(usize);

impl fmt::Debug for TerminalIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#t{}", self.0)
    }
}

impl TerminalIdx {
    #[cfg(test)]
    pub const fn from_usize(i: usize) -> TerminalIdx {
        TerminalIdx(i)
    }

    pub fn as_usize(self) -> usize {
        self.0
    }
}

impl TerminalReprArena {
    pub fn new(kind_type_name: syn::Ident) -> Self {
        TerminalReprArena {
            arena: vec![],
            map: Default::default(),
            kind_type_name,
        }
    }

    /// - `user_name`: User-written name of the terminal in the `enum Token { ... }`
    /// - `variant_name`: Name of the variant for this terminal in the token kind enum
    pub fn new_terminal(&mut self, user_name: String, variant_name: syn::Ident) -> TerminalIdx {
        assert!(!self.map.contains_key(&user_name));
        let idx = TerminalIdx(self.arena.len());
        self.map.insert(user_name.clone(), idx);
        self.arena.push(TerminalRepr {
            ident: variant_name,
            name: user_name,
        });
        idx
    }

    pub fn get_name_idx(&self, user_name: &str) -> TerminalIdx {
        self.map.get(user_name).unwrap().to_owned()
    }

    pub fn n_terminals(&self) -> usize {
        self.arena.len()
    }

    pub fn terminal_indices(&self) -> impl Iterator<Item = TerminalIdx> {
        TerminalIndicesIter {
            current: 0,
            max: self.n_terminals(),
        }
    }
}

struct TerminalIndicesIter {
    current: usize,
    max: usize,
}

impl Iterator for TerminalIndicesIter {
    type Item = TerminalIdx;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.current;
        if current == self.max {
            None
        } else {
            self.current += 1;
            Some(TerminalIdx(current))
        }
    }
}
