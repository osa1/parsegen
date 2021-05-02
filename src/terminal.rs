//! Implements an arena for terminal representations

use std::iter::FromIterator;

use fxhash::FxHashMap;

#[derive(Debug)]
pub struct TerminalReprArena {
    /// Where we allocate terminal representations
    arena: Vec<syn::Ident>,

    /// Maps user-written terminal names (in `enum Token { ... }`) to their indices in the arena
    map: FxHashMap<String, TerminalReprIdx>,

    /// Name of the enum type generated for terminal kinds
    kind_type_name: syn::Ident,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct TerminalReprIdx(usize);

impl TerminalReprIdx {
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
    pub fn new_terminal(&mut self, user_name: String, variant_name: syn::Ident) -> TerminalReprIdx {
        assert!(!self.map.contains_key(&user_name));
        let idx = TerminalReprIdx(self.arena.len());
        self.map.insert(user_name, idx);
        self.arena.push(variant_name);
        idx
    }

    pub fn get_name_idx(&self, user_name: &str) -> TerminalReprIdx {
        self.map.get(user_name).unwrap().to_owned()
    }

    /// Get path to the enum variant for the terminal's kind
    pub fn get_enum_path(&self, idx: TerminalReprIdx) -> syn::Path {
        syn::Path {
            leading_colon: None,
            segments: syn::punctuated::Punctuated::from_iter(
                vec![
                    syn::PathSegment {
                        ident: self.kind_type_name.clone(),
                        arguments: syn::PathArguments::None,
                    },
                    syn::PathSegment {
                        ident: self.arena[idx.0].clone(),
                        arguments: syn::PathArguments::None,
                    },
                ]
                .into_iter(),
            ),
        }
    }

    pub fn len_terminals(&self) -> usize {
        self.arena.len()
    }
}
