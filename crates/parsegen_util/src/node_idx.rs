use std::fmt;
use std::num::NonZeroU32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeIdx(NonZeroU32);

impl NodeIdx {
    pub fn from_u32(u: u32) -> NodeIdx {
        NodeIdx(NonZeroU32::new(u + 1).unwrap())
    }

    pub fn as_u32(&self) -> u32 {
        self.0.get() - 1
    }

    pub fn from_usize(u: usize) -> NodeIdx {
        NodeIdx::from_u32(u32::try_from(u).unwrap())
    }

    pub fn as_usize(&self) -> usize {
        usize::try_from(self.as_u32()).unwrap()
    }
}

impl fmt::Display for NodeIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_u32().fmt(f)
    }
}
