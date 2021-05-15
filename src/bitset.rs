use std::marker::PhantomData;

pub trait ToBitIdx {
    fn to_bit_idx(&self) -> usize;
}

pub trait FromBitIdx {
    fn from_bit_idx(idx: usize) -> Self;
}

#[derive(Debug)]
pub struct BitSet<T: ToBitIdx> {
    chunks: Vec<u64>,
    len: usize,
    phantom: PhantomData<T>,
}

impl<T: ToBitIdx> Clone for BitSet<T> {
    fn clone(&self) -> Self {
        BitSet {
            chunks: self.chunks.clone(),
            len: self.len,
            phantom: PhantomData,
        }
    }
}

#[inline(always)]
fn chunk_idx(n: usize) -> usize {
    n / 64
}

#[inline(always)]
fn bit_idx(n: usize) -> usize {
    n % 64
}

impl<T: ToBitIdx> BitSet<T> {
    pub fn new(n_elems: usize) -> BitSet<T> {
        let words = (n_elems + 63) / 64;
        BitSet {
            chunks: vec![0; words],
            len: n_elems,
            phantom: PhantomData,
        }
    }

    /// Returns whether the bit was updated
    #[inline]
    pub fn set(&mut self, elem: &T) -> bool {
        let elem_idx = elem.to_bit_idx();
        let chunk = &mut self.chunks[chunk_idx(elem_idx)];
        let mask = 1u64 << bit_idx(elem_idx);
        let updated = *chunk & mask == 0;
        *chunk = *chunk | mask;
        updated
    }

    #[inline]
    pub fn get(&self, elem: &T) -> bool {
        let elem_idx = elem.to_bit_idx();
        self.get_bit(elem_idx)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline(always)]
    fn get_bit(&self, bit: usize) -> bool {
        let chunk = self.chunks[chunk_idx(bit)];
        (chunk >> bit_idx(bit)) & 0b1 == 0b1
    }
}

struct BitIter<'a, T: ToBitIdx> {
    set: &'a BitSet<T>,
    idx: usize,
}

// TODO: Test this
impl<'a, T: ToBitIdx + FromBitIdx> Iterator for BitIter<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        // TODO: Maybe store first and last bits in `BitSet` to make this more efficient?
        for i in self.idx..self.set.len() {
            if self.set.get_bit(i) {
                self.idx = i + 1;
                let t = T::from_bit_idx(i);
                return Some(t);
            }
        }
        self.idx = self.set.len();
        None
    }
}

impl<T: ToBitIdx + FromBitIdx> BitSet<T> {
    pub fn elems<'a>(&'a self) -> impl Iterator<Item = T> + 'a {
        BitIter { set: self, idx: 0 }
    }
}

#[test]
fn bit_set_1() {
    impl ToBitIdx for usize {
        fn to_bit_idx(&self) -> usize {
            *self
        }
    }

    const N_ELEMS: usize = 100;

    let mut bitset: BitSet<usize> = BitSet::new(N_ELEMS);
    for i in 0..N_ELEMS {
        assert_eq!(bitset.get(&i), false);
    }
    for i in 0..N_ELEMS {
        bitset.set(&i);
        for j in 0..=i {
            assert_eq!(bitset.get(&j), true);
        }
        for j in i + 1..N_ELEMS {
            assert_eq!(bitset.get(&j), false);
        }
    }
}
