#![allow(unused)]

macro_rules! hashmap {
    (@single $($x:tt)*) => (());
    (@count $($rest:expr),*) => (<[()]>::len(&[$(hashmap!(@single $rest)),*]));

    ($($key:expr => $value:expr,)+) => { hashmap!($($key => $value),+) };
    ($($key:expr => $value:expr),*) => {
        {
            let _cap = hashmap!(@count $($key),*);
            let mut _map = ::std::collections::HashMap::with_capacity_and_hasher(_cap, ::fxhash::FxBuildHasher::default());
            $(
                let _ = _map.insert($key, $value);
            )*
            _map
        }
    };
}

macro_rules! hashset {
    (@single $($x:tt)*) => (());
    (@count $($rest:expr),*) => (<[()]>::len(&[$(hashset!(@single $rest)),*]));

    ($($key:expr,)+) => { hashset!($($key),+) };
    ($($key:expr),*) => {
        {
            let _cap = hashset!(@count $($key),*);
            let mut _set = ::std::collections::HashSet::with_capacity_and_hasher(_cap, ::fxhash::FxBuildHasher::default());
            $(
                let _ = _set.insert($key);
            )*
            _set
        }
    };
}

macro_rules! btreemap {
    // trailing comma case
    ($($key:expr => $value:expr,)+) => (btreemap!($($key => $value),+));

    ( $($key:expr => $value:expr),* ) => {
        {
            let mut _map = ::std::collections::BTreeMap::new();
            $(
                let _ = _map.insert($key, $value);
            )*
            _map
        }
    };
}

macro_rules! btreeset {
    ($($key:expr,)+) => (btreeset!($($key),+));

    ( $($key:expr),* ) => {
        {
            let mut _set = ::std::collections::BTreeSet::new();
            $(
                _set.insert($key);
            )*
            _set
        }
    };
}

#[test]
fn test_hashmap() {
    use crate::collections::Map;

    let names = hashmap! {
        1 => "one",
        2 => "two",
    };
    assert_eq!(names.len(), 2);
    assert_eq!(names[&1], "one");
    assert_eq!(names[&2], "two");
    assert_eq!(names.get(&3), None);

    let empty: Map<i32, i32> = hashmap! {};
    assert_eq!(empty.len(), 0);

    let _nested_compiles = hashmap! {
        1 => hashmap!{0 => 1 + 2,},
        2 => hashmap!{1 => 1,},
    };
}

#[test]
fn test_btreemap() {
    use std::collections::BTreeMap;

    let names = btreemap! {
        1 => "one",
        2 => "two",
    };
    assert_eq!(names.len(), 2);
    assert_eq!(names[&1], "one");
    assert_eq!(names[&2], "two");
    assert_eq!(names.get(&3), None);

    let empty: BTreeMap<i32, i32> = btreemap! {};
    assert_eq!(empty.len(), 0);

    let _nested_compiles = btreemap! {
        1 => btreemap!{0 => 1 + 2,},
        2 => btreemap!{1 => 1,},
    };
}
