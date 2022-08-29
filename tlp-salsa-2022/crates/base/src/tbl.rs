//! Arena table and IDs

pub mod alloc_table;
pub mod intern_table;
pub mod origin_table;

pub mod prelude {
    pub use crate::tbl::InternAllocKey;
    pub use crate::tbl::InternKey;
    pub use crate::tbl::InternValue;
}

/// This module is used by the `tables` macro.
pub mod table_types {
    #![allow(non_camel_case_types)]
    pub type alloc<K, V> = crate::tbl::alloc_table::AllocTable<K, V>;
    pub type intern<K, V> = crate::tbl::intern_table::InternTable<K, V>;
}

/// Dependencies exported for macros
pub mod deps {
    pub use typed_index_collections;
}

/// Declares a struct usable as an id within a table.
#[macro_export]
macro_rules! id {
    ($v:vis struct $n:ident) => {
        #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        $v struct $n(salsa::Id);

        impl $n {
            pub fn zero() -> Self {
                Self::from(0_u32)
            }

            /// Returns an iterator from `0 .. self`.
            pub fn iter(self) -> impl Iterator<Item = Self> {
                (0_u32 .. u32::from(self))
                .map(move |i| Self::from(i))
            }

            /// Returns an iterator from `from .. to`.
            pub fn range(from: usize, to: usize) -> impl Iterator<Item = Self> {
                (from .. to).map(Self::from)
            }
        }

        impl std::fmt::Debug for $n {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}({})", stringify!($n), u32::from(*self))
            }
        }

        impl salsa::AsId for $n {
            fn as_id(self) -> salsa::Id {
                self.0
            }

            fn from_id(id: salsa::Id) -> Self {
                Self(id)
            }
        }

        impl std::ops::Add<usize> for $n {
            type Output = $n;

            fn add(self, other: usize) -> $n {
                $n::from(usize::from(self) + other)
            }
        }

        impl std::ops::Add<u32> for $n {
            type Output = $n;

            fn add(self, other: u32) -> $n {
                $n::from(u32::from(self) + other)
            }
        }

        impl From<usize> for $n {
            fn from(u: usize) -> $n {
                $n(salsa::Id::from(u))
            }
        }

        impl From<u32> for $n {
            fn from(u: u32) -> $n {
                $n(salsa::Id::from(u))
            }
        }

        impl From<$n> for usize {
            fn from(n: $n) -> usize {
                n.0.into()
            }
        }

        impl From<$n> for u32 {
            fn from(n: $n) -> u32 {
                n.0.into()
            }
        }
    }
}

pub use id;

/// Declares a struct containing a group of alloc/interning tables, along with methods for accessing them.
///
/// Example:
///
/// ```ignore
/// tables! {
///     pub struct Foo {
///         exprs: alloc Expr => ExprData,
///         tys: intern Ty => TyData,
///     }
/// }
/// ```
#[macro_export]
macro_rules! tables {
    ($(#[$attr:meta])* $vis:vis struct $n:ident {
        $(
            $f:ident: $tty:ident $k:ty => $v:ty,
        )*
    }) => {
        $(#[$attr])*
        $vis struct $n {
            $(
                $f: $crate::tbl::table_types::$tty<$k, $v>,
            )*
        }

        impl Default for $n {
            fn default() -> Self {
                Self {
                    $($f: <$crate::tbl::table_types::$tty<$k,$v>>::default(),)*
                }
            }
        }

        impl<K> std::ops::Index<K> for $n
        where
            K: $crate::tbl::InternKey<Table = Self>,
        {
            type Output = K::Value;

            fn index(&self, key: K) -> &Self::Output {
                K::data(key, self)
            }
        }

        impl<K> std::ops::IndexMut<K> for $n
        where
            K: $crate::tbl::InternAllocKey<Table = Self>,
        {
            fn index_mut(&mut self, key: K) -> &mut Self::Output {
                K::data_mut(key, self)
            }
        }

        impl $n {
            pub fn add<V>(&mut self, value: V) -> V::Key
            where
                V: $crate::tbl::InternValue<Table = Self>,
            {
                $crate::tbl::InternValue::add(value, self)
            }
        }

        $(
            $crate::tbl::tables!{
                @field_impl[$n] $f: $tty $k => $v
            }
        )*
    };

    (@field_impl[$n:ident] $f:ident: alloc $k:ty => $v:ty) => {
        $crate::tbl::tables!{
            @any_field_impl[$n] $f: $k => $v
        }

        impl $crate::tbl::InternAllocKey for $k {
            fn max_key(table: &Self::Table) -> $k {
                table.$f.next_key()
            }

            fn data_mut(self, table: &mut Self::Table) -> &mut Self::Value {
                table.$f.data_mut(self)
            }
        }
    };

    (@field_impl[$n:ident] $f:ident: intern $k:ty => $v:ty) => {
        $crate::tbl::tables!{
            @any_field_impl[$n] $f: $k => $v
        }
    };

    (@any_field_impl[$n:ident] $f:ident: $k:ty => $v:ty) => {
        impl $crate::tbl::InternValue for $v {
            type Table = $n;
            type Key = $k;

            fn add(self, table: &mut Self::Table) -> Self::Key {
                table.$f.add(self)
            }
        }

        impl $crate::tbl::InternKey for $k {
            type Table = $n;
            type Value = $v;

            fn data(self, table: &Self::Table) -> &Self::Value {
                table.$f.data(self)
            }
        }
    };
}

pub use tables;

pub trait InternValue {
    type Table;
    type Key: salsa::AsId;

    fn add(self, table: &mut Self::Table) -> Self::Key;
}

pub trait InternKey: salsa::AsId + 'static {
    type Table;
    type Value;

    /// Get the data for this key from the given table.
    fn data(self, table: &Self::Table) -> &Self::Value;
}

pub trait InternAllocKey: InternKey {
    /// Get the "max", which is the next key of this type which would be allocated.
    /// Note that this is not (yet) a valid key.
    fn max_key(table: &Self::Table) -> Self;

    /// Get mut ref to data for this key from the given table.
    fn data_mut(self, table: &mut Self::Table) -> &mut Self::Value;
}
