use base::{jar::Word, tbl::id};

id! {
    /// ID of [`PatData`] that implements [`InternAllocKey`] and [`InternKey`]
    ///
    /// # Trait methods
    ///
    /// - [`InternKey::data`] -> [`&ExprData`]
    /// - [`InternAllocKey::data_mut`] -> [`&mut ExprData`]
    /// - [`InternAllocKey:::max_key`] -> [`Self`]
    ///
    /// [`InternAllocKey`]: base::tbl::InternAllocKey
    /// [`InternKey`]: base::tbl::InternKey
    ///
    /// [`InternValue::data`]: base::tbl::InternKey::data
    /// [`InternAllocKey:::max_key`]: base::tbl::InternAllocKey::max_key
    /// [`InternAllocKey::data_mut`]: base::tbl::InternAllocKey::data_mut
    pub struct Pat;
}

/// HIR pattern type
///
/// It's more semantic compared to AST patterns.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub enum PatData {
    /// Given invalid syntax, pattern can miss
    Missing,
    /// Introduces a new identifier
    Bind { name: Word },
}
