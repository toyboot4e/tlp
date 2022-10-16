//! Patterns
//!
//! The simplest pattern is a binding pattern, where no destructuring performed.

use base::{jar::Word, tbl::id};
use salsa::DebugWithDb;

use crate::{ir::IrDb, syntax::ast};

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

impl PatData {
    pub fn from_ast(db: &dyn IrDb, ast_pat: ast::Pat) -> Option<Self> {
        match ast_pat {
            ast::Pat::PatPath(_) => todo!(),
            ast::Pat::PatIdent(ident) => {
                let name = Word::intern(db.base(), ident.ident_token().text());
                Some(PatData::Bind { name })
            }
        }
    }
}

impl DebugWithDb<dyn IrDb> for PatData {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &dyn IrDb,
        _all_fields: bool,
    ) -> std::fmt::Result {
        match self {
            PatData::Missing => write!(f, "<missing>"),
            PatData::Bind { name } => write!(f, "{:?}", name.as_str(db.base())),
        }
    }
}
