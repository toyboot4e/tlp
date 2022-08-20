//! IDs of lowered data types

use std::{fmt, marker::PhantomData};

use derivative::Derivative;
use la_arena::Idx;

use crate::{
    hir_def::{
        db::{self, vfs::VfsFileId},
        item_list::item,
        FileData,
    },
    syntax::{
        ast::{self, AstNode},
        ptr::SyntaxNodePtr,
    },
};

/// Interned string for variable / item name
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    // TODO: consider interning string with salsa?
    data: smol_str::SmolStr,
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.data, f)
    }
}

impl Name {
    pub fn from_str(s: &str) -> Self {
        Self { data: s.into() }
    }

    pub fn as_str(&self) -> &str {
        self.data.as_str()
    }

    pub const fn missing() -> Name {
        Name {
            data: smol_str::SmolStr::new_inline("[missing name]"),
        }
    }
}

/// Interned data ID
#[derive(Derivative)]
#[derivative(Debug, Hash, PartialEq, Eq)]
pub struct Id<T> {
    raw: salsa::InternId,
    _ty: PhantomData<T>,
}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        Self {
            raw: self.raw,
            _ty: PhantomData,
        }
    }
}

impl<T> Copy for Id<T> {}

impl<T> salsa::InternKey for Id<T> {
    fn from_intern_id(id: salsa::InternId) -> Self {
        Self {
            raw: id,
            _ty: PhantomData,
        }
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.raw
    }
}

// --------------------------------------------------------------------------------
// AST (common)
// --------------------------------------------------------------------------------

/// Stable AST node index by [`AstIdMap`]
///
/// [`AstIdMap`]: crate::hir_def::lower::AstIdMap
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstIdx<N: AstNode> {
    /// NOTE: We can't use `AstPtr<N>` since it's stored in heterogeneous `Arena` in `AstIdMap`
    pub raw: Idx<SyntaxNodePtr>,
    _ty: PhantomData<fn() -> N>,
}

impl<N: AstNode> AstIdx<N> {
    pub fn new(idx: Idx<SyntaxNodePtr>) -> Self {
        Self {
            raw: idx,
            _ty: PhantomData,
        }
    }
}

// --------------------------------------------------------------------------------
// AST item (`AstIdMap` index)
// --------------------------------------------------------------------------------

/// Interned [`AstItemLoc<T>`]
pub type AstItemLocId<T> = Id<AstItemLoc<T>>;

// impl AstItemIdx<ast::DefProc> {
//     pub fn lookup_loc(&self, db: &dyn db::Def) -> AstItemLoc<ast::DefProc> {
//         db.lookup_intern_ast_proc_loc(self.clone())
//     }
// }

/// [`VfsFileId`] + [`AstIdx`]
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct AstItemLoc<N: AstNode> {
    pub file: VfsFileId,
    pub idx: AstIdx<N>,
}

// --------------------------------------------------------------------------------
// AST expression ID (`BodySourceMap` index)
// --------------------------------------------------------------------------------

/// Interned [`AstExprLoc<T>`]
pub type AstExprLocId<T> = Id<AstExprLoc<T>>;

impl AstExprLocId<ast::Block> {
    pub fn lookup_loc(&self, db: &dyn db::Def) -> AstExprLoc<ast::Block> {
        db.lookup_intern_ast_block_loc(*self)
    }
}

/// [`VfsFileId`] + [`AstIdx`]
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct AstExprLoc<N: AstNode> {
    pub file: VfsFileId,
    pub idx: AstIdx<N>,
}

// --------------------------------------------------------------------------------
// HIR item ID (`ItemList` index)
// --------------------------------------------------------------------------------

/// Interned [`HirItemLoc<T>`]
pub type HirItemLocId<T> = Id<HirItemLoc<T>>;

impl HirItemLocId<item::DefProc> {
    pub fn lookup_loc(&self, db: &dyn db::Def) -> HirItemLoc<item::DefProc> {
        db.lookup_intern_item_proc_loc(*self)
    }
}

/// [`VfsFileId`] + [`FileDataIdx`] + [`Idx`] ([`ItemList`] arena index)
///
/// [`ItemList`]: crate::hir_def::item_list::ItemList
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HirItemLoc<T> {
    /// Original source file
    pub file: VfsFileId,
    /// Index to [`CrateData`]
    pub file_data: FileDataIdx,
    /// Index to [`ItemList`]
    ///
    /// [`ItemList`]: crate::hir_def::item_list::ItemList
    pub idx: Idx<T>,
}

// --------------------------------------------------------------------------------
// HIR crate/module ID
// --------------------------------------------------------------------------------

/// Index of [`FileData`] in [`CrateData`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileDataIdx {
    // krate: CrateId,
    // block: BlockId,
    pub idx: Idx<FileData>,
}

// impl FileDataId {
//     pub fn lookup(&self, db: &Def) -> FileData
// }
