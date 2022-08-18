//! IDs of lowered data types

use std::marker::PhantomData;

use derivative::Derivative;
use la_arena::Idx;

use crate::{
    hir_def::{
        db::{self, vfs::VfsFileId},
        item,
    },
    syntax::{
        ast::{self, AstNode},
        ptr::SyntaxNodePtr,
    },
};

/// Interned ID to a location
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

/// Item / expression index for AST node's pointer
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstIdx<N: AstNode> {
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
// AST item (`ItemSourceMap` index)
// --------------------------------------------------------------------------------

/// Interned [`AstItemLoc<T>`]
pub type AstItemIdx<T> = Id<AstItemLoc<T>>;

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct AstItemLoc<N: AstNode> {
    pub file: VfsFileId,
    pub idx: AstIdx<N>,
}

// --------------------------------------------------------------------------------
// AST expression ID (`BodySourceMap` index)
// --------------------------------------------------------------------------------

/// Interned [`AstExprLoc<T>`]
pub type AstExprIdx<T> = Id<AstExprLoc<T>>;

impl AstExprIdx<ast::Block> {
    pub fn lookup_loc(&self, db: &dyn db::Def) -> AstExprLoc<ast::Block> {
        db.lookup_intern_ast_block_loc(*self)
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct AstExprLoc<N: AstNode> {
    pub file: VfsFileId,
    pub idx: AstIdx<N>,
}

// --------------------------------------------------------------------------------
// HIR item ID (`ItemList` index)
// --------------------------------------------------------------------------------

// Item AST are lowered into `ItemList` and given (rather) stable Idx.

/// Interned [`ItemLoc<T>`]
pub type HirItemId<T> = Id<HirItemLoc<T>>;

impl HirItemId<item::DefProc> {
    pub fn lookup_loc(&self, db: &dyn db::Def) -> HirItemLoc<item::DefProc> {
        db.lookup_intern_item_proc_loc(*self)
    }
}

/// HIR item location (file ID + item tree arena index)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HirItemLoc<T> {
    /// Index to (TODO: what?)
    pub file: VfsFileId,
    /// Index to [`ItemList`](crate::hir_def::scope::ItemList)
    pub idx: Idx<T>,
}
