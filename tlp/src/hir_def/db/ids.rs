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
// AST
// --------------------------------------------------------------------------------

pub type AstId<T> = Id<AstLoc<T>>;

impl AstId<ast::Block> {
    pub fn lookup_loc(&self, db: &dyn db::Def) -> AstLoc<ast::Block> {
        db.lookup_intern_ast_block_loc(*self)
    }
}

/// AST syntax location (file ID + item tree arena index)
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct AstLoc<N: AstNode> {
    pub file: VfsFileId,
    pub idx: AstIdx<N>,
}

/// New type of `Idx<SyntaxNodePtr>` with `N: AstNode` typed
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
// Item
// --------------------------------------------------------------------------------

pub type ItemId<T> = Id<ItemLoc<T>>;

impl ItemId<item::DefProc> {
    pub fn lookup_loc(&self, db: &dyn db::Def) -> ItemLoc<item::DefProc> {
        db.lookup_intern_item_proc_loc(*self)
    }
}

/// HIR item location (file ID + item tree arena index)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemLoc<T> {
    /// Index to (TODO: what?)
    pub file: VfsFileId,
    /// Index to [`ItemList`](crate::hir_def::scope::ItemList)
    pub idx: Idx<T>,
}
