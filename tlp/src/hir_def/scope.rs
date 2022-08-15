//! Item / expression scopes

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;

use std::{ops, sync::Arc};

use crate::hir_def::{
    db::{
        self,
        ids::{Id, Loc},
        vfs::VfsFileId,
    },
    item::{self, Name},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemList {
    pub(crate) file: VfsFileId,
    pub(crate) procs: Arena<item::DefProc>,
    // pub(crate) imports: Vec<Import>,
}

impl ops::Index<Idx<item::DefProc>> for ItemList {
    type Output = item::DefProc;
    fn index(&self, ix: Idx<item::DefProc>) -> &Self::Output {
        &self.procs[ix]
    }
}

impl ItemList {
    pub(crate) fn new(file: VfsFileId) -> Self {
        Self {
            file,
            procs: Default::default(),
        }
    }

    pub fn procs(&self) -> &Arena<item::DefProc> {
        &self.procs
    }
}

/// Resolves [`Name`] with a stack of [`Scope`] s
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprScopeStack {
    scopes: Vec<Scope>,
}

/// [`ItemScope`] | [`ExprScope`]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Scope {
    Item(ItemScope),
    Expr(ExprScope),
}

/// Items visible in a scope (declarations and imports)
///
/// Built upon `ItemTree`.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ItemScope {
    // declarations
    procs: FxHashMap<Name, Id<Loc<item::DefProc>>>,
}

impl ItemScope {
    pub(crate) fn declare_proc(&mut self, name: Name, proc: Id<Loc<item::DefProc>>) {
        // TOOD: consider upcasting or not
        // let id = DefId { loc_id: proc };
        // self.procs.insert(name, AnyDefId::from(id));
        self.procs.insert(name, proc);
    }

    pub fn lookup_proc(&self, name: &Name) -> Option<Id<Loc<item::DefProc>>> {
        self.procs.get(name).cloned()
    }
}

/// Resolves [`Name`]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprScope {
    //
}

pub(crate) fn proc_expr_scope_query(
    _db: &dyn db::Def,
    _proc_id: Id<Loc<item::DefProc>>,
) -> Arc<ExprScopeStack> {
    todo!()
}
