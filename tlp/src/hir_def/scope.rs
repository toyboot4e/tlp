//! Item / expression scopes

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;

use std::{ops, sync::Arc};

use crate::hir_def::{
    body::Body,
    db::{
        self,
        ids::{Id, Loc},
        vfs::VfsFileId,
    },
    expr,
    item::{self, Name},
    pat,
};

// --------------------------------------------------------------------------------
// Item scope
// --------------------------------------------------------------------------------

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

// --------------------------------------------------------------------------------
// Resolver
// --------------------------------------------------------------------------------

// /// [`ItemScope`] | [`ExprScope`]
// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum Scope {
//     Item(ItemScope),
//     Expr(ExprScope),
// }

// /// Slice of [`ExprScopeStack`]
// #[derive(Debug, Clone, PartialEq, Eq)]
// pub struct ExprScope {
//     //
// }

// --------------------------------------------------------------------------------
// Expression scope
// --------------------------------------------------------------------------------

/// Scope data for a definition
// TODO: Rename; it's not a stack!
#[derive(Debug, Default, PartialEq, Eq)]
pub struct ExprScopeStack {
    scopes: Arena<ScopeData>,
    // scope_by_expr: FxHashMap<Idx<Expr>, Idx<ScopeData>>,
}

impl ExprScopeStack {
    fn alloc_root_scope(&mut self) -> Idx<ScopeData> {
        todo!()
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct ScopeData {
    parent: Option<Idx<ScopeData>>,
    // block: Option<Idx<Block>>,
    // label: Option<(LabelId, Name)>,
    entries: Vec<ScopeEntry>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ScopeEntry {
    name: Name,
    pat: Idx<pat::Pat>,
}

pub(crate) fn proc_expr_scope_query(
    db: &dyn db::Def,
    proc_id: Id<Loc<item::DefProc>>,
) -> Arc<ExprScopeStack> {
    let body = db.proc_body(proc_id);
    self::body_expr_scope(db, &body)
}

#[allow(unused)]
fn body_expr_scope(db: &dyn db::Def, body: &Body) -> Arc<ExprScopeStack> {
    let mut scopes = ExprScopeStack::default();

    let root_scope = scopes.alloc_root_scope();
    self::compute_expr_scopes(body.root_block, body, &mut scopes, root_scope);

    Arc::new(scopes)
}

#[allow(unused)]
fn compute_expr_scopes(
    expr: Idx<expr::Expr>,
    body: &Body,
    scopes: &mut ExprScopeStack,
    scope: Idx<ScopeData>,
) {
    todo!()
}
