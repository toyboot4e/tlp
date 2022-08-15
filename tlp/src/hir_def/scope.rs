//! Item / expression scopes

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;

use std::{ops, sync::Arc};

use crate::{
    hir_def::{
        body::Body,
        db::{
            self,
            ids::{Id, ItemLoc},
            vfs::VfsFileId,
        },
        expr,
        item::{self, Name},
        pat,
    },
    syntax::ast,
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
    procs: FxHashMap<Name, Id<ItemLoc<item::DefProc>>>,
}

impl ItemScope {
    pub(crate) fn declare_proc(&mut self, name: Name, proc: Id<ItemLoc<item::DefProc>>) {
        // TOOD: consider upcasting or not
        // let id = DefId { loc_id: proc };
        // self.procs.insert(name, AnyDefId::from(id));
        self.procs.insert(name, proc);
    }

    pub fn lookup_proc(&self, name: &Name) -> Option<Id<ItemLoc<item::DefProc>>> {
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

// /// View to [`ExprScopeMap`] for one scope
// #[derive(Debug, Clone, PartialEq, Eq)]
// pub struct ExprScope {
//     //
// }

// --------------------------------------------------------------------------------
// Expression scope
// --------------------------------------------------------------------------------

/// All stack data for a definition
#[derive(Debug, Default, PartialEq, Eq)]
pub struct ExprScopeMap {
    scopes: Arena<ScopeData>,
    // scope_by_expr: FxHashMap<Idx<Expr>, Idx<ScopeData>>,
}

impl ExprScopeMap {
    fn alloc_root_scope(&mut self) -> Idx<ScopeData> {
        todo!()
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct ScopeData {
    parent: Option<Idx<ScopeData>>,
    // TODO: include AST location information
    // block: Option<Id<AstLoc<ast::Block>>>,
    entries: Vec<ScopeEntry>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ScopeEntry {
    name: Name,
    pat: Idx<pat::Pat>,
}

pub(crate) fn proc_expr_scope_query(
    db: &dyn db::Def,
    proc_id: Id<ItemLoc<item::DefProc>>,
) -> Arc<ExprScopeMap> {
    let body = db.proc_body(proc_id);
    self::body_expr_scope(db, &body)
}

#[allow(unused)]
fn body_expr_scope(db: &dyn db::Def, body: &Body) -> Arc<ExprScopeMap> {
    let mut scopes = ExprScopeMap::default();

    let root_scope = scopes.alloc_root_scope();
    self::compute_expr_scopes(body.root_block, body, &mut scopes, root_scope);

    Arc::new(scopes)
}

/// Walks through body expressions, creates scopes and tracks the scope for each expression
#[allow(unused)]
fn compute_expr_scopes(
    expr: Idx<expr::Expr>,
    body: &Body,
    scopes: &mut ExprScopeMap,
    scope: Idx<ScopeData>,
) {
    todo!()
}
