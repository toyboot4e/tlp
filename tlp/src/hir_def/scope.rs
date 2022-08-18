//! Item / expression scopes collected from [`Body`]

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use smallvec::SmallVec;

use std::{ops, sync::Arc};

use crate::hir_def::{
    body::Body,
    db::{
        self,
        ids::{HirItemLoc, Id},
        vfs::VfsFileId,
    },
    expr::{self, Expr},
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
    procs: FxHashMap<Name, Id<HirItemLoc<item::DefProc>>>,
}

impl ItemScope {
    pub(crate) fn declare_proc(&mut self, name: Name, proc: Id<HirItemLoc<item::DefProc>>) {
        // TOOD: consider upcasting or not
        // let id = DefId { loc_id: proc };
        // self.procs.insert(name, AnyDefId::from(id));
        self.procs.insert(name, proc);
    }

    pub fn lookup_proc(&self, name: &Name) -> Option<Id<HirItemLoc<item::DefProc>>> {
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
    scope_by_expr: FxHashMap<Idx<Expr>, Idx<ScopeData>>,
}

/// Accessors
impl ExprScopeMap {
    pub fn entries(&self, scope: Idx<ScopeData>) -> &[ScopeEntry] {
        &self.scopes[scope].entries
    }

    // /// If `scope` refers to a block expression scope, returns the corresponding `BlockId`.
    // pub fn block(&self, scope: Idx<ScopeData>) -> Option<Id<AstLoc<ast::Block>>> {stLoc
    //     self.scopes[scope].block
    // }

    pub fn scope_chain(&self, scope: Idx<ScopeData>) -> impl Iterator<Item = Idx<ScopeData>> + '_ {
        std::iter::successors(Some(scope), move |&scope| self.scopes[scope].parent)
    }

    pub fn resolve_name_in_scope_chain(
        &self,
        scope: Idx<ScopeData>,
        name: &Name,
    ) -> Option<&ScopeEntry> {
        self.scope_chain(scope)
            .find_map(|scope| self.entries(scope).iter().find(|it| it.name == *name))
    }

    pub fn scope_by_expr(&self) -> &FxHashMap<Idx<Expr>, Idx<ScopeData>> {
        &self.scope_by_expr
    }

    pub fn scope_for_expr(&self, expr: Idx<Expr>) -> Option<Idx<ScopeData>> {
        self.scope_by_expr.get(&expr).copied()
    }
}

/// Builder methods
impl ExprScopeMap {
    fn alloc_root_scope(&mut self) -> Idx<ScopeData> {
        self.scopes.alloc(ScopeData::default())
    }

    fn track_expr_scope(&mut self, expr: Idx<Expr>, scope: Idx<ScopeData>) {
        self.scope_by_expr.insert(expr, scope);
    }

    /// Creates a new block scope
    fn new_block_scope(
        &mut self,
        parent: Idx<ScopeData>,
        // block: Id<AstLoc<ast::Block>>,
    ) -> Idx<ScopeData> {
        self.scopes.alloc(ScopeData {
            parent: Some(parent),
            // block: Some(block),
            entries: SmallVec::new(),
        })
    }

    /// Creates a new scope on a binding pattern
    fn append_scope(&mut self, parent: Idx<ScopeData>) -> Idx<ScopeData> {
        self.scopes.alloc(ScopeData {
            parent: Some(parent),
            entries: SmallVec::new(),
        })
    }

    fn add_bindings(&mut self, body: &Body, scope: Idx<ScopeData>, pat: Idx<pat::Pat>) {
        let pattern = &body.pats[pat];
        if let pat::Pat::Bind { name } = pattern {
            let entry = ScopeEntry {
                name: name.clone(),
                pat,
            };
            self.scopes[scope].entries.push(entry);
        }
    }
}

/// Scope data created by a binding pattern
#[derive(Debug, Default, PartialEq, Eq)]
pub struct ScopeData {
    parent: Option<Idx<ScopeData>>,
    // TODO: include AST location information
    // block: Option<Id<AstLoc<ast::Block>>>,
    /// The size is always one; a binding pattern only adds one variable to scope in toylisp.
    entries: SmallVec<[ScopeEntry; 1]>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ScopeEntry {
    pub name: Name,
    pub pat: Idx<pat::Pat>,
}

pub(crate) fn proc_expr_scope_query(
    db: &dyn db::Def,
    proc_id: Id<HirItemLoc<item::DefProc>>,
) -> Arc<ExprScopeMap> {
    let body = db.proc_body(proc_id);
    self::body_expr_scope(db, &body)
}

#[allow(unused)]
fn body_expr_scope(db: &dyn db::Def, body: &Body) -> Arc<ExprScopeMap> {
    let mut scopes = ExprScopeMap::default();

    // start with the root block expression
    let root_scope = scopes.alloc_root_scope();
    self::compute_expr_scopes(body.root_block, body, &mut scopes, root_scope);

    Arc::new(scopes)
}

/// Walks through body expressions, creates scopes and tracks the scope for each expression
///
/// Returns the last scope index for tracking the current scope.
fn compute_expr_scopes(
    expr: Idx<expr::Expr>,
    body: &Body,
    scopes: &mut ExprScopeMap,
    scope_idx: Idx<ScopeData>,
) -> Idx<ScopeData> {
    // Current scope is only modified by `Let` expression.
    // (Block scope creates a new scope, but it doesn't modify "current scope").
    let mut scope_idx = scope_idx;

    // track parent expression
    scopes.track_expr_scope(expr, scope_idx);

    // call into the child expressions
    match &body.exprs[expr] {
        // --------------------------------------------------------------------------------
        // Handle block and binding patterns
        // --------------------------------------------------------------------------------
        Expr::Block(block) => {
            let block_scope_idx = scopes.new_block_scope(scope_idx);

            // Overwrite the block scope with the deepest child.
            // This is important for traverse as `ScopeData` only contains `parernt` index.
            scopes.track_expr_scope(expr, block_scope_idx);

            self::compute_block_scopes(&block.children, body, scopes, block_scope_idx);
        }
        Expr::Let(let_) => {
            // expr: track scope
            scope_idx = self::compute_expr_scopes(let_.rhs, body, scopes, scope_idx);

            // pat: create new scope
            scope_idx = scopes.append_scope(scope_idx);
            scopes.add_bindings(body, scope_idx, let_.pat);
        }

        // --------------------------------------------------------------------------------
        // Walk child expressions and track scope for them.
        // It should not modify curernt scope.
        // --------------------------------------------------------------------------------
        Expr::Call(call) => {
            self::compute_expr_scopes(call.path, body, scopes, scope_idx);
            call.args.iter().for_each(|expr| {
                self::compute_expr_scopes(*expr, body, scopes, scope_idx);
            });
        }

        // --------------------------------------------------------------------------------
        // Terminals. No children, nothing to do
        // --------------------------------------------------------------------------------
        Expr::Missing => {}
        Expr::Path(_) => {}
        Expr::Literal(_) => {}
    }

    scope_idx
}

/// Walks through body expressions, creates scopes and tracks the scope for each expression
fn compute_block_scopes(
    exprs: &[Idx<expr::Expr>],
    body: &Body,
    scopes: &mut ExprScopeMap,
    scope_idx: Idx<ScopeData>,
) -> Idx<ScopeData> {
    let mut scope_idx = scope_idx;

    for expr in exprs {
        scope_idx = self::compute_expr_scopes(*expr, body, scopes, scope_idx);
    }

    scope_idx
}
