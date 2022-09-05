//! Item / expression scopes collected from [`Body`]

use std::sync::Arc;

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use smallvec::SmallVec;

use base::jar::Word;

use crate::ir::{
    body::{
        expr::{self, Expr, ExprData},
        pat::{self, Pat, PatData},
        Body, BodyData,
    },
    item,
    item_scope::ItemScope,
    IrDb,
};

/// All stack data for a definition
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ExprScopeMap {
    scopes: Arena<ScopeData>,
    scope_by_expr: FxHashMap<Expr, Idx<ScopeData>>,
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
        name: Word,
    ) -> Option<&ScopeEntry> {
        self.scope_chain(scope)
            .find_map(|scope| self.entries(scope).iter().find(|it| it.name == name))
    }

    pub fn scope_by_expr(&self) -> &FxHashMap<Expr, Idx<ScopeData>> {
        &self.scope_by_expr
    }

    pub fn scope_for_expr(&self, expr: Expr) -> Option<Idx<ScopeData>> {
        self.scope_by_expr.get(&expr).copied()
    }
}

/// Builder methods
impl ExprScopeMap {
    fn alloc_root_scope(&mut self) -> Idx<ScopeData> {
        self.scopes.alloc(ScopeData::default())
    }

    fn track_expr_scope(&mut self, expr: Expr, scope: Idx<ScopeData>) {
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

    fn add_bindings(&mut self, body_data: &BodyData, scope: Idx<ScopeData>, pat: Pat) {
        let pat_data = &body_data.tables[pat];
        if let PatData::Bind { name } = pat_data {
            let entry = ScopeEntry {
                name: name.clone(),
                pat,
            };
            self.scopes[scope].entries.push(entry);
        }
    }
}

/// Scope data created by a binding pattern
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ScopeData {
    parent: Option<Idx<ScopeData>>,
    // TODO: include AST location information
    // block: Option<Id<AstLoc<ast::Block>>>,
    /// The size is always one; a binding pattern only adds one variable to scope in toylisp.
    entries: SmallVec<[ScopeEntry; 1]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScopeEntry {
    pub name: Word,
    pub pat: pat::Pat,
}

pub(crate) fn proc_expr_scope_query(db: &dyn IrDb, proc: item::Proc) -> Arc<ExprScopeMap> {
    let body = proc.body(db);
    let body_data = body.data(db);
    self::body_expr_scope(db, &body_data)
}

#[allow(unused)]
fn body_expr_scope(db: &dyn IrDb, body_data: &BodyData) -> Arc<ExprScopeMap> {
    let mut scopes = ExprScopeMap::default();

    // start with the root block expression
    let root_scope = scopes.alloc_root_scope();
    self::compute_expr_scopes(body_data.root_block, body_data, &mut scopes, root_scope);

    Arc::new(scopes)
}

/// Walks through body expressions, creates scopes and tracks the scope for each expression
///
/// Returns the last scope index for tracking the current scope.
fn compute_expr_scopes(
    expr: expr::Expr,
    body_data: &BodyData,
    scopes: &mut ExprScopeMap,
    scope_idx: Idx<ScopeData>,
) -> Idx<ScopeData> {
    // Current scope is only modified by `Let` expression.
    // (Block scope creates a new scope, but it doesn't modify "current scope").
    let mut scope_idx = scope_idx;

    // track parent expression
    scopes.track_expr_scope(expr, scope_idx);

    // call into the child expressions
    match &body_data.tables[expr] {
        // --------------------------------------------------------------------------------
        // Handle block and binding patterns
        // --------------------------------------------------------------------------------
        ExprData::Block(block) => {
            let block_scope_idx = scopes.new_block_scope(scope_idx);

            // Overwrite the block scope with the deepest child.
            // This is important for traverse as `ScopeData` only contains `parernt` index.
            scopes.track_expr_scope(expr, block_scope_idx);

            self::compute_block_scopes(&block.children, body_data, scopes, block_scope_idx);
        }
        ExprData::Let(let_) => {
            // expr: track scope
            scope_idx = self::compute_expr_scopes(let_.rhs, body_data, scopes, scope_idx);

            // pat: create new scope
            scope_idx = scopes.append_scope(scope_idx);
            scopes.add_bindings(body_data, scope_idx, let_.pat);
        }

        // --------------------------------------------------------------------------------
        // Walk child expressions and track scope for them.
        // It should not modify curernt scope.
        // --------------------------------------------------------------------------------
        ExprData::Call(call) => {
            self::compute_expr_scopes(call.path, body_data, scopes, scope_idx);
            call.args.iter().for_each(|expr| {
                self::compute_expr_scopes(*expr, body_data, scopes, scope_idx);
            });
        }

        // --------------------------------------------------------------------------------
        // Terminals. No children, nothing to do
        // --------------------------------------------------------------------------------
        ExprData::Missing => {}
        ExprData::Path(_) => {}
        ExprData::Literal(_) => {}
    }

    scope_idx
}

/// Walks through body expressions, creates scopes and tracks the scope for each expression
fn compute_block_scopes(
    exprs: &[expr::Expr],
    body_data: &BodyData,
    scopes: &mut ExprScopeMap,
    scope_idx: Idx<ScopeData>,
) -> Idx<ScopeData> {
    let mut scope_idx = scope_idx;

    for expr in exprs {
        scope_idx = self::compute_expr_scopes(*expr, body_data, scopes, scope_idx);
    }

    scope_idx
}
