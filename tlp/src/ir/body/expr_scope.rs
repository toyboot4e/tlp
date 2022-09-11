//! Item / expression scopes collected from [`BodyData`]

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;
use smallvec::SmallVec;

use base::jar::Word;

use crate::ir::{
    body::{
        expr::Expr,
        pat::{self, Pat, PatData},
        BodyData,
    },
    IrJar,
};

#[salsa::tracked(jar = IrJar)]
pub struct ExprScopeMap {
    #[return_ref]
    pub data: ExprScopeMapData,
}

/// All stack data for a definition
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ExprScopeMapData {
    scopes: Arena<ScopeData>,
    scope_by_expr: FxHashMap<Expr, Idx<ScopeData>>,
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

/// Accessors
impl ExprScopeMapData {
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

/// Builder methods for the `lower` module
impl ExprScopeMapData {
    pub(crate) fn alloc_root_scope(&mut self) -> Idx<ScopeData> {
        self.scopes.alloc(ScopeData::default())
    }

    pub(crate) fn track_expr_scope(&mut self, expr: Expr, scope: Idx<ScopeData>) {
        self.scope_by_expr.insert(expr, scope);
    }

    /// Creates a new block scope
    pub(crate) fn new_block_scope(
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
    pub(crate) fn append_scope(&mut self, parent: Idx<ScopeData>) -> Idx<ScopeData> {
        self.scopes.alloc(ScopeData {
            parent: Some(parent),
            entries: SmallVec::new(),
        })
    }

    pub(crate) fn add_bindings(&mut self, body_data: &BodyData, scope: Idx<ScopeData>, pat: Pat) {
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
