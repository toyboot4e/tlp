//! Resolve expressions by name

// TODO: replace `Arc` with `salsa` struct ID?
use std::sync::Arc;

use la_arena::{Arena, Idx};

use crate::ir::{
    body::{
        expr::Expr,
        expr_scope::{ExprScopeMap, ScopeData},
    },
    item,
    item_scope::ItemScope,
    IrDb,
};

/// `Item` | `Expr`
#[derive(Debug, Clone, PartialEq, Eq)]
enum Scope {
    Item(Arc<ItemScope>),
    Expr(ExprScope),
}

/// Self-contained view to [`ExprScopeMap`]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprScope {
    proc: item::Proc,
    map: Arc<ExprScopeMap>,
    idx: Idx<ScopeData>,
}

#[derive(Debug, Clone)]
pub struct Resolver {
    /// Inner-most scope is the last item.
    ///
    /// Invariant: There exists at least one `ItemScope` at the start of the vec.
    scopes: Vec<Scope>,
}

pub fn resolver_for_proc_expr(db: &dyn IrDb, proc: item::Proc, expr_id: Idx<Expr>) -> Resolver {
    todo!("calc expr scope")
    // let scopes = db.proc_expr_scope_map(proc);
    // let scope = scopes.scope_for_expr(expr_id);
    // self::resolver_for_proc_scope(db, proc, scope)
}

pub fn resolver_for_proc_scope(
    db: &dyn IrDb,
    proc_loc_id: item::Proc,
    scope_idx: Option<Idx<ScopeData>>,
) -> Resolver {
    let mut r = Resolver::new();

    // item scope
    // let item_scope = proc.item_scope(db);
    let item_scope: Arc<ItemScope> = todo!("item scope for proc");
    r = r.push_file_item_scope(item_scope);

    // body scopes
    let scope_idx = match scope_idx {
        Some(idx) => idx,
        None => return r,
    };

    // let scopes = db.proc_expr_scope_map(proc_loc_id);
    let scopes: Arc<ExprScopeMap> = todo!("expr scope map");
    let scope_chain = scopes.scope_chain(scope_idx).collect::<Vec<_>>();

    r.scopes.reserve(scope_chain.len());

    for scope in scope_chain.into_iter().rev() {
        // TODO: block item list
        // if let Some(block) = scopes.block(scope) {
        //     if let Some(def_map) = db.block_def_map(block) {
        //         let root = def_map.root();
        //         r = r.push_module_scope(def_map, root);
        //         // FIXME: This adds as many module scopes as there are blocks, but resolving in each
        //         // already traverses all parents, so this is O(n²). I think we could only store the
        //         // innermost module scope instead?
        //     }
        // }

        r = r.push_proc_expr_scope(proc_loc_id, Arc::clone(&scopes), scope);
    }

    r
}

/// Builder
impl Resolver {
    fn new() -> Self {
        Self {
            scopes: Vec::with_capacity(0),
        }
    }

    fn push_scope(mut self, scope: Scope) -> Self {
        self.scopes.push(scope);
        self
    }

    fn push_file_item_scope(self, item_scope: Arc<ItemScope>) -> Self {
        self.push_scope(Scope::Item(item_scope))
    }

    // fn push_block_item_scope(self, _) -> Resolver { }

    fn push_proc_expr_scope(
        self,
        proc: item::Proc,
        map: Arc<ExprScopeMap>,
        idx: Idx<ScopeData>,
    ) -> Self {
        self.push_scope(Scope::Expr(ExprScope { proc, map, idx }))
    }
}
