//! Resolve expressions by name
//!
//! TODO: incremental computation

use la_arena::Idx;

use crate::ir::{
    body::{
        expr::{self, Expr},
        expr_scope::{ExprScopeMap, ScopeData},
        pat::Pat,
    },
    item,
    item_scope::ItemScope,
    InputFileExt, IrDb,
};

/// `Item` | `Expr`
#[derive(Debug, Clone, PartialEq, Eq)]
enum Scope {
    Item(ItemScope),
    Expr(ExprScope),
}

/// Self-contained view to [`ExprScopeMap`]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprScope {
    proc: item::Proc,
    map: ExprScopeMap,
    idx: Idx<ScopeData>,
}

#[derive(Debug, Clone)]
pub struct Resolver {
    /// Inner-most scope is the last item.
    ///
    /// Invariant: There exists at least one `ItemScope` at the start of the vec.
    scopes: Vec<Scope>,
}

/// Resolution in value namespace
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValueNs {
    /// Local binding pattern
    Pat(Pat),
    Proc(item::Proc),
}

impl Resolver {
    fn scopes(&self) -> impl Iterator<Item = &Scope> {
        self.scopes.iter().rev()
    }

    pub fn resolve_path_as_value(&self, db: &dyn IrDb, path: &expr::Path) -> Option<ValueNs> {
        assert_eq!(path.segments.len(), 1, "support path");
        let ident = path.segments[0];

        for scope in self.scopes() {
            match scope {
                Scope::Expr(scope) => {
                    let scope_data = scope.map.data(db);

                    if let Some(entry) = scope_data
                        .entries(scope.idx)
                        .iter()
                        .find(|entry| entry.name == ident)
                    {
                        return Some(ValueNs::Pat(entry.pat));
                    }
                }
                Scope::Item(items) => {
                    let scope_data = items.data(db);

                    if let Some(proc) = scope_data.lookup_proc(&ident) {
                        return Some(ValueNs::Proc(proc));
                    }

                    // TODO: lookup global variables
                }
                _ => {}
            }
        }

        None
    }
}

pub(crate) fn resolver_for_proc_expr(db: &dyn IrDb, proc: item::Proc, expr: Expr) -> Resolver {
    let scopes = proc.expr_scopes(db).data(db);
    let scope = scopes.scope_for_expr(expr);
    self::resolver_for_proc_scope(db, proc, scope)
}

pub(crate) fn resolver_for_proc_scope(
    db: &dyn IrDb,
    proc: item::Proc,
    scope_idx: Option<Idx<ScopeData>>,
) -> Resolver {
    let mut r = Resolver::new();

    // item scope
    let item_scope = proc.span(db).input_file.item_scope(db);
    r = r.push_file_item_scope(item_scope);

    // body scopes
    let scope_idx = match scope_idx {
        Some(idx) => idx,
        None => return r,
    };

    let scopes = proc.expr_scopes(db);
    let scope_chain = scopes.data(db).scope_chain(scope_idx).collect::<Vec<_>>();

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

        r = r.push_proc_expr_scope(proc, scopes, scope);
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

    fn push_file_item_scope(self, item_scope: ItemScope) -> Self {
        self.push_scope(Scope::Item(item_scope))
    }

    // fn push_block_item_scope(self, _) -> Resolver { }

    fn push_proc_expr_scope(
        self,
        proc: item::Proc,
        map: ExprScopeMap,
        idx: Idx<ScopeData>,
    ) -> Self {
        self.push_scope(Scope::Expr(ExprScope { proc, map, idx }))
    }
}
