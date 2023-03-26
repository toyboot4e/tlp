//! Call frame

use rustc_hash::FxHashMap;
use salsa::DebugWithDb;

use crate::{
    ir::{
        body::{
            expr::{self, Expr},
            pat::{Pat, PatData},
        },
        item,
        resolve::ValueNs,
    },
    Db,
};

/// Compile-time call frame information
#[derive(Debug)]
pub struct CallFrame {
    proc: item::Proc,
    /// Maps [`Pat`] to local index in the call frame
    // TODO: consider using Vec-based map
    pub locals: FxHashMap<Pat, usize>,
    /// Number of arguments
    #[allow(unused)]
    n_args: usize,
    /// Number of local variables without arguments
    n_locals: usize,
}

impl CallFrame {
    pub fn new(db: &Db, proc: item::Proc) -> Self {
        let body = proc.body(db);
        let body_data = body.data(db);

        let pats = &body_data.tables.pats;
        let bind_pats = pats
            .enumerate()
            .filter(|(_, data)| matches!(data, PatData::Bind { .. }))
            .map(|(pat, _)| pat);

        let mut locals = FxHashMap::default();
        let mut n_vars = 0;

        // REMARK: Arguments must first apear in the binding patterns
        for pat in bind_pats {
            locals.insert(pat, n_vars);
            n_vars += 1;
        }

        let n_args = body_data.param_pats.len();
        let n_locals = n_vars - n_args;

        Self {
            proc,
            locals,
            n_args,
            n_locals,
        }
    }

    pub fn n_locals(&self) -> usize {
        self.n_locals
    }

    pub fn n_vars(&self) -> usize {
        self.n_args + self.n_locals
    }

    /// Resolves pattern to a local variable offset
    pub fn resolve_path_as_local(
        &self,
        db: &Db,
        path_expr: Expr,
        path: &expr::Path,
    ) -> Option<usize> {
        let resolver = self.proc.expr_resolver(db, path_expr);

        let binding_pat = match resolver.resolve_path_as_value(db, path)? {
            ValueNs::Pat(p) => p,
            x => todo!(
                "path was resolved to a non-pattern: path `{:?}`, resolution: `{:?}`",
                path.debug(db),
                x
            ),
        };

        self.locals.get(&binding_pat).map(|x| *x)
    }
}
