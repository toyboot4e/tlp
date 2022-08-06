//! Lowers ?

use std::sync::Arc;

use la_arena::Idx;

use crate::{
    hir_def::{
        body::{expr::Expr, Body},
        db::{
            self,
            ids::{Id, Loc},
        },
        decl,
    },
    syntax::ast,
};

pub fn proc_data_query(db: &dyn db::Def, proc_id: Id<Loc<decl::DefProc>>) -> Arc<decl::DefProc> {
    let proc_loc = proc_id.lookup(db);
    let tree = proc_loc.tree.item_tree(db);
    let proc = &tree[proc_loc.item];

    // NOTE: In RA, Function is mapepd to FunctionData applying cfg flags.
    // Here we just clone the syntax:
    Arc::new(proc.clone())
}

pub fn proc_body_query(db: &dyn db::Def, proc_id: Id<Loc<decl::DefProc>>) -> Arc<Body> {
    // collect parameters as pattern IDs

    // body = block expr
    let proc_loc = db.lookup_intern_proc(proc_id);
    let tree = proc_loc.tree.item_tree(db);
    let proc = &tree[proc_loc.item];

    LowerExpr {
        db,
        body: Body::default(),
    }
    .lower_proc(proc.ast.clone())
}

/// Proc AST → Proc HIR
struct LowerExpr<'a> {
    db: &'a dyn db::Def,
    /// The HIR procedure body we're building up
    body: Body,
    // TODO: track source map:
    // /// AST expr ID → HIR expr ID
    // /// HIR expr ID → AST expr ID
}

impl<'a> LowerExpr<'a> {
    pub fn lower_proc(mut self, proc: ast::DefProc) -> Arc<Body> {
        self.lower_proc_params(proc.clone());
        self.lower_proc_body(proc.clone());
        Arc::new(self.body)
    }

    fn lower_proc_params(&mut self, proc: ast::DefProc) {
        if proc.params().is_some() {
            // TODO: lower self parameter

            // lower other parameters
        }
    }

    fn lower_proc_body(&mut self, proc: ast::DefProc) {
        // TODO: Consider block modifier (e.g. coroutines)

        if let Some(body) = proc.body() {
            for form in body.forms() {
                self.lower_form(form);
            }
        }
    }

    /// Allocates an expression making up the AST-HIR map. The separation of the source text from
    /// the HIR is helpful to not recompute on syntax changes that do not affect the HIR.
    fn lower_form(&mut self, form: ast::Form) -> Idx<Expr> {
        // TODO: cast AST node to a syntax pointer in order to make up the AST-HIR map
        match form.kind() {
            ast::FormKind::DefProc(_proc) => todo!("nested procedure"),
            ast::FormKind::Call(_call) => {
                todo!("function call");
            }
            ast::FormKind::Literal(lit) => match lit.kind() {
                ast::LiteralKind::Num(x) => self.alloc_expr(Expr::Literal(x.into())),
                ast::LiteralKind::Str(_str) => {
                    todo!()
                }
                ast::LiteralKind::True(_) | ast::LiteralKind::False(_) => {
                    todo!()
                }
            },
        }
    }
}

/// # Allocators
impl<'a> LowerExpr<'a> {
    // fn alloc_pat(&mut self, pat: Pattern, ptr: PatPtr) -> Id<Pattern> {
    //     // InFile<T>
    //     let src = self.expander.to_source(ptr);
    //     let id = self.make_pat(pat, Ok(src.clone()));
    //     // InFile<SyntaxPtr<T>> <-> ExprId
    //     self.source_map.pat_map.insert(src, id);
    //     id
    // }

    // fn make_pat(&mut self, pat: Pat, src: Result<PatSource, SyntheticSyntax>) -> PatId {
    //     let id = self.body.pats.alloc(pat);
    //     self.source_map.pat_map_back.insert(id, src);
    //     id
    // }

    /// Allocates an expression making up the AST-HIR map. The separation of the source text from
    /// the HIR is helpful to not recompute on syntax changes that do not affect the HIR.
    fn alloc_expr(&mut self, expr: Expr) -> Idx<Expr> {
        // TODO: make up the AST-HIR map
        self.body.exprs.alloc(expr)
    }
}
