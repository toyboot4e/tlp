//! Lowers body and code blocks

use std::sync::Arc;

use la_arena::Idx;

use crate::{
    hir_def::{
        body::Body,
        db::{
            self,
            ids::{Id, Loc},
        },
        expr::{self, Expr},
        item::{self, Name},
        pat,
    },
    syntax::ast,
};

pub fn proc_body_query(db: &dyn db::Def, proc_id: Id<Loc<item::DefProc>>) -> Arc<Body> {
    // body = block expr
    let proc_loc = db.lookup_intern_proc(proc_id);
    let tree = db.file_item_list(proc_loc.file);
    let proc = &tree[proc_loc.idx];

    let body = Body::new();
    LowerExpr { _db: db, body }.lower_proc(proc.ast.clone())
}

struct LowerExpr<'a> {
    _db: &'a dyn db::Def,
    body: Body,
    // TODO: source map:
    // /// AST expr ID → HIR expr ID
    // /// HIR expr ID → AST expr ID
}

impl<'a> LowerExpr<'a> {
    pub fn lower_proc(mut self, proc: ast::DefProc) -> Arc<Body> {
        self.lower_proc_params(proc.clone());
        self.lower_proc_body(proc.clone());
        Arc::new(self.body)
    }

    fn lower_proc_params(&mut self, _proc: ast::DefProc) {
        //
    }

    fn lower_proc_body(&mut self, proc: ast::DefProc) {
        if let Some(body) = proc.body() {
            for form in body.forms() {
                let expr = self.lower_form(form);
                self.body.root_block_mut().children.push(expr);
            }
        }
    }

    fn lower_form(&mut self, form: ast::Form) -> Idx<Expr> {
        match form.kind() {
            ast::FormKind::DefProc(_proc) => todo!("nested procedure"),
            ast::FormKind::Call(call) => {
                let name = Name::from_str(call.name_tk().unwrap().text());

                let args = call
                    .arg_forms()
                    .map(|form| self.lower_form(form))
                    .collect::<Vec<_>>();
                let call = expr::Call { name, args };

                self.alloc_expr(expr::Expr::Call(call))
            }
            ast::FormKind::Let(let_) => {
                if let Some(_pat) = let_.pat() {
                    // self.alloc_pat
                }
                todo!()
            }
            ast::FormKind::Path(_) => {
                todo!()
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
/// TODO: Source map pattern
impl<'a> LowerExpr<'a> {
    fn _alloc_pat(&mut self, pat: ast::Ident) -> Idx<pat::Pat> {
        let name = Name::from_str(pat.text());
        let pat = pat::Pat::name(name);
        self.body.pats.alloc(pat)
    }

    fn alloc_expr(&mut self, expr: Expr) -> Idx<Expr> {
        self.body.exprs.alloc(expr)
    }
}
