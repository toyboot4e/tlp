//! Lowers body and code blocks
//!
//! Each expression and pattern are given unique instance.

use std::sync::Arc;

use la_arena::Idx;

use crate::{
    hir_def::{
        body::Body,
        db::{
            self,
            ids::{Id, ItemLoc},
        },
        expr::{self, Expr},
        item::{self, Name},
        pat,
    },
    syntax::ast,
};

pub fn proc_body_query(db: &dyn db::Def, proc_id: Id<ItemLoc<item::DefProc>>) -> Arc<Body> {
    // body = block expr
    let proc_loc = db.lookup_intern_proc_loc(proc_id);
    let tree = db.file_item_list(proc_loc.file);
    let proc = &tree[proc_loc.idx];

    let dummy = Idx::from_raw(u32::MAX.into());
    let body = Body {
        root_block: dummy,
        exprs: Default::default(),
        pats: Default::default(),
    };

    LowerExpr { db, body }.lower_proc(proc.ast.clone())
}

struct LowerExpr<'a> {
    db: &'a dyn db::Def,
    body: Body,
    // TODO: source map:
    // /// AST expr ID → HIR expr ID
    // /// HIR expr ID → AST expr ID
}

impl<'a> LowerExpr<'a> {
    pub fn lower_proc(mut self, proc: ast::DefProc) -> Arc<Body> {
        self.lower_proc_params(proc.clone());

        let block = proc.block();
        self.body.root_block = self.lower_block(block);

        Arc::new(self.body)
    }

    fn lower_proc_params(&mut self, _proc: ast::DefProc) {
        //
    }

    fn lower_block(&mut self, block: ast::Block) -> Idx<Expr> {
        let mut children = Vec::new();

        for form in block.forms() {
            let expr = self.lower_ast_expr(form);
            children.push(expr);
        }

        let children = children.into_boxed_slice();
        let block = expr::Block { children };
        self.alloc_expr(expr::Expr::Block(block))
    }

    fn lower_opt_ast_pat(&mut self, pat: Option<ast::Pat>) -> Idx<pat::Pat> {
        match pat {
            Some(pat) => self.lower_ast_pat(pat),
            None => self.alloc_pat(pat::Pat::Missing),
        }
    }

    fn lower_ast_pat(&mut self, pat: ast::Pat) -> Idx<pat::Pat> {
        match pat.kind() {
            // FIXME: add PatIdent
            ast::PatKind::Path(path) => {
                // FIXME: identifier
                let components = path.components().collect::<Vec<_>>();
                assert_eq!(components.len(), 1);
                let ident = &components[0];

                let name = Name::from_str(ident.text());
                let pat = pat::Pat::Bind { name };
                self.alloc_pat(pat)
            }
        }
    }

    fn lower_opt_ast_expr(&mut self, expr: Option<ast::Form>) -> Idx<expr::Expr> {
        match expr {
            Some(expr) => self.lower_ast_expr(expr),
            None => self.alloc_expr(expr::Expr::Missing),
        }
    }

    fn lower_ast_expr(&mut self, form: ast::Form) -> Idx<Expr> {
        match form.kind() {
            // items
            ast::FormKind::DefProc(_proc) => todo!("nested procedure"),
            // expressions
            ast::FormKind::Call(call) => {
                let path = self.lower_ast_expr(call.path().into_form());
                let args = call.args().map(|form| self.lower_ast_expr(form)).collect();

                let expr = expr::Call { path, args };
                self.alloc_expr(expr::Expr::Call(expr))
            }
            ast::FormKind::Let(let_) => {
                let pat = self.lower_opt_ast_pat(let_.pat());
                let rhs = self.lower_opt_ast_expr(let_.rhs());
                self.alloc_expr(expr::Let { pat, rhs }.into())
            }
            ast::FormKind::Path(ast_path) => {
                let expr_path = expr::Path::lower(ast_path, self.db.upcast());
                self.alloc_expr(expr_path.into())
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
    fn alloc_pat(&mut self, pat: pat::Pat) -> Idx<pat::Pat> {
        self.body.pats.alloc(pat)
    }

    fn alloc_expr(&mut self, expr: Expr) -> Idx<Expr> {
        self.body.exprs.alloc(expr)
    }
}
