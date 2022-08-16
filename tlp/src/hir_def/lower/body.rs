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

        let block = proc.block();
        self.body.root_block = self.lower_block(block);

        Arc::new(self.body)
    }

    fn lower_proc_params(&mut self, _proc: ast::DefProc) {
        //
    }

    fn lower_form(&mut self, form: ast::Form) -> Idx<Expr> {
        match form.kind() {
            // items
            ast::FormKind::DefProc(_proc) => todo!("nested procedure"),
            // expressions
            ast::FormKind::Call(call) => {
                let path = call.path();
                // TODO: support path
                let ident = path.components().next().unwrap();
                let name = Name::from_str(ident.text());

                let args = call
                    .arg_forms()
                    .map(|form| self.lower_form(form))
                    .collect::<Vec<_>>();

                let expr = expr::Call { name, args };
                self.alloc_expr(expr::Expr::Call(expr))
            }
            ast::FormKind::Let(let_) => {
                let pat = self.lower_opt_ast_pat(let_.pat());
                let rhs = self.lower_opt_ast_expr(let_.rhs());

                let expr = expr::Let { pat, rhs };

                self.alloc_expr(expr::Expr::Let(expr))
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

    fn lower_block(&mut self, block: ast::Block) -> Idx<Expr> {
        let mut children = Vec::new();

        for form in block.forms() {
            let expr = self.lower_form(form);
            children.push(expr);
        }

        let block = expr::Block { children };
        self.alloc_expr(expr::Expr::Block(block))
    }

    fn lower_opt_ast_expr(&mut self, expr: Option<ast::Form>) -> Idx<expr::Expr> {
        todo!()
    }

    fn lower_opt_ast_pat(&mut self, pat: Option<ast::Pat>) -> Idx<pat::Pat> {
        todo!()
    }
}

/// # Allocators
/// TODO: Source map pattern
impl<'a> LowerExpr<'a> {
    fn alloc_pat(&mut self, pat: pat::Pat) -> Idx<pat::Pat> {
        self.body.pats.alloc(pat)
    }

    fn alloc_opt_pat(&mut self, pat: Option<pat::Pat>) -> Idx<pat::Pat> {
        self.alloc_pat(pat.unwrap_or(pat::Pat::Missing))
    }

    fn alloc_expr(&mut self, expr: Expr) -> Idx<Expr> {
        self.body.exprs.alloc(expr)
    }

    fn alloc_opt_expr(&mut self, expr: Option<Expr>) -> Idx<Expr> {
        self.alloc_expr(expr.unwrap_or(expr::Expr::Missing))
    }
}
