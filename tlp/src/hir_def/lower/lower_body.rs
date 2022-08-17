//! Lowers body and code blocks
//!
//! Each expression and pattern are given unique instance.

use std::sync::Arc;

use la_arena::Idx;

use crate::{
    hir_def::{
        body::{Body, BodySourceMap, SyntheticSyntax},
        db::{
            self,
            ids::{Id, ItemLoc},
        },
        expr::{self, Expr},
        item::{self, Name},
        pat,
    },
    syntax::{ast, ptr::AstPtr},
};

pub fn lower_proc_body_query(db: &dyn db::Def, proc_id: Id<ItemLoc<item::DefProc>>) -> Arc<Body> {
    db.proc_body_with_source_map(proc_id).0
}

pub fn lower_proc_body_with_source_map_query(
    db: &dyn db::Def,
    proc_id: Id<ItemLoc<item::DefProc>>,
) -> (Arc<Body>, Arc<BodySourceMap>) {
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

    let (body, map) = LowerExpr {
        db,
        body,
        map: Default::default(),
    }
    .lower_proc(proc.ast.clone());
    (Arc::new(body), Arc::new(map))
}

struct LowerExpr<'a> {
    db: &'a dyn db::Def,
    body: Body,
    map: BodySourceMap,
}

impl<'a> LowerExpr<'a> {
    pub fn lower_proc(mut self, proc: ast::DefProc) -> (Body, BodySourceMap) {
        self.lower_proc_params(proc.clone());

        let block = proc.block();
        self.body.root_block = self.lower_block(block);

        (self.body, self.map)
    }

    fn lower_proc_params(&mut self, _proc: ast::DefProc) {
        //
    }

    fn lower_block(&mut self, ast_block: ast::Block) -> Idx<Expr> {
        let mut children = Vec::new();

        for ast_expr in ast_block.exprs() {
            let hir_expr = self.lower_ast_expr(ast_expr);
            children.push(hir_expr);
        }

        let children = children.into_boxed_slice();
        let hir_block = expr::Block { children };

        let ast_ptr = AstPtr::new(&ast_block.into());
        self.alloc_expr(expr::Expr::Block(hir_block), ast_ptr)
    }

    fn lower_opt_ast_pat(&mut self, pat: Option<ast::Pat>) -> Idx<pat::Pat> {
        match pat {
            Some(pat) => self.lower_ast_pat(pat),
            None => self.alloc_missing_pat(),
        }
    }

    fn lower_ast_pat(&mut self, ast_pat: ast::Pat) -> Idx<pat::Pat> {
        let ast_ptr = AstPtr::new(&ast_pat.clone());
        match ast_pat {
            ast::Pat::PatPath(_) => todo!(),
            ast::Pat::PatIdent(ident) => {
                let name = Name::from_str(ident.ident_token().text());
                let hir_pat = pat::Pat::Bind { name };
                self.alloc_pat(hir_pat, ast_ptr)
            }
        }
    }

    fn lower_opt_ast_expr(&mut self, expr: Option<ast::Expr>) -> Idx<expr::Expr> {
        match expr {
            Some(expr) => self.lower_ast_expr(expr),
            None => self.alloc_missing_expr(),
        }
    }

    fn lower_ast_expr(&mut self, ast_expr: ast::Expr) -> Idx<Expr> {
        let ast_ptr = AstPtr::new(&ast_expr);
        match ast_expr {
            // expressions
            ast::Expr::Call(call) => {
                let path = self.lower_ast_expr(call.path().into());
                let args = call.args().map(|form| self.lower_ast_expr(form)).collect();

                let hir_expr = expr::Call { path, args };
                self.alloc_expr(expr::Expr::Call(hir_expr), ast_ptr)
            }
            ast::Expr::Let(let_) => {
                let pat = self.lower_opt_ast_pat(let_.pat());
                let rhs = self.lower_opt_ast_expr(let_.rhs());
                self.alloc_expr(expr::Let { pat, rhs }.into(), ast_ptr)
            }
            ast::Expr::Path(ast_path) => {
                let expr_path = expr::Path::lower(ast_path, self.db.upcast());
                self.alloc_expr(expr_path.into(), ast_ptr)
            }
            ast::Expr::Literal(lit) => match lit.kind() {
                ast::LiteralKind::Num(x) => self.alloc_expr(Expr::Literal(x.into()), ast_ptr),
                ast::LiteralKind::Str(_str) => {
                    todo!()
                }
                ast::LiteralKind::True(_) | ast::LiteralKind::False(_) => {
                    todo!()
                }
            },
            ast::Expr::Block(block) => self.lower_block(block),
        }
    }
}

/// # Allocators
impl<'a> LowerExpr<'a> {
    fn alloc_pat(&mut self, pat: pat::Pat, ptr: AstPtr<ast::Pat>) -> Idx<pat::Pat> {
        let idx = self.body.pats.alloc(pat);

        self.map.pat_ast_hir.insert(ptr.clone(), idx);
        self.map.pat_hir_ast.insert(idx, Ok(ptr));

        idx
    }

    fn alloc_missing_pat(&mut self) -> Idx<pat::Pat> {
        let idx = self.body.pats.alloc(pat::Pat::Missing);

        self.map.pat_hir_ast.insert(idx, Err(SyntheticSyntax));

        idx
    }

    fn alloc_expr(&mut self, expr: Expr, ptr: AstPtr<ast::Expr>) -> Idx<Expr> {
        let idx = self.body.exprs.alloc(expr);

        self.map.expr_ast_hir.insert(ptr.clone(), idx);
        self.map.expr_hir_ast.insert(idx, Ok(ptr));

        idx
    }

    fn alloc_missing_expr(&mut self) -> Idx<expr::Expr> {
        let idx = self.body.exprs.alloc(expr::Expr::Missing);

        self.map.expr_hir_ast.insert(idx, Err(SyntheticSyntax));

        idx
    }
}
