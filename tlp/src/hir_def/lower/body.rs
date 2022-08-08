//! Lowers body and code blocks

use std::sync::Arc;

use la_arena::Idx;

use crate::{
    hir_def::{
        body::{
            expr::{self, Expr},
            Body,
        },
        db::{
            self,
            ids::{Id, Loc},
        },
        item::{self, Name},
    },
    syntax::ast,
};

pub fn proc_body_query(db: &dyn db::Def, proc_id: Id<Loc<item::DefProc>>) -> Arc<Body> {
    // collect parameters as pattern IDs

    // body = block expr
    let proc_loc = db.lookup_intern_proc(proc_id);
    let tree = db.file_item_list(proc_loc.file);
    let proc = &tree[proc_loc.idx];

    LowerExpr {
        db,
        body: Body::default(),
    }
    .lower_proc(proc.ast.clone())
}

/// Proc AST → Proc HIR
struct LowerExpr<'a> {
    db: &'a dyn db::Def,
    body: Body,
    // TODO: source map:
    // /// AST expr ID → HIR expr ID
    // /// HIR expr ID → AST expr ID
}

impl<'a> LowerExpr<'a> {
    pub fn lower_proc(mut self, proc: ast::DefProc) -> Arc<Body> {
        self.lower_proc_body(proc.clone());
        Arc::new(self.body)
    }

    fn lower_proc_body(&mut self, proc: ast::DefProc) {
        if let Some(body) = proc.body() {
            for form in body.forms() {
                let expr = self.lower_form(form);
                self.body.root.children.push(expr);
            }
        }
    }

    fn lower_form(&mut self, form: ast::Form) -> Idx<Expr> {
        match form.kind() {
            ast::FormKind::DefProc(_proc) => todo!("nested procedure"),
            ast::FormKind::Call(call) => {
                let name = Name::from_str(call.name_tk().text());

                let args = call
                    .arg_forms()
                    .map(|form| self.lower_form(form))
                    .collect::<Vec<_>>();
                let call = expr::Call { name, args };

                self.alloc_expr(expr::Expr::Call(call))
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

    fn alloc_expr(&mut self, expr: Expr) -> Idx<Expr> {
        self.body.exprs.alloc(expr)
    }
}
