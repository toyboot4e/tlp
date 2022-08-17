//! Item definition body and code blocks lowered from AST

use la_arena::{Arena, ArenaMap, Idx};
use rustc_hash::FxHashMap;

use crate::{
    hir_def::{
        expr::{self, Expr},
        pat::{self, Pat},
    },
    syntax::{ast, ptr::AstPtr},
};

/// Body
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Body {
    pub root_block: Idx<expr::Expr>,
    pub exprs: Arena<expr::Expr>,
    pub pats: Arena<pat::Pat>,
}

impl Body {
    pub fn root_block(&self) -> &expr::Block {
        match &self.exprs[self.root_block] {
            expr::Expr::Block(seq) => seq,
            _ => unreachable!(),
        }
    }
}

/// Expr variant getters
impl Body {
    pub fn get_path(&self, path: Idx<expr::Expr>) -> &expr::Path {
        match &self.exprs[path] {
            Expr::Path(path) => path,
            _ => unreachable!(),
        }
    }
}

/// Map between HIR `Idx` and AST pointers
///
/// # Incremental compilation
///
/// HIR uses positional index instead of source syntax location. It works great on source change
/// that does not change item/expression order.
#[derive(Default, Debug, Eq, PartialEq)]
pub struct BodySourceMap {
    pub(crate) expr_ast_hir: FxHashMap<AstPtr<ast::Expr>, Idx<Expr>>,
    pub(crate) expr_hir_ast: ArenaMap<Idx<Expr>, ToAst<ast::Expr>>,

    pub(crate) pat_ast_hir: FxHashMap<AstPtr<ast::Pat>, Idx<Pat>>,
    pub(crate) pat_hir_ast: ArenaMap<Idx<Pat>, ToAst<ast::Pat>>,
    // /// Diagnostics accumulated during body lowering. These contain `AstPtr`s and so are stored in
    // /// the source map (since they're just as volatile).
    // pub(crate) diagnostics: Vec<BodyDiagnostic>,
}

/// Syntax pointer to an existing AST node or missing
pub type ToAst<Ast> = Result<AstPtr<Ast>, SyntheticSyntax>;

/// Represents missing syntax in AST
#[derive(Default, Debug, Eq, PartialEq, Clone, Copy)]
pub struct SyntheticSyntax;
