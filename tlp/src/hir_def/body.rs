//! HIR expressions and patterns lowered from AST item definition body
//!
//! # Source map pattern
//!
//! [`BodySourceMap`] records the map between HIR index and AST pointers.

pub mod expr;
pub mod expr_scope;
pub mod pat;

use la_arena::{Arena, ArenaMap, Idx};
use rustc_hash::FxHashMap;

use crate::syntax::{ast, ptr::AstPtr};

use self::{expr::Expr, pat::Pat};

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

/// Map between HIR expression / pattern `Idx` and AST pointers
#[derive(Default, Debug, Eq, PartialEq)]
pub struct BodySourceMap {
    pub expr_ast_hir: FxHashMap<AstPtr<ast::Expr>, Idx<Expr>>,
    pub expr_hir_ast: ArenaMap<Idx<Expr>, ToAst<ast::Expr>>,

    pub pat_ast_hir: FxHashMap<AstPtr<ast::Pat>, Idx<Pat>>,
    pub pat_hir_ast: ArenaMap<Idx<Pat>, ToAst<ast::Pat>>,
    // /// Diagnostics accumulated during body lowering. These contain `AstPtr`s and so are stored in
    // /// the source map (since they're just as volatile).
    // pub(crate) diagnostics: Vec<BodyDiagnostic>,
}

/// Syntax pointer to an existing AST node or missing (used by [`BodySourceMap`])
pub type ToAst<Ast> = Result<AstPtr<Ast>, SyntheticSyntax>;

/// Represents missing syntax in AST (used by [`BodySourceMap`])
#[derive(Default, Debug, Eq, PartialEq, Clone, Copy)]
pub struct SyntheticSyntax;
