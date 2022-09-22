//! Body

pub mod expr;
pub mod expr_scope;
pub mod pat;

use base::{
    span::Span,
    tbl::{origin_table::origin_table, tables},
};

use crate::ir::IrJar;

use self::{
    expr::{Expr, ExprData},
    pat::{Pat, PatData},
};

#[salsa::tracked(jar = IrJar)]
pub struct Body {
    #[return_ref]
    pub data: BodyData,
    #[return_ref]
    pub spans: BodySpans,
}

/// Body AST
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BodyData {
    /// Interning tables for expressions and the like.
    pub tables: BodyTables,

    pub param_pats: Vec<Pat>,

    // /// Parameter declarations
    // pub parameter_decls: Vec<LocalVariableDecl>,
    /// The root block expression
    pub root_block: Expr,
}

impl BodyData {
    pub fn root_block(&self) -> &expr::Block {
        match &self.tables[self.root_block] {
            expr::ExprData::Block(x) => x,
            _ => unreachable!(),
        }
    }
}

tables! {
    /// Tables that store the data for expr in the AST.
    /// You can use `tables[expr]` (etc) to access the data.
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct BodyTables {
        pub exprs: alloc Expr => ExprData,
        pub pats: alloc Pat => PatData,
        // named_exprs: alloc NamedExpr => NamedExprData,
        // local_variable_decls: alloc LocalVariableDecl => LocalVariableDeclData,
    }
}

/// Represents missing span
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct SyntheticSyntax;

origin_table! {
    /// Side table that contains the spans for everything in a syntax tree.
    /// This isn't normally needed except for diagnostics, so it's
    /// kept separate to avoid reducing incremental reuse.
    /// You can request it by invoking the `spans`
    /// method in the `dada_parse` prelude.
    #[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
    pub struct BodySpans {
        expr_spans: Expr => Result<Span, SyntheticSyntax>,
        pat_spans: Pat => Result<Span, SyntheticSyntax>,
        // named_expr_spans: NamedExpr => Span,
        // local_variable_decl_spans: LocalVariableDecl => LocalVariableDeclSpan,
    }
}
