//! Body

use base::{
    jar::Word,
    span::Span,
    tbl::{id, origin_table::origin_table, tables},
};

use crate::ir::IrJar;

#[salsa::tracked(jar = IrJar)]
pub struct Body {
    #[return_ref]
    data: BodyData,
    #[return_ref]
    spans: BodySpans,
}

/// Body AST
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BodyData {
    /// Interning tables for expressions and the like.
    pub tables: BodyTables,

    // /// Parameter declarations
    // pub parameter_decls: Vec<LocalVariableDecl>,
    /// The root block expression
    pub root_block: Expr,
}

tables! {
    /// Tables that store the data for expr in the AST.
    /// You can use `tables[expr]` (etc) to access the data.
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct BodyTables {
        exprs: alloc Expr => ExprData,
        // named_exprs: alloc NamedExpr => NamedExprData,
        // local_variable_decls: alloc LocalVariableDecl => LocalVariableDeclData,
    }
}

origin_table! {
    /// Side table that contains the spans for everything in a syntax tree.
    /// This isn't normally needed except for diagnostics, so it's
    /// kept separate to avoid reducing incremental reuse.
    /// You can request it by invoking the `spans`
    /// method in the `dada_parse` prelude.
    #[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
    pub struct BodySpans {
        expr_spans: Expr => Span,
        // named_expr_spans: NamedExpr => Span,
        // local_variable_decl_spans: LocalVariableDecl => LocalVariableDeclSpan,
    }
}

id! {
    /// ID of [`ExprData`] that implements [`InternAllocKey`] and [`InternKey`]
    ///
    /// # Trait methods
    ///
    /// - [`InternKey::data`] -> [`&ExprData`]
    /// - [`InternAllocKey::data_mut`] -> [`&mut ExprData`]
    /// - [`InternAllocKey:::max_key`] -> [`Self`]
    ///
    /// [`InternAllocKey`]: base::tbl::InternAllocKey
    /// [`InternKey`]: base::tbl::InternKey
    ///
    /// [`InternValue::data`]: base::tbl::InternKey::data
    /// [`InternAllocKey:::max_key`]: base::tbl::InternAllocKey::max_key
    /// [`InternAllocKey::data_mut`]: base::tbl::InternAllocKey::data_mut
    pub struct Expr;
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub enum ExprData {
    Block(Vec<Expr>),
    Id(Word),
    /// parse or other error
    Error,
}

// id!(pub struct LocalVariableDecl);
//
// #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
// pub struct LocalVariableDeclData {
//     pub atomic: Atomic,
//     pub name: Word,
//     pub ty: Option<crate::ty::Ty>,
// }
//
// #[derive(PartialEq, Eq, Clone, Hash, Debug)]
// pub struct LocalVariableDeclSpan {
//     pub atomic_span: Span,
//     pub name_span: Span,
// }
//
// id!(pub struct NamedExpr);
//
// #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
// pub struct NamedExprData {
//     pub name: SpannedOptionalWord,
//     pub expr: Expr,
// }
