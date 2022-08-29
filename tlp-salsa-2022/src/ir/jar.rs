//! Jars

use salsa::DebugWithDb;

use crate::{
    base::{
        jar::{InputFile, Word},
        span::Span,
        tbl::{id, origin_table::origin_table, tables, InternKey},
    },
    ir::{item::Item, IrDb, IrJar},
};

pub struct InIrDb<'me, T: ?Sized> {
    this: &'me T,
    db: &'me dyn IrDb,
}

impl<'me, T> InIrDb<'me, T> {
    pub fn db(&self) -> &'me dyn IrDb {
        self.db
    }
}

impl<'me, T: ?Sized> std::ops::Deref for InIrDb<'me, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.this
    }
}

pub trait InIrDbExt {
    fn in_ir_db<'me>(&'me self, db: &'me dyn IrDb) -> InIrDb<'me, Self>;
}

impl<T: ?Sized> InIrDbExt for T {
    fn in_ir_db<'me>(&'me self, db: &'me dyn IrDb) -> InIrDb<'me, Self> {
        InIrDb { this: self, db }
    }
}

/// File parsed into items
#[salsa::tracked(jar = IrJar)]
pub struct ParsedFile {
    #[id]
    input_file: InputFile,

    /// The items found in the file.
    #[return_ref]
    items: Vec<Item>,
}

#[salsa::tracked(jar = IrJar)]
pub struct Body {
    #[return_ref]
    data: BodyData,
    #[return_ref]
    spans: BodySpans,
}

impl DebugWithDb<dyn IrDb> for Body {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &dyn IrDb) -> std::fmt::Result {
        f.debug_struct("syntax::Tree")
            .field("info", &self.data(db).debug(&self.in_ir_db(db)))
            .finish()
    }
}

impl InIrDb<'_, Body> {
    fn tables(&self) -> &BodyTables {
        &self.data(self.db()).tables
    }
}

/// Body AST
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BodyData {
    /// Interning tables for expressions and the like.
    pub tables: BodyTables,

    // /// Parameter declarations
    // pub parameter_decls: Vec<LocalVariableDecl>,
    /// The root block expression
    pub root_expr: Expr,
}

impl DebugWithDb<InIrDb<'_, Body>> for BodyData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &InIrDb<'_, Body>) -> std::fmt::Result {
        f.debug_struct("syntax::Tree")
            .field("root_expr", &self.root_expr.debug(db)) // FIXME
            .finish()
    }
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

id!(pub struct Expr);

impl DebugWithDb<InIrDb<'_, Body>> for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &InIrDb<'_, Body>) -> std::fmt::Result {
        f.debug_tuple(&format!("{self:?}"))
            .field(&self.data(db.tables()).debug(db))
            .finish()
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub enum ExprData {
    Id(Word),
    /// parse or other error
    Error,
}

impl DebugWithDb<InIrDb<'_, Body>> for ExprData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &InIrDb<'_, Body>) -> std::fmt::Result {
        match self {
            ExprData::Id(w) => f
                .debug_tuple("Id")
                .field(&w.debug(db.db().as_base_db()))
                .finish(),
            ExprData::Error => f.debug_tuple("Error").finish(),
        }
    }
}

// id!(pub struct LocalVariableDecl);
//
// impl DebugWithDb<InIrDb<'_, Tree>> for LocalVariableDecl {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &InIrDb<'_, Tree>) -> std::fmt::Result {
//         DebugWithDb::fmt(self.data(db.tables()), f, db)
//     }
// }
//
// #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
// pub struct LocalVariableDeclData {
//     pub atomic: Atomic,
//     pub name: Word,
//     pub ty: Option<crate::ty::Ty>,
// }
//
// impl DebugWithDb<InIrDb<'_, Tree>> for LocalVariableDeclData {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &InIrDb<'_, Tree>) -> std::fmt::Result {
//         f.debug_struct("LocalVariableDeclData")
//             .field("atomic", &self.atomic)
//             .field("name", &self.name.debug(db.db()))
//             .field("ty", &self.ty.debug(db.db()))
//             .finish()
//     }
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
// impl DebugWithDb<InIrDb<'_, Tree>> for NamedExpr {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &InIrDb<'_, Tree>) -> std::fmt::Result {
//         DebugWithDb::fmt(self.data(db.tables()), f, db)
//     }
// }
//
// #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
// pub struct NamedExprData {
//     pub name: SpannedOptionalWord,
//     pub expr: Expr,
// }
//
// impl DebugWithDb<InIrDb<'_, Tree>> for NamedExprData {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &InIrDb<'_, Tree>) -> std::fmt::Result {
//         f.debug_tuple(&format!("{:?}", self.name.word(db.db()).debug(db.db())))
//             .field(&self.expr.debug(db))
//             .finish()
//     }
// }
