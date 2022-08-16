//! HIR expression
//!
//! Source locations are interned.

use std::cmp;

use la_arena::Idx;

use crate::{
    hir_def::{
        db::{self, ids::Id},
        item::Name,
        pat,
    },
    syntax::ast::{self, AstToken},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    /// Invalid syntax can contain missing expression
    Missing,
    Block(Block),
    Let(Let),
    Call(Call),
    Literal(Literal),
    Path(Path),
}

macro_rules! impl_from {
    ( $ty:ty = $( $ty_from:ident )|* ; ) => {
        $(
            impl From<$ty_from> for $ty {
                fn from(x: $ty_from) -> $ty {
                    Self::$ty_from(x)
                }
            }
        )*
    }
}

impl_from! {
    Expr = Block | Let | Call | Literal;
}

/// Code block of S-expressions
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    // TODO: statements?
    pub children: Vec<Idx<Expr>>,
    // pub ast_loc_id: Id<AstLoc<ast::Block>>,
}

/// Code block of S-expressions
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Let {
    pub pat: Idx<pat::Pat>,
    pub rhs: Idx<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Call {
    pub path: Idx<Expr>,
    pub args: Box<[Idx<Expr>]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    String(String),
    Char(char),
    Bool(bool),
    Int(i32),
    Float(EqF32),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EqF32(pub f32);

impl cmp::Eq for EqF32 {}

impl From<ast::Num> for Literal {
    fn from(x: ast::Num) -> Self {
        let text = x.syntax().text();

        if let Ok(x) = text.parse::<i32>() {
            return Self::Int(x);
        }

        if let Ok(x) = text.parse::<f32>() {
            return Self::Float(EqF32(x));
        }

        todo!("if not a number");
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    data: Id<PathData>,
}

impl Path {
    pub fn lower(ast: ast::Path, db: &dyn db::Intern) -> Self {
        let data = PathData::parse(ast);
        let data = db.intern_path_data(data);
        Self { data }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PathData {
    segments: Vec<Name>,
}

impl PathData {
    pub fn parse(ast: ast::Path) -> Self {
        let segments = ast
            .components()
            .map(|c| Name::from_str(c.text()))
            .collect::<Vec<_>>();

        Self { segments }
    }
}
