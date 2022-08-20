//! HIR expressions in [`Body`]
//!
//! [`Body`]: crate::hir_def::body::Body

use std::cmp;

use la_arena::Idx;

use crate::{
    hir_def::{
        body::pat,
        db,
        ids::{self, Name},
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
    Expr = Block | Let | Call | Literal | Path;
}

/// Code block of S-expressions
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub id: ids::AstExprLocId<ast::Block>,
    // TODO: statements?
    pub children: Box<[Idx<Expr>]>,
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
    F32(EqF32),
    I32(i32),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EqF32(pub f32);

impl cmp::Eq for EqF32 {}

impl Literal {
    pub fn parse(x: ast::Num) -> Result<Self, String> {
        let text = x.syntax().text();

        if let Ok(x) = text.parse::<i32>() {
            return Ok(Self::I32(x));
        }

        if let Ok(x) = text.parse::<f32>() {
            return Ok(Self::F32(EqF32(x)));
        }

        Err("if not a number".to_string())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    data: ids::Id<PathData>,
}

impl Path {
    pub fn lower(ast: ast::Path, db: &dyn db::Intern) -> Self {
        let data = PathData::parse(ast);
        let data = db.intern_path_data(data);
        Self { data }
    }

    pub fn lookup(&self, db: &dyn db::Intern) -> PathData {
        db.lookup_intern_path_data(self.data)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PathData {
    pub segments: Box<[Name]>,
}

impl PathData {
    pub fn parse(ast: ast::Path) -> Self {
        let segments = ast.components().map(|c| Name::from_str(c.text())).collect();

        Self { segments }
    }
}
