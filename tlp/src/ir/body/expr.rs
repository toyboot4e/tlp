use std::{cmp, hash};

use base::{jar::Word, tbl::id};

use crate::{
    ir::{body::pat::Pat, IrDb},
    syntax::ast::{self, AstToken},
};

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
    Missing,
    Block(Block),
    Let(Let),
    Call(Call),
    Literal(Literal),
    Path(Path),
}

impl ExprData {
    pub fn into_path(self) -> Path {
        match self {
            Self::Path(x) => x,
            _ => panic!("unable to unwrap `ExprData` as path"),
        }
    }
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
    ExprData = Block | Let | Call | Literal | Path;
}

/// Code block of S-expressions
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub struct Block {
    // TODO: statements?
    pub children: Box<[Expr]>,
}

impl Block {
    pub fn iter(&self) -> impl Iterator<Item = &Expr> {
        self.children.iter()
    }
}

impl<'a> IntoIterator for &'a Block {
    type Item = &'a Expr;
    type IntoIter = std::slice::Iter<'a, Expr>;

    fn into_iter(self) -> Self::IntoIter {
        self.children.into_iter()
    }
}

/// Code block of S-expressions
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub struct Let {
    pub pat: Pat,
    pub rhs: Expr,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub struct Call {
    pub path: Expr,
    pub args: Box<[Expr]>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub enum Literal {
    String(String),
    Char(char),
    Bool(bool),
    F32(TotalF32),
    I32(i32),
}

#[derive(PartialEq, PartialOrd, Clone, Debug)]
pub struct TotalF32(pub f32);

impl cmp::Eq for TotalF32 {}

// FIXME: what?
impl cmp::Ord for TotalF32 {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        <f32 as cmp::PartialOrd>::partial_cmp(&self.0, &other.0).unwrap_or(cmp::Ordering::Equal)
    }
}

// FIXME: what?
impl hash::Hash for TotalF32 {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        if self.0.is_nan() {
            0x7fc00000u32.hash(state); // a particular bit representation for NAN
        } else if self.0 == 0.0 {
            // catches both positive and negative zero
            0u32.hash(state);
        } else {
            self.0.to_bits().hash(state);
        }
    }
}

impl Literal {
    pub fn parse(x: ast::Num) -> Result<Self, String> {
        let text = x.syntax().text();

        if let Ok(x) = text.parse::<i32>() {
            return Ok(Self::I32(x));
        }

        if let Ok(x) = text.parse::<f32>() {
            return Ok(Self::F32(TotalF32(x)));
        }

        Err("if not a number".to_string())
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub struct Path {
    pub segments: Box<[Word]>,
}

impl Path {
    pub fn parse(db: &dyn IrDb, ast: ast::Path) -> Self {
        let segments = ast
            .components()
            .map(|c| Word::new(db.as_base_db(), c.text().to_string()))
            .collect();

        Self { segments }
    }
}
