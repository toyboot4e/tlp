use std::{cmp, fmt::Write, hash};

use base::{jar::Word, tbl::id};
use salsa::DebugWithDb;

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprData {
    // TODO: consider removing missing expr
    Missing,
    Block(Block),
    Let(Let),
    Call(Call),
    /// Arithmetic: `+`, `-`, `*`, `/`. Comparison: `=`
    CallOp(CallOp),
    Op(OpKind),
    Literal(Literal),
    Path(Path),
    And(And),
    Or(Or),
    Set(Set),
    When(When),
    Unless(Unless),
    Cond(Cond),
    Loop(Loop),
    While(While),
}

impl ExprData {
    pub fn cast_as_path(&self) -> &Path {
        match self {
            Self::Path(x) => x,
            _ => panic!("unable to unwrap `ExprData` as path"),
        }
    }

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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    // TODO: statements?
    pub exprs: Box<[Expr]>,
}

impl Block {
    pub fn iter(&self) -> impl Iterator<Item = &Expr> {
        self.exprs.iter()
    }
}

impl<'a> IntoIterator for &'a Block {
    type Item = &'a Expr;
    type IntoIter = std::slice::Iter<'a, Expr>;

    fn into_iter(self) -> Self::IntoIter {
        self.exprs.into_iter()
    }
}

/// Code block of S-expressions
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Let {
    pub pat: Pat,
    pub rhs: Expr,
}

/// Procedure call
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Call {
    pub path: Expr,
    pub args: Box<[Expr]>,
}

/// Builtin operator call
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CallOp {
    pub op_expr: Expr,
    pub args: Box<[Expr]>,
}

/// Builtin operator kinds
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpKind {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `=`
    Eq,
    /// `!=`
    NotEq,
    /// `<`
    Lt,
    /// `<=`
    Le,
    /// `>`
    Gt,
    /// `>=`
    Ge,
}

impl OpKind {
    pub fn parse(s: &str) -> Option<Self> {
        let op = match s {
            "+" => Self::Add,
            "-" => Self::Sub,
            "*" => Self::Mul,
            "/" => Self::Div,
            "=" => Self::Eq,
            "=" => Self::Eq,
            "!=" => Self::NotEq,
            "<" => Self::Lt,
            "<=" => Self::Le,
            ">" => Self::Gt,
            ">=" => Self::Ge,
            _ => return None,
        };

        Some(op)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Literal {
    String(String),
    Char(char),
    Bool(bool),
    F32(TotalF32),
    I32(i32),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
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
    pub fn parse_num(x: ast::Num) -> Result<Self, String> {
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Path {
    pub segments: Box<[Word]>,
}

impl DebugWithDb<dyn IrDb> for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &dyn IrDb) -> std::fmt::Result {
        let (first, rest) = match self.segments.split_first() {
            Some((first, rest)) => (first, rest),
            _ => return Ok(()),
        };

        f.write_str(first.as_str(db.base()))?;

        for word in rest.iter() {
            let s = word.as_str(db.base());
            f.write_char('.')?;
            f.write_str(s)?;
        }

        Ok(())
    }
}

impl Path {
    pub fn parse(db: &dyn IrDb, ast: ast::Path) -> Self {
        let segments = ast
            .components()
            .map(|c| Word::new(db.base(), c.text().to_string()))
            .collect();

        Self { segments }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct And {
    pub exprs: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Or {
    pub exprs: Vec<Expr>,
}

/// Set value
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Set {
    pub place: Expr,
    pub rhs: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct When {
    pub pred: Expr,
    pub block: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unless {
    pub pred: Expr,
    pub block: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Cond {
    /// If any of the cond cases has `true` test, it can be an expression
    pub can_be_expr: bool,
    pub cases: Vec<CondCase>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CondCase {
    pub pred: Expr,
    pub block: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Loop {
    pub block: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct While {
    pub pred: Expr,
    pub block: Expr,
}
