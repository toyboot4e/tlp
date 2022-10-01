use std::{cmp, hash};

use base::{jar::Word, tbl::id};

use crate::{
    ir::{body::pat::Pat, ty, IrDb},
    syntax::ast::{self, AstToken},
};

// TODO: consider using `ExprT<T>`

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

crate::util::enum_impl_from! {
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
    // TODO: add type annotation
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
            "!=" => Self::NotEq,
            "<" => Self::Lt,
            "<=" => Self::Le,
            ">" => Self::Gt,
            ">=" => Self::Ge,
            _ => return None,
        };

        Some(op)
    }

    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Eq => "=",
            Self::NotEq => "!=",
            Self::Lt => "<",
            Self::Le => "<=",
            Self::Gt => ">",
            Self::Ge => ">=",
        }
    }

    pub fn is_bool(&self) -> bool {
        matches!(
            self,
            OpKind::Eq | OpKind::NotEq | OpKind::Lt | OpKind::Le | OpKind::Gt | OpKind::Ge
        )
    }

    pub fn is_arithmetic(&self) -> bool {
        !self.is_bool()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    String(String),
    Char(char),
    Bool(bool),
    F32(TotalF32),
    I32(i32),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TotalF32(pub f32);

impl cmp::Eq for TotalF32 {}

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

// TODO: consider interning
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub segments: Box<[Word]>,
}

impl Path {
    pub fn from_ast(db: &dyn IrDb, ast: ast::Path) -> Self {
        Self::from_ast_segments(db, ast.segments())
    }

    pub fn from_ast_segments(
        db: &dyn IrDb,
        segments: impl Iterator<Item = ast::PathSegment>,
    ) -> Self {
        let segments = segments
            .map(|c| Word::new(db.base(), c.text().to_string()))
            .collect();

        Self { segments }
    }
}

/// Unresolved, syntax-level type
// TODO: consider interning
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeSyntax {
    Missing,
    Primitive(ty::PrimitiveType),
    Path(Path),
}

impl TypeSyntax {
    pub fn from_ast(db: &dyn IrDb, ast: ast::Type) -> Self {
        match ast {
            ast::Type::TypePath(type_path) => {
                let path = Path::from_ast(db, type_path.into_path());
                assert_eq!(path.segments.len(), 1, "TODO: path {:?}", path);

                let ident = path.segments[0].as_str(db.base());
                if let Some(prim) = ty::PrimitiveType::parse(ident) {
                    Self::Primitive(prim)
                } else {
                    Self::Path(path)
                }
            }
        }
    }

    pub fn from_opt_ast(db: &dyn IrDb, ast: Option<ast::Type>) -> Self {
        match ast {
            Some(ast) => Self::from_ast(db, ast),
            None => Self::Missing,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct And {
    pub exprs: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Or {
    pub exprs: Vec<Expr>,
}

/// Set value
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Set {
    pub place: Expr,
    pub rhs: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct When {
    pub pred: Expr,
    pub block: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Unless {
    pub pred: Expr,
    pub block: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Cond {
    /// If any of the cond cases has `true` test, it can be an expression
    pub can_be_expr: bool,
    pub cases: Vec<CondCase>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CondCase {
    pub pred: Expr,
    pub block: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Loop {
    pub block: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct While {
    pub pred: Expr,
    pub block: Expr,
}
