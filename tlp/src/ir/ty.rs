//! Types of patterns and expressions

// TODO: resolve path to a pattern without making duplicates?

pub mod lower_type;
pub mod ty_debug;

use std::ops;

use rustc_hash::FxHashMap;
use typed_index_collections::TiVec;

use crate::ir::{
    body::{
        expr::{self, Expr},
        pat::Pat,
    },
    IrDb, IrJar,
};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TypeTable {
    /// Indices to `types` vector
    expr_types: FxHashMap<Expr, TyIndex>,
    /// Indices to `types` vector
    pat_types: FxHashMap<Pat, TyIndex>,
    /// Type storage. Type information can be share among expressions that poion to the same pattern.
    types: TiVec<TyIndex, Ty>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TyIndex(usize);

impl From<usize> for TyIndex {
    fn from(raw: usize) -> Self {
        Self(raw)
    }
}

impl From<TyIndex> for usize {
    fn from(index: TyIndex) -> Self {
        index.0
    }
}

impl ops::Index<Expr> for TypeTable {
    type Output = Ty;
    fn index(&self, expr: Expr) -> &Self::Output {
        let index = self.expr_types[&expr];
        &self.types[index]
    }
}

impl ops::Index<Pat> for TypeTable {
    type Output = Ty;
    fn index(&self, pat: Pat) -> &Self::Output {
        let index = self.pat_types[&pat];
        &self.types[index]
    }
}

/// WIP mutable type information while lowering
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum WipTypeData {
    /// Unresolved type variable
    Var,
    /// Resolved type (it can be concluded as `Unknown` though)
    Ty(Ty),
}

impl WipTypeData {
    pub fn cast_as_data<'a>(&self, db: &'a dyn IrDb) -> &'a TypeData {
        match self {
            Self::Var => unreachable!("failed to cast to `TypeData`"),
            Self::Ty(ty) => ty.data(db),
        }
    }
}

/// Interned [`TypeData`]
#[salsa::interned(jar = IrJar)]
pub struct Ty {
    #[return_ref]
    pub data: TypeData,
}

impl Ty {
    pub fn intern(db: &dyn IrDb, data: TypeData) -> Self {
        Self::new(db, data)
    }
}

/// Best-effor type information
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeData {
    /// Unknown or diverging
    Unknown,
    /// Statement type that is just discarded
    Stmt,
    /// Primitive type
    Primitive(PrimitiveType),
    /// Builtin operator (function) type
    Op(OpType),
    /// Procedure type
    Proc(ProcType),
}

// Builtin operator (function) type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OpType {
    pub kind: expr::OpKind,
    pub operand_ty: OpOperandType,
}

/// Builtin operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpOperandType {
    Unknown,
    I32,
    F32,
    Bool,
}

impl OpOperandType {
    pub fn from_type_data(ty_data: &TypeData) -> Option<Self> {
        match ty_data {
            TypeData::Primitive(prim) => match prim {
                PrimitiveType::Bool => Some(OpOperandType::Bool),
                PrimitiveType::I32 => Some(OpOperandType::I32),
                PrimitiveType::F32 => Some(OpOperandType::F32),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn to_type_data(self) -> Option<TypeData> {
        let prim = match self {
            Self::Unknown => return None,
            Self::I32 => PrimitiveType::I32,
            Self::F32 => PrimitiveType::F32,
            Self::Bool => PrimitiveType::Bool,
        };

        Some(TypeData::Primitive(prim))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    I32,
    F32,
    Bool,
}

impl PrimitiveType {
    pub fn parse(s: &str) -> Option<Self> {
        let ty = match s {
            "i32" => Self::I32,
            "f32" => Self::F32,
            "bool" => Self::Bool,
            _ => return None,
        };

        Some(ty)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProcType {
    pub param_tys: Box<[Ty]>,
    pub ret_ty: Ty,
}
