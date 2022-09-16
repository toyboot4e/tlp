//! Types of patterns and expressions

// TODO: resolve path to a pattern without making duplicates?

pub mod lower_type;
pub mod typed_body;

use std::ops;

use rustc_hash::FxHashMap;
use typed_index_collections::TiVec;

use crate::ir::body::{
    expr::{self, Expr},
    pat::Pat,
};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TypeTable {
    /// Indices to `types` vector
    expr_types: FxHashMap<Expr, TyIndex>,
    /// Indices to `types` vector
    pat_types: FxHashMap<Pat, TyIndex>,
    /// Type storage. Type information can be share among expressions that poion to the same pattern.
    types: TiVec<TyIndex, TypeData>,
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
    type Output = TypeData;
    fn index(&self, expr: Expr) -> &Self::Output {
        let index = self.expr_types[&expr];
        &self.types[index]
    }
}

impl ops::Index<Pat> for TypeTable {
    type Output = TypeData;
    fn index(&self, pat: Pat) -> &Self::Output {
        let index = self.pat_types[&pat];
        &self.types[index]
    }
}

/// WIP type information
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum WipTypeData {
    /// Unresolved type variable
    Var,
    Data(TypeData),
}

impl WipTypeData {
    pub fn cast_as_data(&self) -> &TypeData {
        match self {
            Self::Var => unreachable!("failed to cast to `TypeData`"),
            Self::Data(data) => data,
        }
    }

    pub fn cast_as_data_mut(&mut self) -> &mut TypeData {
        match self {
            Self::Var => unreachable!("failed to cast to `TypeData`"),
            Self::Data(data) => data,
        }
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
    pub fn from_wip_type(ty: &WipTypeData) -> Option<Self> {
        match ty {
            WipTypeData::Data(TypeData::Primitive(prim)) => match prim {
                PrimitiveType::Bool => Some(OpOperandType::Bool),
                PrimitiveType::I32 => Some(OpOperandType::I32),
                PrimitiveType::F32 => Some(OpOperandType::F32),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn to_wip_type(self) -> Option<WipTypeData> {
        let prim = match self {
            Self::Unknown => return None,
            Self::I32 => PrimitiveType::I32,
            Self::F32 => PrimitiveType::F32,
            Self::Bool => PrimitiveType::Bool,
        };

        Some(WipTypeData::Data(TypeData::Primitive(prim)))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    I32,
    F32,
    Bool,
}
