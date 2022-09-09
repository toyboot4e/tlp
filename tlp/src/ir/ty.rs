//! Types expressions

// TODO: resolve path to a pattern without making duplicates?

pub mod lower_type;
pub mod typed_body;

use std::ops;

use rustc_hash::FxHashMap;

use crate::ir::body::{expr::Expr, pat::Pat};

// #[salsa::tracked(jar = IrJar)]
// pub struct Ty {
//     pub data: TypeData,
// }

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TypeTable {
    expr_types: FxHashMap<Expr, TypeData>,
    pat_types: FxHashMap<Pat, TypeData>,
}

impl ops::Index<Expr> for TypeTable {
    type Output = TypeData;
    fn index(&self, expr: Expr) -> &Self::Output {
        &self.expr_types[&expr]
    }
}

impl ops::Index<Pat> for TypeTable {
    type Output = TypeData;
    fn index(&self, pat: Pat) -> &Self::Output {
        &self.pat_types[&pat]
    }
}

/// WIP type information
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum WipTypeData {
    /// Type varable to be inferred
    Var,
    Data(TypeData),
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
    /// Builtin bimnary operator type
    Op(OpType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OpType {
    pub kind: OpKind,
    pub target_ty: OpTargetType,
}

/// Builtin operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpTargetType {
    Unknown,
    I32,
    F32,
}

impl OpTargetType {
    pub fn from_wip_type(ty: &WipTypeData) -> Option<Self> {
        match ty {
            WipTypeData::Data(TypeData::Primitive(prim)) => match prim {
                PrimitiveType::I32 => Some(OpTargetType::I32),
                PrimitiveType::F32 => Some(OpTargetType::F32),
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
        };

        Some(WipTypeData::Data(TypeData::Primitive(prim)))
    }
}

/// Builtin operator kinds
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpKind {
    Add,
    Sub,
    Mul,
    Div,
}

impl OpKind {
    pub fn parse(s: &str) -> Option<Self> {
        let op = match s {
            "+" => Self::Add,
            "-" => Self::Sub,
            "*" => Self::Mul,
            "/" => Self::Div,
            _ => return None,
        };

        Some(op)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    I32,
    F32,
    Bool,
}

/// Extentional impls
mod ext {
    use super::*;
    use crate::ir::{item, IrDb};

    impl item::Proc {
        pub fn type_table<'db>(&self, db: &'db dyn IrDb) -> &'db TypeTable {
            lower_type::lower_body_types(db, *self)
        }
    }
}
