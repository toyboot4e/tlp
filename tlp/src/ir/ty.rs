//! Types expressions

use crate::ir::body::{expr::Expr, pat::Pat};

pub mod lower_type;
pub mod typed_body;

// #[salsa::tracked(jar = IrJar)]
// pub struct Ty {
//     pub data: TypeData,
// }

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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
}

/// Builtin operator kinds
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    I32,
    F32,
    Bool,
}
