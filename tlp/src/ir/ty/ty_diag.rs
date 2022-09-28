//! Diagonstics

use std::fmt;

use salsa::DebugWithDb;

use crate::ir::{
    body::{expr::Expr, expr_debug::DebugContext},
    item,
    ty::Ty,
    IrDb, IrJar,
};

#[salsa::accumulator(jar = IrJar)]
pub struct TypeDiagnostics(TypeDiagnostic);

crate::util::define_enum! {
    #[derive(Debug, Clone)]
    pub TypeDiagnostic = MissingParamType | TypeMismatch | CantResolve;
}

impl DebugWithDb<DebugContext<'_>> for TypeDiagnostic {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        dcx: &DebugContext<'_>,
        all_fields: bool,
    ) -> fmt::Result {
        write!(f, "TypeDiagnostic(")?;

        match self {
            Self::MissingParamType(x) => {
                write!(
                    f,
                    "MissingParamType {{ param: {:?} }}",
                    x.param.debug_with(dcx, all_fields)
                )?;
            }
            Self::TypeMismatch(x) => {
                write!(
                    f,
                    "TypeMismatch {{ expr: {:?}, expected: {:?}, actual: {:?} }}",
                    x.expr.debug_with(dcx, all_fields),
                    x.expected.debug_with(dcx.db(), all_fields),
                    x.actual.debug_with(dcx.db(), all_fields),
                )?;
            }
            Self::CantResolve(x) => {
                write!(
                    f,
                    "CantResolve {{ param: {:?} }}",
                    x.expr.debug_with(dcx, all_fields),
                )?;
            }
        }

        write!(f, ")")?;

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct MissingParamType {
    pub param: item::Param,
}

#[derive(Debug, Clone)]
pub struct TypeMismatch {
    pub expr: Expr,
    pub expected: Ty,
    pub actual: Ty,
    // why:
}

#[derive(Debug, Clone)]
pub struct CantResolve {
    pub expr: Expr,
}
