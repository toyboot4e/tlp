//! Diagonstics

use std::fmt;

use salsa::DebugWithDb;

use crate::{util::diag, ir::{
    body::{expr::Expr, expr_debug::DebugContext},
    item,
    ty::Ty,
    IrJar,
}};

/// Type diagnostics for a procedure
#[salsa::accumulator(jar = IrJar)]
pub struct TypeDiagnostics(TypeDiagnostic);

crate::util::define_enum! {
    /// Type diagnostic for a procedure
    #[derive(Debug, Clone)]
    pub TypeDiagnostic = MissingParamType | TypeMismatch | CantResolve;
}

impl diag::Diagnostic for TypeDiagnostic {
    fn code(&self) -> &str {
        match self {
            TypeDiagnostic::MissingParamType(_) => "E0000",
            TypeDiagnostic::TypeMismatch(_) => "E0001",
            TypeDiagnostic::CantResolve(_) => "E0002",
        }
    }

    fn severity(&self) -> diag::Severity {
        diag::Severity::Error
    }

    fn msg(&self) -> &str {
        match self {
            TypeDiagnostic::MissingParamType(_) => "missing parameter type",
            TypeDiagnostic::TypeMismatch(_) => "type mismatch",
            TypeDiagnostic::CantResolve(_) => "can't resolve path",
        }
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

// /// Type mismatch of branches
// pub struct IncompatibleTypes {

#[derive(Debug, Clone)]
pub struct CantResolve {
    pub expr: Expr,
}

impl TypeDiagnostic {
    pub fn severity(&self) -> diag::Severity {
        diag::Severity::Error
    }

    pub fn msg(&self) -> &str {
        match self {
            TypeDiagnostic::MissingParamType(_) => "missing parameter type",
            TypeDiagnostic::TypeMismatch(_) => "type mismatch",
            TypeDiagnostic::CantResolve(_) => "can't resolve",
        }
    }
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
