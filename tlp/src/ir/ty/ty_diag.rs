//! Diagonstics

use std::fmt;

use salsa::DebugWithDb;

use crate::{
    ir::{
        body::{expr::Expr, expr_debug::DebugContext},
        item,
        ty::Ty,
        IrJar,
    },
    util::diag,
};

/// Type diagnostics for a procedure
#[salsa::accumulator(jar = IrJar)]
pub struct TypeDiagnostics(TypeDiagnostic);

crate::util::define_enum! {
    /// Type diagnostic for a procedure
    #[derive(Debug, Clone)]
    pub TypeDiagnostic = MissingParamType | MismatchedTypes | CannotFindTypeInScope | CannotFindValueInScope;
}

impl diag::Diagnostic for TypeDiagnostic {
    fn code(&self) -> &str {
        match self {
            TypeDiagnostic::MissingParamType(_) => "E0000",
            TypeDiagnostic::MismatchedTypes(_) => "E0001",
            TypeDiagnostic::CannotFindTypeInScope(_) => "E0002",
            TypeDiagnostic::CannotFindValueInScope(_) => "E0003",
        }
    }

    fn severity(&self) -> diag::Severity {
        diag::Severity::Error
    }

    fn msg(&self) -> &str {
        match self {
            TypeDiagnostic::MissingParamType(_) => "missing parameter type",
            TypeDiagnostic::MismatchedTypes(_) => "mismatched types",
            TypeDiagnostic::CannotFindTypeInScope(_) => "cannot find type in scope",
            TypeDiagnostic::CannotFindValueInScope(_) => "cannot find value in scope",
        }
    }

    fn reason(&self) -> &str {
        self.msg()
    }
}

#[derive(Debug, Clone)]
pub struct MissingParamType {
    pub param: item::Param,
}

#[derive(Debug, Clone)]
pub struct MismatchedTypes {
    pub expr: Expr,
    pub expected: Ty,
    pub actual: Ty,
    // why:
}

#[derive(Debug, Clone)]
pub struct OpArgTypeMismatch {
    pub first_expr: Expr,
    pub expr: Expr,
    pub expected_ty: Ty,
    pub actual_ty: Ty,
}

// /// Type mismatch of branches
// pub struct IncompatibleTypes {

#[derive(Debug, Clone)]
pub struct CannotFindTypeInScope {
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct CannotFindValueInScope {
    pub expr: Expr,
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
            Self::MismatchedTypes(x) => {
                write!(
                    f,
                    "MismatchedTypes {{ expr: {:?}, expected: {:?}, actual: {:?} }}",
                    x.expr.debug_with(dcx, all_fields),
                    x.expected.debug_with(dcx.db(), all_fields),
                    x.actual.debug_with(dcx.db(), all_fields),
                )?;
            }
            Self::CannotFindTypeInScope(x) => {
                write!(
                    f,
                    "CantResolveType {{ param: {:?} }}",
                    x.expr.debug_with(dcx, all_fields),
                )?;
            }
            Self::CannotFindValueInScope(x) => {
                write!(
                    f,
                    "CantResolveValue {{ param: {:?} }}",
                    x.expr.debug_with(dcx, all_fields),
                )?;
            }
        }

        write!(f, ")")?;

        Ok(())
    }
}
