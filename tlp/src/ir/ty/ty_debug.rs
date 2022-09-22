//! Type data debug printer

use std::fmt;

use salsa::DebugWithDb;

use crate::ir::{body::expr_debug::DebugContext, ty::*};

impl<'db> DebugContext<'db> {
    pub fn types(&self) -> &TypeTable {
        self.proc().type_table(self.db())
    }
}

impl DebugWithDb<DebugContext<'_>> for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        self.data(dcx.db()).fmt(f, dcx)
    }
}

impl DebugWithDb<DebugContext<'_>> for TypeData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        match self {
            Self::Unknown => write!(f, "<unknown>"),
            Self::Stmt => write!(f, "<stmt>"),
            Self::Primitive(x) => write!(f, "{:?}", x),
            Self::Op(x) => write!(f, "{:?}", x),
            Self::Proc(x) => x.fmt(f, dcx),
        }
    }
}

impl DebugWithDb<DebugContext<'_>> for ProcType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        write!(f, "ProcType {{ param_tys: {{ ")?;

        for ty in self.param_tys.iter() {
            ty.fmt(f, dcx)?;
        }

        write!(f, "}}, ret_ty: ")?;
        self.ret_ty.fmt(f, dcx)?;
        write!(f, " }}")?;

        Ok(())
    }
}
