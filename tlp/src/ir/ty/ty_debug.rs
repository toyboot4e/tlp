//! Type data debug print implementations

use std::fmt;

use salsa::DebugWithDb;

use crate::ir::{ty::*, IrDb};

// impl<'db> DebugContext<'db> {
//     pub fn types(&self) -> &TypeTable {
//         self.proc().type_table(self.db())
//     }
// }

impl DebugWithDb<dyn IrDb + '_> for WipTypeData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &dyn IrDb, all_fields: bool) -> fmt::Result {
        match self {
            Self::Var => write!(f, "<ty-var>"),
            Self::Ty(ty) => ty.fmt(f, db, all_fields),
        }
    }
}

impl DebugWithDb<dyn IrDb + '_> for TypeData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &dyn IrDb, all_fields: bool) -> fmt::Result {
        match self {
            Self::Unknown => write!(f, "<unknown>"),
            Self::Stmt => write!(f, "<stmt>"),
            Self::Primitive(x) => write!(f, "{:?}", x),
            Self::Op(x) => write!(f, "{:?}", x),
            Self::Proc(x) => x.fmt(f, db, all_fields),
        }
    }
}

impl DebugWithDb<dyn IrDb + '_> for ProcType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &dyn IrDb, all_fields: bool) -> fmt::Result {
        write!(f, "ProcType {{ param_tys: {{ ")?;

        for ty in self.param_tys.iter() {
            ty.fmt(f, db, all_fields)?;
        }

        write!(f, "}}, ret_ty: ")?;
        self.ret_ty.fmt(f, db, all_fields)?;
        write!(f, " }}")?;

        Ok(())
    }
}
