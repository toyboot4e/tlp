use std::fmt::{self, Write};

// TODO: consider `all_fields`

use base::tbl::InternKey;
use salsa::DebugWithDb;

use crate::ir::{
    body::{
        expr::*,
        pat::{Pat, PatData},
        BodyData, BodyTables,
    },
    item, IrDb,
};

impl<T: ?Sized + IrDb> DebugWithDb<T> for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &T, _all_fields: bool) -> fmt::Result {
        let (first, rest) = match self.segments.split_first() {
            Some((first, rest)) => (first, rest),
            _ => return Ok(()),
        };

        f.write_str(first.as_str(db.base()))?;

        for word in rest.iter() {
            let s = word.as_str(db.base());
            f.write_char('.')?;
            f.write_str(s)?;
        }

        Ok(())
    }
}

/// Debug context for procedures
pub struct DebugContext<'db> {
    proc: item::Proc,
    db: &'db dyn IrDb,
}

impl item::Proc {
    /// Creates [`DebugContext`]
    pub fn with_db<'db>(&self, db: &'db dyn IrDb) -> DebugContext<'db> {
        DebugContext { proc: *self, db }
    }
}

impl<'db> DebugContext<'db> {
    pub fn db(&self) -> &'db dyn IrDb {
        self.db
    }

    pub fn proc(&self) -> item::Proc {
        self.proc
    }

    pub fn body_data(&self) -> &BodyData {
        self.proc().body_data(self.db)
    }

    pub fn body_tables(&self) -> &BodyTables {
        &self.proc().body_data(self.db).tables
    }
}

fn fmt_exprs(
    exprs: &[Expr],
    f: &mut fmt::Formatter<'_>,
    dcx: &DebugContext<'_>,
    all_fields: bool,
) -> fmt::Result {
    let (first, exprs) = match exprs.split_first() {
        Some(x) => x,
        None => return Ok(()),
    };

    first
        .data(&dcx.body_data().tables)
        .fmt(f, dcx, all_fields)?;

    for expr in exprs.iter() {
        write!(f, ", ")?;
        expr.data(dcx.body_tables()).fmt(f, dcx, all_fields)?;
    }

    Ok(())
}

fn with_square_brackets(
    f: &mut fmt::Formatter<'_>,
    run: impl FnOnce(&mut fmt::Formatter<'_>) -> fmt::Result,
) -> fmt::Result {
    write!(f, "[")?;
    run(f)?;
    write!(f, "]")?;

    Ok(())
}

impl DebugWithDb<DebugContext<'_>> for Expr {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        dcx: &DebugContext<'_>,
        all_fields: bool,
    ) -> fmt::Result {
        dcx.body_tables()[*self].fmt(f, dcx, all_fields)
    }
}

impl DebugWithDb<DebugContext<'_>> for ExprData {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        dcx: &DebugContext<'_>,
        all_fields: bool,
    ) -> fmt::Result {
        match self {
            Self::Missing => write!(f, "<missing-expr>"),
            Self::Block(x) => x.fmt(f, dcx, all_fields),
            Self::Let(x) => x.fmt(f, dcx, all_fields),
            Self::Call(x) => x.fmt(f, dcx, all_fields),
            Self::CallOp(x) => x.fmt(f, dcx, all_fields),
            Self::Op(x) => x.fmt(f, dcx, all_fields),
            Self::Literal(x) => x.fmt(f, dcx, all_fields),
            Self::Path(x) => x.fmt(f, dcx, all_fields),
            Self::And(x) => x.fmt(f, dcx, all_fields),
            Self::Or(x) => x.fmt(f, dcx, all_fields),
            Self::Set(x) => x.fmt(f, dcx, all_fields),
            Self::When(x) => x.fmt(f, dcx, all_fields),
            Self::Unless(x) => x.fmt(f, dcx, all_fields),
            Self::Cond(x) => x.fmt(f, dcx, all_fields),
            Self::Loop(x) => x.fmt(f, dcx, all_fields),
            Self::While(x) => x.fmt(f, dcx, all_fields),
        }
    }
}

impl DebugWithDb<DebugContext<'_>> for Block {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        dcx: &DebugContext<'_>,
        all_fields: bool,
    ) -> fmt::Result {
        write!(f, "Block [ ")?;
        self::fmt_exprs(&self.exprs, f, dcx, all_fields)?;
        write!(f, " ]")?;
        Ok(())
    }
}

impl DebugWithDb<DebugContext<'_>> for Let {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        dcx: &DebugContext<'_>,
        all_fields: bool,
    ) -> fmt::Result {
        write!(
            f,
            "Let({:?}, {:?})",
            self.pat.debug_with(dcx, all_fields),
            self.rhs.debug_with(dcx, all_fields)
        )
    }
}

impl DebugWithDb<DebugContext<'_>> for Call {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        dcx: &DebugContext<'_>,
        all_fields: bool,
    ) -> fmt::Result {
        write!(f, "Call({:?},", self.path.debug_with(dcx, all_fields))?;
        self::fmt_exprs(&self.args, f, dcx, all_fields)?;
        write!(f, ")")?;
        Ok(())
    }
}

impl DebugWithDb<DebugContext<'_>> for CallOp {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        dcx: &DebugContext<'_>,
        all_fields: bool,
    ) -> fmt::Result {
        write!(f, "CallOp({:?}, ", self.op_expr.debug_with(dcx, all_fields))?;
        self::fmt_exprs(&self.args, f, dcx, all_fields)?;
        write!(f, ")")?;
        Ok(())
    }
}

impl<T> DebugWithDb<T> for OpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, _: &T, _all_fields: bool) -> fmt::Result {
        write!(f, "{:?}", self.to_str())
    }
}

impl<T> DebugWithDb<T> for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, _: &T, _all_fields: bool) -> fmt::Result {
        match self {
            Self::String(x) => write!(f, "{:?}", x),
            Self::Char(x) => write!(f, "{:?}", x),
            Self::Bool(x) => write!(f, "{:?}", x),
            Self::F32(x) => write!(f, "{:?}", x),
            Self::I32(x) => write!(f, "{:?}", x),
        }
    }
}

impl DebugWithDb<DebugContext<'_>> for Path {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        dcx: &DebugContext<'_>,
        all_fields: bool,
    ) -> fmt::Result {
        self.fmt(f, dcx.db(), all_fields)
    }
}

impl DebugWithDb<DebugContext<'_>> for And {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        dcx: &DebugContext<'_>,
        all_fields: bool,
    ) -> fmt::Result {
        write!(f, "And(")?;
        self::with_square_brackets(f, |f| self::fmt_exprs(&self.exprs, f, dcx, all_fields))?;
        write!(f, ")")?;
        Ok(())
    }
}

impl DebugWithDb<DebugContext<'_>> for Or {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        dcx: &DebugContext<'_>,
        all_fields: bool,
    ) -> fmt::Result {
        write!(f, "Or(")?;
        self::with_square_brackets(f, |f| self::fmt_exprs(&self.exprs, f, dcx, all_fields))?;
        write!(f, ")")?;
        Ok(())
    }
}

impl DebugWithDb<DebugContext<'_>> for Set {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        dcx: &DebugContext<'_>,
        all_fields: bool,
    ) -> fmt::Result {
        write!(
            f,
            "Set({:?}, {:?})",
            self.place.debug_with(dcx, all_fields),
            self.rhs.debug_with(dcx, all_fields)
        )
    }
}

impl DebugWithDb<DebugContext<'_>> for When {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        dcx: &DebugContext<'_>,
        all_fields: bool,
    ) -> fmt::Result {
        write!(
            f,
            "When({:?}, {:?})",
            self.pred.debug_with(dcx, all_fields),
            self.block.debug_with(dcx, all_fields)
        )
    }
}

impl DebugWithDb<DebugContext<'_>> for Unless {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        dcx: &DebugContext<'_>,
        all_fields: bool,
    ) -> fmt::Result {
        write!(
            f,
            "Unless({:?}, {:?}",
            self.pred.debug_with(dcx, all_fields),
            self.block.debug_with(dcx, all_fields)
        )
    }
}

impl DebugWithDb<DebugContext<'_>> for Cond {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        dcx: &DebugContext<'_>,
        all_fields: bool,
    ) -> fmt::Result {
        write!(f, "Cond(")?;

        write!(f, "[")?;
        for case in &self.cases {
            case.fmt(f, dcx, all_fields)?;
        }
        write!(f, "]")?;

        write!(f, ")")?;

        Ok(())
    }
}

impl DebugWithDb<DebugContext<'_>> for CondCase {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        dcx: &DebugContext<'_>,
        all_fields: bool,
    ) -> fmt::Result {
        write!(
            f,
            "CondCase({:?}, {:?}",
            self.pred.debug_with(dcx, all_fields),
            self.block.debug_with(dcx, all_fields),
        )
    }
}

impl DebugWithDb<DebugContext<'_>> for Loop {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        dcx: &DebugContext<'_>,
        all_fields: bool,
    ) -> fmt::Result {
        write!(f, "Loop({:?})", self.block.debug_with(dcx, all_fields))
    }
}

impl DebugWithDb<DebugContext<'_>> for While {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        dcx: &DebugContext<'_>,
        all_fields: bool,
    ) -> fmt::Result {
        write!(
            f,
            "While({:?}, {:?})",
            self.pred.debug_with(dcx, all_fields),
            self.block.debug_with(dcx, all_fields)
        )
    }
}

// --------------------------------------------------------------------------------
// Patterns
// --------------------------------------------------------------------------------

impl DebugWithDb<DebugContext<'_>> for Pat {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        dcx: &DebugContext<'_>,
        all_fields: bool,
    ) -> fmt::Result {
        self.data(dcx.body_tables()).fmt(f, dcx, all_fields)
    }
}

impl DebugWithDb<DebugContext<'_>> for PatData {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        dcx: &DebugContext<'_>,
        _all_fields: bool,
    ) -> fmt::Result {
        match self {
            Self::Missing => write!(f, "<missing>"),
            Self::Bind { name } => write!(f, "{:?}", name.as_str(dcx.db().base())),
        }
    }
}

impl DebugWithDb<DebugContext<'_>> for TypeSyntax {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        dcx: &DebugContext<'_>,
        all_fields: bool,
    ) -> fmt::Result {
        match self {
            Self::Missing => write!(f, "<missing-type>"),
            Self::Primitive(prim) => write!(f, "{:?}", prim),
            Self::Path(path) => write!(f, "{:?}", path.debug_with(dcx, all_fields)),
        }
    }
}

impl DebugWithDb<DebugContext<'_>> for item::Param {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
        dcx: &DebugContext<'_>,
        all_fields: bool,
    ) -> fmt::Result {
        write!(
            f,
            "Param {{ pat: {:?}, ty: {:?} }}",
            self.pat.debug_with(dcx, all_fields),
            self.ty.debug_with(dcx, all_fields)
        )
    }
}
