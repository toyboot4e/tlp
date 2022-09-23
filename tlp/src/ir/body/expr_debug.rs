use std::fmt::{self, Write};

// TODO: add debug format framework

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

// TODO: proper implementation format?
impl<T: ?Sized + IrDb> DebugWithDb<T> for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, db: &T) -> fmt::Result {
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

pub struct WithIrDb<'me, T: ?Sized> {
    this: &'me T,
    db: &'me dyn IrDb,
}

impl<'me, T> WithIrDb<'me, T> {
    pub fn db(&self) -> &'me dyn IrDb {
        self.db
    }
}

impl<'me, T: ?Sized> std::ops::Deref for WithIrDb<'me, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.this
    }
}

impl item::Proc {
    /// Creates [`DebugContext`]
    pub fn with_db<'me>(&'me self, db: &'me dyn IrDb) -> WithIrDb<'me, Self> {
        WithIrDb { this: self, db }
    }
}

/// Debug context for patterns and expressions used with [`salsa::DebugWithDb`]
pub type DebugContext<'body> = WithIrDb<'body, item::Proc>;

impl DebugContext<'_> {
    pub fn proc(&self) -> &item::Proc {
        &self.this
    }

    pub fn body_data(&self) -> &BodyData {
        self.proc().body_data(self.db)
    }

    pub fn body_tables(&self) -> &BodyTables {
        &self.proc().body_data(self.db).tables
    }
}

fn fmt_exprs(exprs: &[Expr], f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
    let (first, exprs) = match exprs.split_first() {
        Some(x) => x,
        None => return Ok(()),
    };

    first.data(&dcx.body_data().tables).fmt(f, dcx)?;

    for expr in exprs.iter() {
        write!(f, ", ")?;
        expr.data(dcx.body_tables()).fmt(f, dcx)?;
    }

    Ok(())
}

fn with_square_brackets(
    f: &mut fmt::Formatter<'_>,
    dcx: &DebugContext,
    run: impl FnOnce(&mut fmt::Formatter<'_>) -> fmt::Result,
) -> fmt::Result {
    write!(f, "[")?;
    run(f)?;
    write!(f, "]")?;

    Ok(())
}

impl DebugWithDb<DebugContext<'_>> for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        dcx.body_tables()[*self].fmt(f, dcx)
    }
}

impl DebugWithDb<DebugContext<'_>> for ExprData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        match self {
            Self::Missing => write!(f, "<missing>"),
            Self::Block(x) => x.fmt(f, dcx),
            Self::Let(x) => x.fmt(f, dcx),
            Self::Call(x) => x.fmt(f, dcx),
            Self::CallOp(x) => x.fmt(f, dcx),
            Self::Op(x) => x.fmt(f, dcx),
            Self::Literal(x) => x.fmt(f, dcx),
            Self::Path(x) => x.fmt(f, dcx),
            Self::And(x) => x.fmt(f, dcx),
            Self::Or(x) => x.fmt(f, dcx),
            Self::Set(x) => x.fmt(f, dcx),
            Self::When(x) => x.fmt(f, dcx),
            Self::Unless(x) => x.fmt(f, dcx),
            Self::Cond(x) => x.fmt(f, dcx),
            Self::Loop(x) => x.fmt(f, dcx),
            Self::While(x) => x.fmt(f, dcx),
        }
    }
}

impl DebugWithDb<DebugContext<'_>> for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        write!(f, "Block [ ")?;
        self::fmt_exprs(&self.exprs, f, dcx)?;
        write!(f, " ]")?;
        Ok(())
    }
}

impl DebugWithDb<DebugContext<'_>> for Let {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        write!(f, "Let {{ ")?;

        self.pat.fmt(f, dcx)?;
        write!(f, ", ")?;
        self.rhs.fmt(f, dcx)?;

        write!(f, " }}")?;
        Ok(())
    }
}

impl DebugWithDb<DebugContext<'_>> for Call {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        write!(f, "Call(")?;

        self.path.fmt(f, dcx)?;
        write!(f, ", ")?;
        self::fmt_exprs(&self.args, f, dcx)?;

        write!(f, ")")?;
        Ok(())
    }
}

impl DebugWithDb<DebugContext<'_>> for CallOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        write!(f, "CallOp(")?;

        self.op_expr.fmt(f, dcx)?;
        write!(f, ", ")?;
        self::fmt_exprs(&self.args, f, dcx)?;

        write!(f, ")")?;
        Ok(())
    }
}

impl DebugWithDb<DebugContext<'_>> for OpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        write!(f, "{:?}", self.to_str())
    }
}

impl DebugWithDb<DebugContext<'_>> for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        self.fmt(f, dcx.db())
    }
}

impl DebugWithDb<DebugContext<'_>> for And {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        write!(f, "And(")?;
        self::with_square_brackets(f, dcx, |f| self::fmt_exprs(&self.exprs, f, dcx))?;
        write!(f, ")")?;
        Ok(())
    }
}

impl DebugWithDb<DebugContext<'_>> for Or {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        write!(f, "Or(")?;
        self::with_square_brackets(f, dcx, |f| self::fmt_exprs(&self.exprs, f, dcx))?;
        write!(f, ")")?;
        Ok(())
    }
}

impl DebugWithDb<DebugContext<'_>> for Set {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        write!(f, "Set(")?;
        self.place.fmt(f, dcx)?;
        write!(f, ", ")?;
        self.rhs.fmt(f, dcx)?;
        write!(f, ")")?;

        Ok(())
    }
}

impl DebugWithDb<DebugContext<'_>> for When {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        write!(f, "When(")?;
        self.pred.fmt(f, dcx)?;
        write!(f, ", ")?;
        self.block.fmt(f, dcx)?;
        write!(f, ")")?;

        Ok(())
    }
}

impl DebugWithDb<DebugContext<'_>> for Unless {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        write!(f, "Unless(")?;
        self.pred.fmt(f, dcx)?;
        write!(f, ", ")?;
        self.block.fmt(f, dcx)?;
        write!(f, ")")?;

        Ok(())
    }
}

impl DebugWithDb<DebugContext<'_>> for Cond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        write!(f, "Cond(")?;

        write!(f, "[")?;
        for case in &self.cases {
            case.fmt(f, dcx)?;
        }
        write!(f, "]")?;

        write!(f, ")")?;

        Ok(())
    }
}

impl DebugWithDb<DebugContext<'_>> for CondCase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        write!(f, "CondCase(")?;
        self.pred.fmt(f, dcx)?;
        write!(f, ", ")?;
        self.block.fmt(f, dcx)?;
        write!(f, ")")?;

        Ok(())
    }
}

impl DebugWithDb<DebugContext<'_>> for Loop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        write!(f, "Loop(")?;
        self.block.fmt(f, dcx)?;
        write!(f, ")")?;

        Ok(())
    }
}

impl DebugWithDb<DebugContext<'_>> for While {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        write!(f, "While(")?;

        self.pred.fmt(f, dcx)?;
        write!(f, ", ")?;
        self.block.fmt(f, dcx)?;

        write!(f, ")")?;

        Ok(())
    }
}

// --------------------------------------------------------------------------------
// Patterns
// --------------------------------------------------------------------------------

impl DebugWithDb<DebugContext<'_>> for Pat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        self.data(dcx.body_tables()).fmt(f, dcx)
    }
}

impl DebugWithDb<DebugContext<'_>> for PatData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, dcx: &DebugContext<'_>) -> fmt::Result {
        match self {
            Self::Missing => write!(f, "<missing>"),
            Self::Bind { name } => write!(f, "{:?}", name.as_str(dcx.db().base())),
        }
    }
}
