/*!
Definitions
*/

// TODO: Replace access types with ItemLoc<Self>

use crate::{ir::db::ids::AccessId, syntax::ast::data as ast};
use smol_str::SmolStr;

/// Interned string that represents name of something in HIR
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    data: SmolStr,
}

impl Name {
    pub fn from_str(s: &str) -> Self {
        Self { data: s.into() }
    }
}

/// Visibility of an item
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Visibility {
    /// The default
    ModuleOnly,
    Public,
}

/// Function definition
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefProc {
    // FIXME:
    pub access: AccessId,
    // pub name: crate::syntax::grammar::IdentString,
    pub params: Option<ProcParams>,
    // pub vis: Visibility,
    // pub ret_ty: TypeRefId,
    ast: ast::DefProc,
}

impl DefProc {
    pub fn new(ast: ast::DefProc, access: AccessId) -> Self {
        Self {
            access,
            params: ast.params().map(|ast| ProcParams::from_ast(ast)),
            ast,
        }
    }
}

/// Function parameters
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProcParams {
    pub ast: ast::Params,
}

impl ProcParams {
    pub fn from_ast(ast: ast::Params) -> Self {
        Self { ast }
    }
}

/// Code block
pub struct Block {
    // pub scope: LexScope,
    pub ast: ast::Block,
}

// /// Recursive lex scope
// #[derive(Debug, Clone)]
// pub struct LexScope {
//     pub exprs: Vec<Expr>,
//     pub depth: usize,
// }
