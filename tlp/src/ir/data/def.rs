/*!
Item definitions
*/

// TODO: Replace access types with ItemLoc<Self>

use crate::syntax::{ast::data as ast, cst::data::SyntaxToken};
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

    pub fn from_tk(syn: SyntaxToken) -> Self {
        Self {
            data: SmolStr::from(syn.text()),
        }
    }

    pub fn as_str(&self) -> &str {
        self.data.as_str()
    }
}

// /// Visibility of an item
// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
// pub enum Visibility {
//     /// The default
//     ModuleOnly,
//     Public,
// }

/// Function parameters
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProcParams {
    pub ast: ast::Params,
}

/// Procedure definition
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefProc {
    name: Name,
    params: Option<ProcParams>,
    // pub vis: Visibility,
    // pub ret_ty: TypeRefId,
    ast: ast::DefProc,
}

impl DefProc {
    pub fn new(ast: ast::DefProc) -> Self {
        let name = Name::from_tk(ast.name_tk());
        // TODO: Non-Option type
        let params = ast.params().map(|ast| ProcParams::from_ast(ast));

        Self { name, params, ast }
    }

    pub fn name(&self) -> &Name {
        &self.name
    }
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

// DefStruct

// /// Recursive lex scope
// #[derive(Debug, Clone)]
// pub struct LexScope {
//     pub exprs: Vec<Expr>,
//     pub depth: usize,
// }
