/*!
Module item declarations
*/

use std::ops;

use la_arena::{Arena, Idx};
use smol_str::SmolStr;

use crate::{
    ir::db::input::*,
    syntax::{ast::data as ast, cst::data::SyntaxToken},
};

/// Upcast of module item IDs
pub enum ItemDecl {
    Proc(DefProc),
}

/// Simplified AST that only contains top-level items in a module
///
/// In rust-analyzer, `DeclTree` is known as `ItemTree`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DeclTree {
    pub(crate) file: FileId,
    pub(crate) procs: Arena<DefProc>,
    // pub(crate) imports: Vec<Import>,
}

impl ops::Index<Idx<DefProc>> for DeclTree {
    type Output = DefProc;
    fn index(&self, ix: Idx<DefProc>) -> &Self::Output {
        &self.procs[ix]
    }
}

impl DeclTree {
    pub fn new(file: FileId) -> Self {
        Self {
            file,
            procs: Default::default(),
        }
    }

    pub fn procs(&self) -> &Arena<DefProc> {
        &self.procs
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    // TODO: consider preferring salsa?
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Visibility {
    /// Default: can be seen from the module and sub modules
    Module,
    Public,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    name: Name,
    // ty: Type,
}

impl Param {
    pub fn name(&self) -> &Name {
        &self.name
    }
}

/// Function parameters
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProcParams {
    params: Vec<Param>,
    // ast: ast::Params,
}

impl std::ops::Index<usize> for ProcParams {
    type Output = Param;
    fn index(&self, index: usize) -> &Self::Output {
        &self.params[index]
    }
}

impl ProcParams {
    pub fn len(&self) -> usize {
        self.params.len()
    }
}

impl ProcParams {
    pub fn none() -> Self {
        Self { params: Vec::new() }
    }

    pub fn from_ast(ast: ast::Params) -> Self {
        let mut params = Vec::new();

        for tk in ast.param_tks() {
            let text = tk.text();
            params.push(Param {
                name: Name::from_str(text),
            });
        }

        Self { params }
    }
}

/// Procedure definition
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefProc {
    pub(crate) name: Name,
    pub(crate) params: ProcParams,
    // pub vis: Visibility,
    // pub ret_ty: TypeRefId,
    pub(crate) ast: ast::DefProc,
}

impl DefProc {
    pub fn from_ast(ast: ast::DefProc) -> Self {
        let name = Name::from_tk(ast.name_tk());

        let params = match ast.params() {
            Some(ast) => ProcParams::from_ast(ast),
            None => ProcParams::none(),
        };

        Self { name, params, ast }
    }

    pub fn name(&self) -> &Name {
        &self.name
    }

    pub fn params(&self) -> &ProcParams {
        &self.params
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
