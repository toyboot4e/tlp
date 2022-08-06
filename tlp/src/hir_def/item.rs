//! Module item declarations. Imports are just listed and not resolved

pub mod path;
pub mod expr;

use std::ops;

use la_arena::{Arena, Idx};
use smol_str::SmolStr;

use crate::{hir_def::db::vfs::*, syntax::ast};

/// Upcast of module item IDs
pub enum ItemDecl {
    Proc(DefProc),
}

/// Simplified AST that only contains top-level items in a module
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemTree {
    pub(crate) file: FileId,
    pub(crate) procs: Arena<DefProc>,
    // pub(crate) imports: Vec<Import>,
}

impl ops::Index<Idx<DefProc>> for ItemTree {
    type Output = DefProc;
    fn index(&self, ix: Idx<DefProc>) -> &Self::Output {
        &self.procs[ix]
    }
}

impl ItemTree {
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
    // hygiene info?
}

impl Name {
    pub fn from_str(s: &str) -> Self {
        Self { data: s.into() }
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

        for param in ast.param_nodes() {
            // assuming that parameter = identifier
            let tk = param.token();
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
    pub(crate) name: Option<Name>,
    pub(crate) params: ProcParams,
    // pub vis: Visibility,
    // pub ret_ty: TypeRefId,
    pub(crate) ast: ast::DefProc,
}

impl DefProc {
    pub fn from_ast(ast: ast::DefProc) -> Self {
        let name = ast.name().map(|name| Name::from_str(name.token().text()));

        let params = match ast.params() {
            Some(ast) => ProcParams::from_ast(ast),
            None => ProcParams::none(),
        };

        Self { name, params, ast }
    }

    pub fn name(&self) -> Option<&Name> {
        self.name.as_ref()
    }

    pub fn params(&self) -> &ProcParams {
        &self.params
    }
}

// DefStruct

// /// Recursive lex scope
// #[derive(Debug, Clone)]
// pub struct LexScope {
//     pub exprs: Vec<Expr>,
//     pub depth: usize,
// }
