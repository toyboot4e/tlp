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
    name: Name,
    params: ProcParams,
    // pub vis: Visibility,
    // pub ret_ty: TypeRefId,
    ast: ast::DefProc,
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
