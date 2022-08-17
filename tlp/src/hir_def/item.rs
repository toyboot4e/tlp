//! HIR item syntax lowered into [`ItemList`]
//!
//! [`ItemList`]: crate::hir_def::scope::ItemList
//!
//! Macros are not expanded and imports not are resolved.

use smol_str::SmolStr;

use crate::syntax::ast;

/// Upcast of module item IDs
pub enum ItemId {
    Proc(DefProc),
}

/// Declared item's name
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    // TODO: consider interning string with salsa?
    data: SmolStr,
}

impl Name {
    pub fn from_str(s: &str) -> Self {
        Self { data: s.into() }
    }

    pub fn as_str(&self) -> &str {
        self.data.as_str()
    }

    pub const fn missing() -> Name {
        Name {
            data: SmolStr::new_inline("[missing name]"),
        }
    }
}

/// Function parameter
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub(crate) name: Name,
    // ty: Type,
}

impl Param {
    pub fn name(&self) -> &Name {
        &self.name
    }
}

/// Procedure definition
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefProc {
    pub(crate) name: Option<Name>,
    pub(crate) params: ProcParams,
    // TODO: remove AST completely
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

/// Function parameters
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProcParams {
    params: Vec<Param>,
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

    pub fn iter(&self) -> impl Iterator<Item = &'_ Param> {
        self.params.iter()
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
