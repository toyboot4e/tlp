/*!
Interned representation of source code
*/

use crate::{
    intern::interner::{AccessId, ModuleId, ScopeId},
    syntax::{ast::data as ast, cst::data as cst},
};

/// Represents a crate, but doesn't contain anything in it
///
/// TODO: Intern
#[derive(Debug, Clone)]
pub struct CrateToken {
    pub name: String,
}

/// Represents a module, but doesn't contain anything in it
#[derive(Debug, Clone)]
pub struct ModuleToken {
    pub access: AbsAccess,
}

impl ModuleToken {
    pub fn crate_root(krate: CrateToken) -> Self {
        Self {
            access: AbsAccess::crate_root(krate),
        }
    }
}

/// Locagion of lexical scope in a crate
#[derive(Debug, Clone)]
pub enum Scope {
    /// Just under the crate
    Crate {},
    /// Just under the module
    Module { module: ModuleId },
    LexScope {
        module: ModuleId,
        access: AccessId,
        pos: usize,
    },
}

/// Access to an item from a scope
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RelAccess {
    pub name: cst::SyntaxToken,
}

impl std::hash::Hash for RelAccess {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // TODO: don't allocate string
        self.name.to_string().hash(state);
    }
}

/// Absolute access to an item
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct AbsAccess {
    // TODO: include crate index
}

impl AbsAccess {
    /// TODO: allow any lexical scope
    pub fn new(_scope: ScopeId, _access: &RelAccess) -> Self {
        Self {}
    }

    pub fn crate_root(_kraet: CrateToken) -> Self {
        Self {}
    }
}

/// Function definition
#[derive(Debug, Clone)]
pub struct DefProc {
    pub access: AccessId,
    pub params: Option<ProcParams>,
    pub scope: ScopeId,
    pub ast: ast::DefProc,
}

impl DefProc {
    pub fn new(ast: ast::DefProc, scope: ScopeId, access: AccessId) -> Self {
        Self {
            access,
            params: ast.params().map(|ast| ProcParams::from_ast(ast)),
            scope,
            ast,
        }
    }
}

/// Function parameters
#[derive(Debug, Clone)]
pub struct ProcParams {
    pub ast: ast::Params,
}

impl ProcParams {
    pub fn from_ast(ast: ast::Params) -> Self {
        Self { ast }
    }
}

/// Multiple lexical scopes in a function body
#[derive(Debug, Clone)]
pub struct LexScope {
    depths: Vec<usize>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Call(Call),
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: AccessId,
    pub args: Vec<Arg>,
    pub ast: ast::Call,
}

#[derive(Debug, Clone)]
pub struct Arg {
    /// TODO: lifetime?
    pub name: rowan::SyntaxText,
    // pub ast: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    F32(f32),
}
