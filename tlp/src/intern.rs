/*!
Interned view of AST
*/

mod data;
mod interner;
mod tree;

use thiserror::Error;

use crate::{syntax::ast::data as ast, utils::arena::Idx};

use self::{data::*, interner::*, tree::*};

#[derive(Debug, Clone)]
pub struct CrateData {
    pub intern: CrateIntern,
    pub tree: CrateTree,
}

impl CrateData {
    pub fn new(krate: CrateToken) -> Self {
        let mut intern = CrateIntern::new(krate.clone(), ModuleToken::crate_root(krate.clone()));
        let root = intern.alloc_crate_root_module(krate.clone());

        Self {
            intern,
            tree: CrateTree::new(krate.clone(), root),
        }
    }
}

#[derive(Debug, Clone, Error)]
pub enum InternError {
    #[error("Duplicate function definition: {s}")]
    DupFn { access: AbsAccess, s: String },
}

/// FIXME:
pub fn intern(doc: ast::Document) -> (CrateData, Vec<InternError>) {
    let krate = CrateToken {
        name: "implicit-placeholder-crate".to_string(),
    };

    let mut interner = CrateInterner::new(krate.clone());

    {
        let access = AbsAccess {};
        interner.intern_module(access, doc);
    }

    (interner.data, interner.errs)
}

#[derive(Debug, Clone)]
struct CrateInterner {
    token: CrateToken,
    data: CrateData,
    errs: Vec<InternError>,
}

impl CrateInterner {
    pub fn new(tk: CrateToken) -> Self {
        Self {
            token: tk.clone(),
            data: CrateData::new(tk.clone()),
            errs: vec![],
        }
    }

    pub fn intern_module(&mut self, access: AbsAccess, doc: ast::Document) {
        let module = self.data.intern.modules.alloc(ModuleToken { access });
        let mut interner = ModuleIntern::new(self, self.token.clone(), module);
        interner.intern(doc);
    }
}

fn intern_proc(ast: ast::DefProc, scope: ScopeId, icx: &mut CrateInterner) -> Option<Idx<DefProc>> {
    let data = &mut icx.data;

    let access = {
        let rel = RelAccess {
            name: ast.name_tk(),
        };
        AbsAccess::new(scope, &rel)
    };

    let access_id = {
        if data.intern.procs.contains(&access) {
            icx.errs.push(InternError::DupFn {
                access,
                s: ast.name_tk().text().to_string(),
            });
            return None;
        }

        data.intern.access.intern(access.clone())
    };

    let proc = DefProc::new(ast, scope, access_id);
    let idx = data.intern.procs.intern(proc, access).unwrap();

    Some(idx)
}

struct ModuleIntern<'i> {
    icx: &'i mut CrateInterner,
    tree: Idx<ModuleTree>,
    scope: ScopeId,
}

impl<'i> ModuleIntern<'i> {
    pub fn new(icx: &'i mut CrateInterner, krate: CrateToken, module: Idx<ModuleToken>) -> Self {
        let tree = icx.data.tree.insert_module(krate, module);
        let scope = icx.data.intern.scopes.alloc(Scope::Module { module });
        Self { icx, tree, scope }
    }

    fn intern(&mut self, ast: ast::Document) {
        self.intern_procs(ast.item_nodes(), self.scope);
    }

    fn intern_procs(&mut self, forms: impl Iterator<Item = ast::Form>, scope: ScopeId) {
        for form in forms {
            if let Some(proc) = form.as_proc() {
                if let Some(idx) = self::intern_proc(proc, scope, self.icx) {
                    self.icx.data.tree.procs.push(idx);
                }
            }
        }
    }
}
