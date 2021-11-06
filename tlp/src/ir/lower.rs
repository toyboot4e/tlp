use std::sync::Arc;

use la_arena::{Arena, Idx};

use crate::{
    ir::{
        data::{
            body::*,
            decl::{self, ItemTree},
            def,
            expr::Expr,
            res::{CrateDefMap, ItemScope, ModuleData},
        },
        db::{
            self,
            ids::{Id, Loc, TreeId},
            vfs::*,
        },
    },
    syntax::ast,
};

use super::data::decl::Visibility;

/// Collects syntax-reflected form of declarations and imports
pub(crate) fn item_tree_query(db: &dyn db::Def, file: FileId) -> Arc<ItemTree> {
    let ast = db.parse(file.clone()).doc.clone();
    Arc::new(ItemTreeCollect::run(file, ast))
}

struct ItemTreeCollect {
    tree: ItemTree,
}

impl ItemTreeCollect {
    fn run(file: FileId, ast: ast::Document) -> ItemTree {
        let mut me = Self {
            tree: ItemTree::new(file),
        };

        me.collect(ast.item_nodes());

        me.tree
    }

    fn collect(&mut self, forms: impl Iterator<Item = ast::Form>) {
        for form in forms {
            match form {
                ast::Form::DefProc(ast_proc) => {
                    let hir_proc = self.lower_proc(ast_proc);
                    self.tree.procs.alloc(hir_proc);
                    continue;
                }
                _ => {}
            }
        }
    }

    fn lower_proc(&mut self, ast: ast::DefProc) -> decl::DefProc {
        decl::DefProc::from_ast(ast)
    }
}

/// Collects tree of modules with `ItemScope`
pub(crate) fn def_map_query(db: &dyn db::Def, krate: FileId) -> Arc<CrateDefMap> {
    let mut modules = Arena::<ModuleData>::new();

    let root_item_tree = db.file_item_tree(krate.clone());
    let tree_id = TreeId::new(krate);
    let root_scope = ModCollector { tree_id }.module_item_scope(db);

    let root = modules.alloc(ModuleData {
        file: krate.clone(),
        vis: Visibility::Public,
        parent: None,
        children: Vec::new(),
        scope: root_scope,
    });

    Arc::new(CrateDefMap { root, modules })
}

struct ModCollector {
    tree_id: TreeId,
}

impl ModCollector {
    /// Visits module items and makes up the scope
    fn module_item_scope(&self, db: &dyn db::Def) -> Arc<ItemScope> {
        let item_tree = self.tree_id.item_tree(db);
        let mut scope = ItemScope::default();

        for (id, proc) in item_tree.procs().iter() {
            let name = proc.name.clone();

            let id = db.intern_proc(Loc {
                tree: self.tree_id,
                item: id,
            });

            let ix = scope.declare_proc(name, id);
        }

        Arc::new(scope)
    }
}

pub(crate) fn proc_data_query(
    db: &dyn db::Def,
    proc_id: Id<Loc<decl::DefProc>>,
) -> Arc<def::ProcData> {
    let proc_loc = proc_id.lookup(db);
    let tree = proc_loc.tree.item_tree(db);
    let proc = &tree[proc_loc.item];

    Arc::new(def::ProcData {
        name: proc.name.clone(),
    })
}

pub(crate) fn proc_body_query(db: &dyn db::Def, proc_id: Id<Loc<decl::DefProc>>) -> Arc<Body> {
    // collect parameters as pattern IDs

    // body = block expr
    let proc_loc = db.lookup_intern_proc(proc_id);
    let tree = proc_loc.tree.item_tree(db);
    let proc = &tree[proc_loc.item];

    LowerExpr {
        db,
        body: Body::default(),
    }
    .lower_proc(proc.ast.clone())
}

/// Proc AST → Proc HIR
struct LowerExpr<'a> {
    db: &'a dyn db::Def,
    /// The HIR procedure body we're building up
    body: Body,
    // /// AST expr ID → HIR expr ID
    // /// HIR expr ID → AST expr ID
}

impl<'a> LowerExpr<'a> {
    pub fn lower_proc(mut self, proc: ast::DefProc) -> Arc<Body> {
        self.lower_proc_params(proc.clone());
        self.lower_proc_body(proc.clone());
        Arc::new(self.body)
    }

    fn lower_proc_params(&mut self, proc: ast::DefProc) {
        if proc.params().is_some() {
            // lower self parameter
            // lower other parameters
        }
    }

    fn lower_proc_body(&mut self, proc: ast::DefProc) {
        // TODO: Consider block modifier (e.g. coroutines)

        for form in proc.body_forms() {
            self.lower_expr(form);
        }
    }

    /// Allocates an expression making up the AST-HIR map. The separation of the source text from
    /// the HIR is helpful to not recompute on syntax changes that do not affect the HIR.
    fn lower_expr(&mut self, form: ast::Form) -> Idx<Expr> {
        // TODO: cast AST node to a syntax pointer in order to make up the AST-HIR map
        match form {
            ast::Form::DefProc(_proc) => todo!("nested procedure"),
            ast::Form::Call(_call) => {
                todo!("function call");
            }
            ast::Form::Atom(atom) => match atom {
                ast::Atom::Literal(lit) => match lit {
                    ast::Literal::Num(x) => self.alloc_expr(Expr::Literal(x.into())),
                    ast::Literal::Str(_str) => {
                        todo!()
                    }
                    ast::Literal::Bool(_bool) => {
                        todo!()
                    }
                },
            },
        }
    }

    /// Allocates an expression making up the AST-HIR map. The separation of the source text from
    /// the HIR is helpful to not recompute on syntax changes that do not affect the HIR.
    fn alloc_expr(&mut self, expr: Expr) -> Idx<Expr> {
        // TODO: make up the AST-HIR map
        self.body.exprs.alloc(expr)
    }
}
