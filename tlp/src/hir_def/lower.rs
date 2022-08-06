//! Lowers AST into ID-based representation

use std::sync::Arc;

use la_arena::{Arena, Idx};

use crate::{
    hir_def::{
        db::{
            self,
            ids::{Id, Loc, TreeId},
            vfs::*,
        },
        def,
        def::*,
        item::{self, expr::Expr, ItemTree, Visibility},
        res::{CrateDefMap, ItemScope, ModuleData},
    },
    syntax::ast,
};

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
            match form.kind() {
                ast::FormKind::DefProc(ast_proc) => {
                    let hir_proc = self.lower_proc(ast_proc);
                    self.tree.procs.alloc(hir_proc);
                    continue;
                }
                _ => {}
            }
        }
    }

    fn lower_proc(&mut self, ast: ast::DefProc) -> item::DefProc {
        item::DefProc::from_ast(ast)
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
            let name = match &proc.name {
                Some(name) => name.clone(),
                None => continue,
            };

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
    proc_id: Id<Loc<item::DefProc>>,
) -> Arc<def::ProcData> {
    let proc_loc = proc_id.lookup(db);
    let tree = proc_loc.tree.item_tree(db);
    let proc = &tree[proc_loc.item];

    Arc::new(def::ProcData {
        name: proc
            .name
            .clone()
            .unwrap_or_else(|| item::Name::from_str("<no-name-proc>")),
    })
}

pub(crate) fn proc_body_query(db: &dyn db::Def, proc_id: Id<Loc<item::DefProc>>) -> Arc<Body> {
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

// // TODO: give file ID
// struct LowerContext {
//     file_id: FileId,
// }
//
// impl LowerContext {
//     pub fn
// }

impl<'a> LowerExpr<'a> {
    pub fn lower_proc(mut self, proc: ast::DefProc) -> Arc<Body> {
        self.lower_proc_params(proc.clone());
        self.lower_proc_body(proc.clone());
        Arc::new(self.body)
    }

    fn lower_proc_params(&mut self, proc: ast::DefProc) {
        if proc.params().is_some() {
            // TODO: lower self parameter

            // lower other parameters
        }
    }

    fn lower_proc_body(&mut self, proc: ast::DefProc) {
        // TODO: Consider block modifier (e.g. coroutines)

        if let Some(body) = proc.body() {
            for form in body.forms() {
                self.lower_form(form);
            }
        }
    }

    /// Allocates an expression making up the AST-HIR map. The separation of the source text from
    /// the HIR is helpful to not recompute on syntax changes that do not affect the HIR.
    fn lower_form(&mut self, form: ast::Form) -> Idx<Expr> {
        // TODO: cast AST node to a syntax pointer in order to make up the AST-HIR map
        match form.kind() {
            ast::FormKind::DefProc(_proc) => todo!("nested procedure"),
            ast::FormKind::Call(_call) => {
                todo!("function call");
            }
            ast::FormKind::Literal(lit) => match lit.kind() {
                ast::LiteralKind::Num(x) => self.alloc_expr(Expr::Literal(x.into())),
                ast::LiteralKind::Str(_str) => {
                    todo!()
                }
                ast::LiteralKind::True(_) | ast::LiteralKind::False(_) => {
                    todo!()
                }
            },
        }
    }
}

/// # Allocators
impl<'a> LowerExpr<'a> {
    // fn alloc_pat(&mut self, pat: Pattern, ptr: PatPtr) -> Id<Pattern> {
    //     // InFile<T>
    //     let src = self.expander.to_source(ptr);
    //     let id = self.make_pat(pat, Ok(src.clone()));
    //     // InFile<SyntaxPtr<T>> <-> ExprId
    //     self.source_map.pat_map.insert(src, id);
    //     id
    // }

    // fn make_pat(&mut self, pat: Pat, src: Result<PatSource, SyntheticSyntax>) -> PatId {
    //     let id = self.body.pats.alloc(pat);
    //     self.source_map.pat_map_back.insert(id, src);
    //     id
    // }

    /// Allocates an expression making up the AST-HIR map. The separation of the source text from
    /// the HIR is helpful to not recompute on syntax changes that do not affect the HIR.
    fn alloc_expr(&mut self, expr: Expr) -> Idx<Expr> {
        // TODO: make up the AST-HIR map
        self.body.exprs.alloc(expr)
    }
}
