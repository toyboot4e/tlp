//! Item definition body (expressions and patterns) lowered from AST
//!
//! # Incremental compilation
//!
//! HIR uses positional index instead of source syntax location. It works great on source change
//! that do not reorder items/expressions.
//!
//! # Source map pattern
//!
//! Those data structure that provide the positional indexes are called "source maps".

pub mod expr;
pub mod expr_scope;
pub mod pat;

use std::{
    fmt,
    hash::{BuildHasher, BuildHasherDefault, Hash, Hasher},
};

use la_arena::{Arena, ArenaMap, Idx};
use rustc_hash::{FxHashMap, FxHasher};

use crate::{
    hir_def::ids::AstIdx,
    syntax::{
        ast::{self, AstNode},
        cst,
        ptr::{AstPtr, SyntaxNodePtr},
    },
};

use self::{expr::Expr, pat::Pat};

/// Map between AST item locations and HIR stable indices
#[derive(Debug, Clone, Default)]
pub struct ItemSourceMap {
    arena: Arena<SyntaxNodePtr>,
    // `hashbrown` unleashes unstable features in std hashmap
    record: hashbrown::HashMap<Idx<SyntaxNodePtr>, (), ()>,
}

impl PartialEq for ItemSourceMap {
    fn eq(&self, other: &Self) -> bool {
        self.arena == other.arena
    }
}

impl Eq for ItemSourceMap {}

impl ItemSourceMap {
    pub(crate) fn from_source(node: &cst::SyntaxNode) -> ItemSourceMap {
        assert!(node.parent().is_none());
        let mut res = ItemSourceMap::default();

        // By walking the tree in breadth-first order we make sure that parents
        // get lower ids then children. That is, adding a new child does not
        // change parent's id. This means that, say, adding a new function to a
        // trait does not change ids of top-level items, which helps caching.
        bdfs(node, |it| {
            let kind = it.kind();
            if ast::Item::can_cast(kind) || ast::Block::can_cast(kind) {
                res.alloc(&it);
                true
            } else {
                false
            }
        });

        res.record = hashbrown::HashMap::with_capacity_and_hasher(res.arena.len(), ());

        for (idx, ptr) in res.arena.iter() {
            let hash = hash_ptr(ptr);
            match res
                .record
                .raw_entry_mut()
                .from_hash(hash, |idx2| *idx2 == idx)
            {
                hashbrown::hash_map::RawEntryMut::Occupied(_) => unreachable!(),
                hashbrown::hash_map::RawEntryMut::Vacant(entry) => {
                    entry.insert_with_hasher(hash, idx, (), |&idx| hash_ptr(&res.arena[idx]));
                }
            }
        }

        res
    }

    fn alloc(&mut self, item: &cst::SyntaxNode) -> Idx<SyntaxNodePtr> {
        self.arena.alloc(SyntaxNodePtr::new(item))
    }
}

/// Walks the subtree in bdfs order, calling `f` for each node. What is bdfs
/// order? It is a mix of breadth-first and depth first orders. Nodes for which
/// `f` returns true are visited breadth-first, all the other nodes are explored
/// depth-first.
///
/// In other words, the size of the bfs queue is bound by the number of "true"
/// nodes.
fn bdfs(node: &cst::SyntaxNode, mut f: impl FnMut(cst::SyntaxNode) -> bool) {
    let mut curr_layer = vec![node.clone()];
    let mut next_layer = vec![];
    while !curr_layer.is_empty() {
        curr_layer.drain(..).for_each(|node| {
            let mut preorder = node.preorder();
            while let Some(event) = preorder.next() {
                match event {
                    rowan::WalkEvent::Enter(node) => {
                        if f(node.clone()) {
                            next_layer.extend(node.children());
                            preorder.skip_subtree();
                        }
                    }
                    rowan::WalkEvent::Leave(_) => {}
                }
            }
        });
        std::mem::swap(&mut curr_layer, &mut next_layer);
    }
}

impl ItemSourceMap {
    /// Maps syntax pointer to stable index
    pub fn ptr_to_idx<N: AstNode + fmt::Debug>(&self, node: &N) -> AstIdx<N> {
        let ptr = SyntaxNodePtr::new(node.syntax());
        let hash = hash_ptr(&ptr);

        let raw_idx = match self
            .record
            .raw_entry()
            .from_hash(hash, |&idx| self.arena[idx] == ptr)
        {
            Some((&idx, &())) => idx,
            None => panic!(
                "Can't find {:?} in {}:\n{:?}",
                node,
                std::any::type_name::<Self>(),
                self.arena.iter().map(|(_id, i)| i).collect::<Vec<_>>(),
            ),
        };

        AstIdx::new(raw_idx)
    }

    /// Maps index back to syntax pointer
    pub fn idx_to_ptr<N: AstNode>(&self, id: AstIdx<N>) -> AstPtr<N> {
        // TODO: read source code
        AstPtr::try_from_raw(self.arena[id.raw].clone()).unwrap()
    }
}

fn hash_ptr(ptr: &SyntaxNodePtr) -> u64 {
    let mut hasher = BuildHasherDefault::<FxHasher>::default().build_hasher();
    ptr.hash(&mut hasher);
    hasher.finish()
}

/// Body
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Body {
    pub root_block: Idx<expr::Expr>,
    pub exprs: Arena<expr::Expr>,
    pub pats: Arena<pat::Pat>,
}

impl Body {
    pub fn root_block(&self) -> &expr::Block {
        match &self.exprs[self.root_block] {
            expr::Expr::Block(seq) => seq,
            _ => unreachable!(),
        }
    }
}

/// Expr variant getters
impl Body {
    pub fn get_path(&self, path: Idx<expr::Expr>) -> &expr::Path {
        match &self.exprs[path] {
            Expr::Path(path) => path,
            _ => unreachable!(),
        }
    }
}

/// Map between HIR expression / pattern `Idx` and AST pointers
#[derive(Default, Debug, Eq, PartialEq)]
pub struct BodySourceMap {
    pub expr_ast_hir: FxHashMap<AstPtr<ast::Expr>, Idx<Expr>>,
    pub expr_hir_ast: ArenaMap<Idx<Expr>, ToAst<ast::Expr>>,

    pub pat_ast_hir: FxHashMap<AstPtr<ast::Pat>, Idx<Pat>>,
    pub pat_hir_ast: ArenaMap<Idx<Pat>, ToAst<ast::Pat>>,
    // /// Diagnostics accumulated during body lowering. These contain `AstPtr`s and so are stored in
    // /// the source map (since they're just as volatile).
    // pub(crate) diagnostics: Vec<BodyDiagnostic>,
}

/// Syntax pointer to an existing AST node or missing
pub type ToAst<Ast> = Result<AstPtr<Ast>, SyntheticSyntax>;

/// Represents missing syntax in AST
#[derive(Default, Debug, Eq, PartialEq, Clone, Copy)]
pub struct SyntheticSyntax;
