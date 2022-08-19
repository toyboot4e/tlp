//! Procecures for creating `hir_def` data types
//!
//! Lowering is about converting AST data into `Arena` s.

mod collect_item_scope;
mod lower_body;
mod lower_item;

pub use self::{collect_item_scope::*, lower_body::*, lower_item::*};

use std::{
    fmt,
    hash::{BuildHasher, BuildHasherDefault, Hash, Hasher},
};

use la_arena::{Arena, Idx};
use rustc_hash::FxHasher;

use crate::{
    hir_def::ids::AstIdx,
    syntax::{
        ast::{self, AstNode},
        cst,
        ptr::{AstPtr, SyntaxNodePtr},
    },
};

/// Map between AST item locations and stable indices
#[derive(Debug, Clone, Default)]
pub struct AstIdMap {
    arena: Arena<SyntaxNodePtr>,
    // `hashbrown` unleashes unstable features in std hashmap
    record: hashbrown::HashMap<Idx<SyntaxNodePtr>, (), ()>,
}

impl PartialEq for AstIdMap {
    fn eq(&self, other: &Self) -> bool {
        self.arena == other.arena
    }
}

impl Eq for AstIdMap {}

impl AstIdMap {
    pub(crate) fn from_source(node: &cst::SyntaxNode) -> AstIdMap {
        assert!(node.parent().is_none());
        let mut res = AstIdMap::default();

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

impl AstIdMap {
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
