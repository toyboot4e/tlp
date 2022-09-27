//! Syntax pointers

use std::{
    cmp::{Eq, PartialEq},
    fmt,
    hash::{Hash, Hasher},
    iter,
    marker::PhantomData,
};

use rowan::TextRange;

use crate::syntax::{
    ast::AstNode,
    cst::{SyntaxKind, SyntaxNode},
};

/// A "pointer" to a [`SyntaxNode`], via location in the source code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SyntaxNodePtr {
    kind: SyntaxKind,
    range: TextRange,
}

impl SyntaxNodePtr {
    /// Returns a [`SyntaxNodePtr`] for the node.
    pub fn new(node: &SyntaxNode) -> Self {
        Self {
            kind: node.kind(),
            range: node.text_range(),
        }
    }

    pub fn from_range(kind: SyntaxKind, range: TextRange) -> Self {
        Self { kind, range }
    }

    /// "Dereferences" the pointer to get the [`SyntaxNode`] it points to.
    ///
    /// Panics if node is not found, so make sure that `root` syntax tree is
    /// equivalent (is build from the same text) to the tree which was
    /// originally used to get this [`SyntaxNodePtr`].
    ///
    /// Also panics if `root` is not actually a root (i.e. it has a parent).
    ///
    /// The complexity is linear in the depth of the tree and logarithmic in
    /// tree width. As most trees are shallow, thinking about this as
    /// `O(log(N))` in the size of the tree is not too wrong!
    pub fn to_node(&self, root: &SyntaxNode) -> SyntaxNode {
        assert!(root.parent().is_none());
        iter::successors(Some(root.clone()), |node| {
            node.child_or_token_at_range(self.range)
                .and_then(|it| it.into_node())
        })
        .find(|it| it.text_range() == self.range && it.kind() == self.kind)
        .unwrap_or_else(|| panic!("can't resolve local ptr to SyntaxNode: {:?}", self))
    }

    /// Casts this to an [`AstPtr`] to the given node type if possible.
    pub fn cast<N: AstNode>(self) -> Option<AstPtr<N>> {
        if !N::can_cast(self.kind) {
            return None;
        }

        Some(AstPtr {
            raw: self,
            _ty: PhantomData,
        })
    }

    /// Returns the kind of the syntax node this points to.
    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    /// Returns the range of the syntax node this points to.
    pub fn text_range(&self) -> TextRange {
        self.range
    }
}

/// Like [`SyntaxNodePtr`], but remembers the type of node.
pub struct AstPtr<N: AstNode> {
    raw: SyntaxNodePtr,
    _ty: PhantomData<fn() -> N>,
}

impl<N: AstNode> AstPtr<N> {
    /// Returns an [`AstPtr`] for the node.
    pub fn new(node: &N) -> Self {
        Self {
            raw: SyntaxNodePtr::new(node.syntax()),
            _ty: PhantomData,
        }
    }

    pub fn from_range(kind: SyntaxKind, range: TextRange) -> Self {
        Self {
            raw: SyntaxNodePtr::from_range(kind, range),
            _ty: PhantomData,
        }
    }

    /// Given the root node containing the node `n` that `self` is a pointer to,
    /// returns `n`. See [`SyntaxNodePtr::to_node`].
    pub fn to_node(&self, root: &SyntaxNode) -> N {
        N::cast_node(self.raw.to_node(root)).unwrap()
    }

    /// Returns the underlying [`SyntaxNodePtr`].
    pub fn syntax_node_ptr(&self) -> SyntaxNodePtr {
        self.raw.clone()
    }

    /// Casts this to an [`AstPtr`] to the given node type if possible.
    pub fn cast<U: AstNode>(self) -> Option<AstPtr<U>> {
        if !U::can_cast(self.raw.kind) {
            return None;
        }
        Some(AstPtr {
            raw: self.raw,
            _ty: PhantomData,
        })
    }

    /// Like `SyntaxNodePtr::cast` but the trait bounds work out.
    pub fn try_from_raw(raw: SyntaxNodePtr) -> Option<AstPtr<N>> {
        N::can_cast(raw.kind()).then(|| AstPtr {
            raw,
            _ty: PhantomData,
        })
    }
}

impl<N: AstNode> fmt::Debug for AstPtr<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AstPtr").field("raw", &self.raw).finish()
    }
}

impl<N: AstNode> Clone for AstPtr<N> {
    fn clone(&self) -> Self {
        Self {
            raw: self.raw.clone(),
            _ty: PhantomData,
        }
    }
}

impl<N: AstNode> PartialEq for AstPtr<N> {
    fn eq(&self, other: &AstPtr<N>) -> bool {
        self.raw == other.raw
    }
}

impl<N: AstNode> Eq for AstPtr<N> {}

impl<N: AstNode> Hash for AstPtr<N> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.raw.hash(state)
    }
}

impl<N: AstNode> From<AstPtr<N>> for SyntaxNodePtr {
    fn from(ptr: AstPtr<N>) -> SyntaxNodePtr {
        ptr.raw
    }
}
