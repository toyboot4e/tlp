//! Abstract syntax tree, typed tree layered on top of CST
//!
//! All AST nodes have the same internal structure, i.e, CST. As a result, AST nodes are just
//! wrappers around CST nodes. Each component is lazily retrieved via accessors traversing the
//! internal CST.
//!
//! # AST node types
//!
//! - Concrete node (e.g. [`Block`])
//!   They implement [`AstNode`]. They have corresponding CST node. This is the most basic node
//!   type.
//!
//! - Transparent node (e.g. [`Item`] enum, [`Expr`] enum and [`Pat`] enum)
//!   They implement [`AstNode`]. They don't have corresponding CST node and just wraps the
//!   underlying [`AstNode`] types.
//!
//! - Token wrapper node (e.g. [`Literal`] with [`LiteralKind`])
//!   They implement [`AstNode`], but they don't have corresponding CST token. So we need
//!   indirection from node to token, i.e., [`LiteralKind`].

pub use crate::syntax::cst::ParseError;

use std::{
    cmp::{Eq, PartialEq},
    hash::Hash,
};

use crate::syntax::cst::{self, SyntaxKind, SyntaxNode, SyntaxToken};

const STR_PROC: &'static str = "proc";
const STR_LET: &'static str = "let";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseResult {
    pub doc: Document,
    pub errs: Vec<ParseError>,
}

impl ParseResult {
    pub fn into_tuple(self) -> (Document, Vec<ParseError>) {
        (self.doc, self.errs)
    }
}

pub fn parse(src: &str) -> ParseResult {
    let (cst, errs) = cst::parse_str(src);
    let doc = Document::from_root(cst).unwrap();
    ParseResult { doc, errs }
}

/// Semantic node casted from syntax node
pub trait AstNode: Sized {
    /// Method for "syntax pointers"
    fn can_cast(kind: SyntaxKind) -> bool;
    fn cast_node(syn: SyntaxNode) -> Option<Self>;
    fn syntax(&self) -> &SyntaxNode;
}

/// Semantic token casted from syntax token
pub trait AstToken: Sized {
    /// Method for "syntax pointers"
    fn can_cast(kind: SyntaxKind) -> bool;
    fn cast_token(syn: SyntaxToken) -> Option<Self>;
    fn syntax(&self) -> &SyntaxToken;
}

/// Defines a physical syntax node
macro_rules! define_node {
    (
        $(
            $( #[$meta:meta] )*
            $ty:ident: $pat:pat,
        )*
    ) => {
        $(
            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            $( #[$meta] )*
            pub struct $ty {
                pub(crate) syn: SyntaxNode,
            }

            impl AstNode for $ty {
                fn can_cast(kind: SyntaxKind) -> bool {
                    matches!(kind, $pat)
                }

                fn cast_node(syn: SyntaxNode) -> Option<Self> {
                    if Self::can_cast(syn.kind()) {
                        Some(Self { syn })
                    } else {
                        None
                    }
                }

                fn syntax(&self) -> &SyntaxNode {
                    &self.syn
                }
            }
        )*
    };
}

/// Defines an enum node that does NOT have corresponding CST node
macro_rules! define_enum_node {
    (
        $( #[$meta:meta] )*
        $ty:ident = $( $var:ident )|*, $pat:pat
    ) => {
        $( #[$meta] )*
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum $ty {
            $( $var($var), )*
        }

        impl AstNode for $ty {
            fn can_cast(kind: SyntaxKind) -> bool {
                matches!(kind, $pat)
            }

            fn cast_node(syn: SyntaxNode) -> Option<Self> {
                $(
                    if let Some(node) = $var::cast_node(syn.clone()) {
                        return Some(Self::$var(node));
                    }
                )*
                None
            }

            fn syntax(&self) -> &SyntaxNode {
                match self {
                    $( Self::$var(x) => x.syntax(), )*
                }
            }
        }

        $(
            impl From<$var> for $ty {
                fn from(v: $var) -> Self {
                    Self::$var(v)
                }
            }
        )*
    };
}

macro_rules! define_token_wrapper {
    (
        $( #[$meta:meta] )*
        $ty:ident: $pat:pat,
        $( #[$kind_meta:meta] )*
        $ty_kind:ident = $( $var:ident )|* ;
    ) => {
        define_node! {
            $( #[$meta] )*
            $ty: $pat,
        }

        impl $ty {
            pub fn token(&self) -> SyntaxToken {
                self.syn
                    .children_with_tokens()
                    // .filter(|elem| !elem.kind().is_trivia())
                    .find_map(|e| e.into_token())
                    .unwrap()
            }

            pub fn kind(&self) -> $ty_kind {
                let token = self.token();
                $(
                    if let Some(x) = $var::cast_token(token.clone()) {
                        return $ty_kind::$var(x);
                    }
                )*
                unreachable!("Can't be casted as {:?}: {:?}", stringify!($ty), token);
            }
        }

        #[derive(Debug, Clone, PartialEq)]
        $( #[$kind_meta] )*
        pub enum $ty_kind {
            $($var($var),)*
        }


        $(
            impl From<$var> for $ty_kind {
                fn from(v: $var) -> Self {
                    Self::$var(v)
                }
            }
        )*
    };
}

/// AST of a file
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Document {
    pub(crate) syn: SyntaxNode,
}

impl Document {
    pub fn from_root(syn: SyntaxNode) -> Option<Self> {
        if syn.kind() == SyntaxKind::ROOT {
            Some(Self { syn })
        } else {
            None
        }
    }

    pub fn item_nodes(&self) -> impl Iterator<Item = Item> {
        self.syn.children().filter_map(Item::cast_node)
    }

    pub fn syntax(&self) -> &SyntaxNode {
        &self.syn
    }
}

define_enum_node! {
    /// AST item node (transparent node wrapper)
    Item = DefProc,
    SyntaxKind::DefProc
}

define_enum_node! {
    /// AST expression node (transparent node wrapper)
    Expr = Let | Call | Literal | Path | Block,
    SyntaxKind::Let | SyntaxKind::Call | SyntaxKind::Literal | SyntaxKind::Path | SyntaxKind::Block
}

define_node! {
    /// (proc name (params?) (block)..)
    DefProc: SyntaxKind::DefProc,

    /// (let pat sexp*)
    Let: SyntaxKind::Let,

    /// (ident args sexp*)
    Call: SyntaxKind::Call,

    /// Expressions marked as inline code block
    Block: SyntaxKind::Block,

    /// Path
    Path: SyntaxKind::Path,

    /// Pattern variant
    PatIdent: SyntaxKind::PatIdent,

    /// Pattern variant
    PatPath: SyntaxKind::PatPath,
}

impl Block {
    pub fn exprs(&self) -> impl Iterator<Item = Expr> {
        self.syn.children().filter_map(Expr::cast_node)
    }
}

impl Path {
    pub fn components(&self) -> impl Iterator<Item = PathComponent> {
        self.syn
            .children_with_tokens()
            .filter_map(|e| e.into_token())
            .filter_map(PathComponent::cast_token)
    }
}

impl PatIdent {
    pub fn ident_token(self) -> SyntaxToken {
        self.syn
            .children_with_tokens()
            .find_map(|e| e.into_token())
            .unwrap()
    }
}

impl PatPath {
    pub fn components(&self) -> impl Iterator<Item = PathComponent> {
        self.syn
            .children_with_tokens()
            .filter_map(|e| e.into_token())
            .filter_map(PathComponent::cast_token)
    }

    pub fn into_form(self) -> Item {
        Item::cast_node(self.syn).unwrap()
    }
}

impl Let {
    pub fn let_tk(&self) -> SyntaxToken {
        self.syn
            .children_with_tokens()
            .filter_map(|e| e.into_token())
            .find(|t| t.kind() == SyntaxKind::Ident && t.text() == STR_LET)
            .unwrap_or_else(|| unreachable!())
    }

    // pattern (currently an identifier only)
    pub fn pat(&self) -> Option<Pat> {
        self.syn.children().find_map(Pat::cast_node)
    }

    pub fn rhs(&self) -> Option<Expr> {
        self.syn.children().find_map(Expr::cast_node)
    }
}

impl Call {
    /// Function path
    pub fn path(&self) -> Path {
        self.syn.children().find_map(Path::cast_node).unwrap()
    }

    /// Function arguments
    pub fn args(&self) -> impl Iterator<Item = Expr> {
        // skip path
        self.syn.children().filter_map(Expr::cast_node).skip(1)
    }
}

impl DefProc {
    // TODO: add defproc token
    /// `DefProc` token
    pub fn proc_tk(&self) -> SyntaxToken {
        self.syn
            .children_with_tokens()
            .filter_map(|e| e.into_token())
            .find(|t| t.kind() == SyntaxKind::Ident && t.text() == STR_PROC)
            .unwrap_or_else(|| unreachable!())
    }

    pub fn name(&self) -> Option<ProcName> {
        let mut c = self
            .syn
            .children()
            .filter(|node| node.kind() != SyntaxKind::Ws);

        c.next().and_then(ProcName::cast_node)
    }

    pub fn params(&self) -> Option<Params> {
        self.syn
            .children()
            .filter(|node| node.kind() == SyntaxKind::Params)
            .next()
            .map(|node| Params { syn: node.clone() })
    }

    pub fn block(&self) -> Block {
        self.syn.children().find_map(Block::cast_node).unwrap()
    }
}

define_node! {
    /// Procedure name
    ProcName: SyntaxKind::ProcName,
}

impl ProcName {
    pub fn token(&self) -> SyntaxToken {
        self.syn
            .children_with_tokens()
            .next()
            .unwrap()
            .into_token()
            .unwrap()
    }
}

define_node! {
    /// Procedure parameters
    Params: SyntaxKind::Params,

    /// A single procedure parameter
    Param: SyntaxKind::Param,
}

impl Params {
    pub fn param_nodes(&self) -> impl Iterator<Item = Param> {
        self.syn.children().filter_map(Param::cast_node)
    }
}

impl Param {
    pub fn token(&self) -> SyntaxToken {
        self.syn
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .find(|tk| tk.kind() == SyntaxKind::Ident)
            .unwrap()
    }
}

define_enum_node! {
    /// Pattern node
    Pat = PatIdent | PatPath, SyntaxKind::PatIdent | SyntaxKind::PatPath
}

define_token_wrapper! {
    /// Literal node
    Literal: SyntaxKind::Literal,
    // View to the [`Literal`] node
    // TODO: use `Bool` node?
    LiteralKind = Num | Str | True | False;
}

macro_rules! define_token {
    (
        $(
            $( #[$meta:meta] )*
            $ty:ident: $kind:path $(| $kind2:path)* ;
        )*
    ) => {
        $(
            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            $( #[$meta] )*
            pub struct $ty {
                pub(crate) syn: SyntaxToken,
            }

            impl AstToken for $ty {
                fn can_cast(kind: SyntaxKind) -> bool {
                    matches!(kind, $kind $(| $kind2)*)
                }

                fn cast_token(syn: SyntaxToken) -> Option<Self> {
                    if matches!(syn.kind(), $kind $(| $kind2)*) {
                        Some(Self { syn })
                    } else {
                        None
                    }
                }

                fn syntax(&self) -> &SyntaxToken {
                    &self.syn
                }
            }

            impl $ty {
                pub fn text(&self) -> &str {
                    self.syn.text()
                }
            }
        )*
    };
}

define_token! {
    /// Untyped, not validated number type (integers and floats)
    Num: SyntaxKind::Num;

    /// String, including the surroundings
    Str: SyntaxKind::Str;

    /// true
    True: SyntaxKind::True;

    /// false
    False: SyntaxKind::False;

    /// Identifier
    Ident: SyntaxKind::Ident;

    /// PathComponent
    PathComponent: SyntaxKind::Ident;
}
