//! Abstract syntax tree, typed tree layered on top of CST
//!
//! All AST nodes have the same internal structure, i.e, CST. As a result, AST nodes are just
//! wrappers around CST nodes. Each component is lazily retrieved via accessors traversing the
//! internal CST.
//!
//! # AST node types
//!
//! | Node type     | Example           | Has CST Node | Description                     |
//! |---------------|-------------------|--------------|---------------------------------|
//! | Concrete      | [`Block`]         | Yes          | Directly mapped from CST to AST |
//! | Transparent   | [`Item`], [`Pat`] | No           | Wraps a concrete node           |
//! | Token wrapper | [`Literal`]       | Yes          | Wraps a node in `*Kind`         |

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

    pub fn items(&self) -> impl Iterator<Item = Item> {
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

// FIXME: make sure if `Block` is `ast::Expr` or not
define_enum_node! {
    /// AST expression node (transparent node wrapper)
    Expr = Let
        | Call | Set | And | Or | When | Unless | Cond
        | Loop | While
        | Literal | Path | Block,
    SyntaxKind::Let | SyntaxKind::Call | SyntaxKind::Literal | SyntaxKind::Path | SyntaxKind::Block
}

define_node! {
    /// Expressions marked as inline code block
    Block: SyntaxKind::Block,

    /// ident ("." ident)*
    Path: SyntaxKind::Path,

    /// Pattern node variant
    PatIdent: SyntaxKind::PatIdent,

    /// Pattern node variant
    PatPath: SyntaxKind::PatPath,

    /// Type node variant
    TypePath: SyntaxKind::TypePath,
}

impl Block {
    pub fn exprs(&self) -> impl Iterator<Item = Expr> {
        self.syn.children().filter_map(Expr::cast_node)
    }
}

impl Path {
    pub fn segments(&self) -> impl Iterator<Item = PathSegment> {
        self.syn
            .children_with_tokens()
            .filter_map(|e| e.into_token())
            .filter_map(PathSegment::cast_token)
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
    pub fn segments(&self) -> impl Iterator<Item = PathSegment> {
        self.syn
            .children_with_tokens()
            .filter_map(|e| e.into_token())
            .filter_map(PathSegment::cast_token)
    }

    pub fn into_item(self) -> Item {
        Item::cast_node(self.syn).unwrap()
    }
}

impl TypePath {
    /// Unwraps the underlying `Path` node
    pub fn into_path(&self) -> Path {
        self.syn.children().find_map(Path::cast_node).unwrap()
    }

    /// Wraps the self type with the [`Type`] enum
    pub fn into_type(self) -> Type {
        Type::cast_node(self.syn).unwrap()
    }
}

define_node! {
    /// "(" "let" pat expr ")"
    Let: SyntaxKind::Let,

    /// "(" "when" sexp* ")"
    When: SyntaxKind::When,

    /// "(" "cond" cond-case* ")"
    Cond: SyntaxKind::Cond,

    /// "(" sexp sexp* ")"
    ///
    /// It's very similar to `when`.
    CondCase: SyntaxKind::CondCase,

    /// Loop node
    Loop: SyntaxKind::Loop,

    /// While node
    While: SyntaxKind::While,

    /// "(" "unless" sexp* ")"
    Unless: SyntaxKind::Unless,
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

    pub fn params(&self) -> Option<ProcParams> {
        self.syn
            .children()
            .find(|node| node.kind() == SyntaxKind::Params)
            .map(|node| ProcParams { syn: node.clone() })
    }

    pub fn right_arrow(&self) -> Option<SyntaxToken> {
        self.syn
            .children_with_tokens()
            .filter_map(|e| e.into_token())
            .find(|t| t.kind() == SyntaxKind::RightArrow)
    }

    pub fn return_ty(&self) -> Option<ReturnType> {
        self.syn.children().find_map(|elem| ReturnType::cast_node(elem))
    }

    pub fn block(&self) -> Block {
        self.syn.children().find_map(Block::cast_node).unwrap()
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

impl When {
    pub fn pred(&self) -> Option<Expr> {
        self.syn.children().find_map(Expr::cast_node)
    }

    pub fn block(&self) -> Block {
        self.syn.children().find_map(Block::cast_node).unwrap()
    }
}

impl Unless {
    pub fn pred(&self) -> Option<Expr> {
        self.syn.children().find_map(Expr::cast_node)
    }

    pub fn block(&self) -> Block {
        self.syn.children().find_map(Block::cast_node).unwrap()
    }
}

impl Cond {
    pub fn cases(&self) -> impl Iterator<Item = CondCase> {
        self.syn.children().filter_map(CondCase::cast_node)
    }
}

impl CondCase {
    pub fn pred(&self) -> Option<Expr> {
        self.syn.children().find_map(Expr::cast_node)
    }

    pub fn block(&self) -> Block {
        self.syn.children().find_map(Block::cast_node).unwrap()
    }
}

impl Loop {
    pub fn block(&self) -> Option<Block> {
        self.syn.children().find_map(Block::cast_node)
    }
}

impl While {
    pub fn pred(&self) -> Option<Expr> {
        self.syn
            .children()
            .filter(|e| e.kind() != SyntaxKind::Block)
            .find_map(Expr::cast_node)
    }

    pub fn block(&self) -> Option<Block> {
        self.syn.children().find_map(Block::cast_node)
    }
}

define_node! {
    /// "(" ident sexp* ")"
    Call: SyntaxKind::Call,

    /// "(" "set" sexp ")"
    Set: SyntaxKind::Set,

    /// "(" "and" sexp* ")"
    And: SyntaxKind::And,

    /// "(" "or" sexp* ")"
    Or: SyntaxKind::Or,
}

impl Call {
    /// Function path
    pub fn path(&self) -> Path {
        self.syn.children().find_map(Path::cast_node).unwrap()
    }

    /// Function arguments
    pub fn args(&self) -> impl Iterator<Item = Expr> {
        // NOTE: skip function path
        self.syn.children().filter_map(Expr::cast_node).skip(1)
    }
}

impl Set {
    /// Place
    pub fn place(&self) -> Option<Path> {
        self.syn.children().find_map(Path::cast_node)
    }

    /// Function arguments
    pub fn rhs(&self) -> Option<Expr> {
        let mut iter = self.syn.children().filter_map(Expr::cast_node);

        let i0 = iter.next()?;
        if i0.syntax().kind() != SyntaxKind::Path {
            return None;
        }

        iter.next()
    }
}

impl And {
    pub fn exprs(&self) -> impl Iterator<Item = Expr> {
        self.syn.children().filter_map(Expr::cast_node)
    }
}

impl Or {
    pub fn exprs(&self) -> impl Iterator<Item = Expr> {
        self.syn.children().filter_map(Expr::cast_node)
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
    /// "(" "proc" name "(" params? ")" block ")"
    DefProc: SyntaxKind::DefProc,

    /// Procedure parameters
    ProcParams: SyntaxKind::Params,

    /// A single procedure parameter
    Param: SyntaxKind::Param,

    /// Return type
    ReturnType: SyntaxKind::ReturnType,
}

impl ProcParams {
    pub fn nodes(&self) -> impl Iterator<Item = Param> {
        self.syn.children().filter_map(Param::cast_node)
    }
}

impl Param {
    pub fn pat(&self) -> Option<Pat> {
        self.syn.children().find_map(|elem| Pat::cast_node(elem))
    }

    pub fn colon(&self) -> Option<SyntaxToken> {
        self.syn
            .children_with_tokens()
            .filter_map(|e| e.into_token())
            .find(|tk| tk.kind() == SyntaxKind::Colon)
    }

    pub fn ty(&self) -> Option<Type> {
        self.syn.children().find_map(|elem| Type::cast_node(elem))
    }

    /// Text range with whitespce carefully excluded
    pub fn view_range(&self) -> rowan::TextRange{
        if self.ty().is_some() {
            // pat: Ty
            self.syntax().text_range()
        } else if let Some(colon) = self.colon() {
            // pat:
            let start = self.syntax().text_range().start();
            let end = colon.text_range().end();

            rowan::TextRange::new(start, end)
        } else if let Some(pat) = self.pat() {
            // pat
            pat.syntax().text_range()
        } else {
            // ?:
            self.syntax().text_range()
        }
    }
}

impl ReturnType {
    pub fn ty(&self) -> Type {
        self.syn.children().find_map(|elem| Type::cast_node(elem)).unwrap()
    }
}

define_enum_node! {
    /// Pattern node (transparent)
    Pat = PatIdent | PatPath,
    SyntaxKind::PatIdent | SyntaxKind::PatPath
}

define_enum_node! {
    /// Type node (transparent)
    Type = TypePath,
    SyntaxKind::TypePath
}

define_token_wrapper! {
    /// Literal node (token wrapper)
    Literal: SyntaxKind::Literal,
    /// View to a [`Literal`] node token
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

    /// Path segment
    PathSegment: SyntaxKind::Ident;
}
