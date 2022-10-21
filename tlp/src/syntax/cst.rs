//! Concrete syntax tree
//!
//! `cst` is powered by [`rowan`]. It manages lossless structure of text, represented as a tree
//! (node) of untyped, homogeneous tokens.
//!
//! Semanctic information is latter added by casting the CST nodes to AST nodes.

pub mod lex;

mod parse;

#[doc(inline)]
pub use parse::parse;

#[doc(inline)]
pub use parse::ParseError;

/// Bridge to [`rowan`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}

impl rowan::Language for Lang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= SyntaxKind::ROOT as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

/// Subtree. View to green tree (red tree) in words of "red-green tree"
pub type SyntaxNode = rowan::SyntaxNode<Lang>;

/// Leaf. View to green token (red token) in words of "red-green tree"
pub type SyntaxToken = rowan::SyntaxToken<Lang>;

/// Subtree or leaf: [`SyntaxNode`] | [`SyntaxToken`]
pub type SyntaxElement = rowan::SyntaxElement<Lang>;

pub use rowan::TextRange;

/// Syntactic kind of a tree element (a token or a node)
///
/// The `SyntaxKind` is shared by the lexer and the parser. While the lexer is only interested in
/// tokens, sharing `SyntaxKind` is good for simplicity and is common pattern with [`rowan`] (as far
/// as I understand).
///
/// Some nodes are only available in AST and not appear in CST. For example, `Item` node in CST is
/// always a concrete example of it such as `Proc`. The AST casts those nodes in more abstract level
/// for allowing to process.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u16)]
pub enum SyntaxKind {
    // ----------------------------------------
    // Tokens
    // ----------------------------------------
    Ws,
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// String token, including the enclosures
    Str,
    /// `:`
    Colon,
    /// `.`
    Dot,
    /// Number token, including dots
    Num,
    /// `true`
    True,
    /// `false`
    False,
    /// `nil`
    Nil,
    // ?
    Ident,
    /// Any kind of comment (oneline or multiline, normal comment or docstring)
    Comment,
    /// Right arrow
    RightArrow,
    // ----------------------------------------
    // Compsitors (parser only, not used by lexer)
    // ----------------------------------------
    /// Literal node
    Literal,
    /// Path node
    Path,
    /// Pattern enum variant
    PatIdent,
    /// Pattern enum variant
    PatPath,
    /// Call node
    Call,
    /// Set node
    Set,
    /// And node
    And,
    /// Or node
    Or,
    /// `=` node
    Equal,
    /// When node
    When,
    /// Unless node
    Unless,
    /// Cond node
    Cond,
    /// Cond case node
    CondCase,
    /// Loop node
    Loop,
    /// While node
    While,
    /// DefProc node
    DefProc,
    /// DefProc node name node
    ProcName,
    /// DefProc node parameter list
    Params,
    /// A single procedure parameter
    Param,
    /// ReturnType,
    ReturnType,
    /// Type node
    TypePath,
    /// Expressions marked as inline code block
    Block,
    /// Let
    Let,
    // ----------------------------------------
    // composite node
    /// Beginning of text
    ROOT,
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

impl SyntaxKind {
    #[inline]
    pub fn is_trivia(self) -> bool {
        matches!(self, SyntaxKind::Ws | SyntaxKind::Comment)
    }
}
