/*!
Concrete syntax tree

`cst` is powered by [`rowan`]. It manages lossless structure of text, represented as a tree (node)
of untyped, homogeneous tokens.

Semanctic information is latter added by casting the CST nodes to AST nodes.
*/

pub mod lex;

mod parse;

#[doc(inline)]
pub use parse::parse_str;

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

/**
Syntactic kind of a tree element

Both the lexer and the parser use this enum.
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    String,
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
    // ----------------------------------------
    // Compsitors (parser only, not used by lexer)
    /// Path node
    Path,
    /// Call node
    Call,
    /// DefProc node
    DefProc,
    /// Procedure parameter list
    Params,
    /// Procedure body node
    Body,
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
