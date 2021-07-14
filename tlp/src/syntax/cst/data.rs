/*!
CST data types on [`rowan`]
*/

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

/// Syntactic kind of a tree element
///
/// It's used for both lexing and parsing, but the lexer doesn't use compositing items.
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
    /// `"`
    StrEnclosure,
    /// `:`
    Colon,
    /// `.`
    Dot,
    /// Text inside string enclosures
    StrContent,
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
    // CommentChunk
    List,
    String,
    Path,
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
