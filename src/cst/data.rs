/*!
Syntaxtic data types
*/

/// Bridge for `rowan`
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

/// View to `GreenNode`
pub type SyntaxNode = rowan::SyntaxNode<Lang>;

pub type SyntaxToken = rowan::SyntaxToken<Lang>;

/// [`SyntaxNode`] | [`SyntaxToken`]
pub type SyntaxElement = rowan::SyntaxElement<Lang>;

/// Syntactic kind for `rowan`
///
/// It's used for both lexing and parsing.
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
    // TODO: comment
    // ----------------------------------------
    // Compsitors (parser only, not used by lexer)
    List,
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
