/// Generic data that represents span of tokens
#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<Body> {
    pub tsp: TokenSpan,
    pub body: Body,
}

macro_rules! enum_from {
    ($into:ident, $($ty:ident),*) => {
        $(
            impl From<$ty> for $into {
                fn from(x: $ty) -> Self {
                    Self::$ty(x)
                }
            }
        )*
    };
}

/// S-expression = [`List`] | [`Atom`]
///
/// # Syntax
///
/// * [`Atom`] = [`Symbol`] | [`Lit`]
///     * Symbol = ValSymbol | FnSymbol | Keyword
#[derive(Debug, Clone, PartialEq)]
pub enum Sx {
    Atom(Atom),
    List(List),
}

// S-expression = List | Atom
enum_from!(Sx, List, Atom);

impl Sx {
    /// Token span
    pub fn tsp(&self) -> TokenSpan {
        match self {
            Sx::Atom(a) => a.tsp(),
            Sx::List(l) => l.tsp,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Lit(Lit),
    Symbol(Symbol),
}

// Atom = Lit | Symbol
enum_from!(Atom, Lit, Symbol);

impl Atom {
    /// Token span
    pub fn tsp(&self) -> TokenSpan {
        match self {
            Atom::Lit(l) => l.tsp,
            Atom::Symbol(s) => s.tsp,
        }
    }
}

pub type Lit = Spanned<LitBody>;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LitBody {
    Nil,
    Bool,
    Str {
        beg: TokenSpan,
        content: TokenSpan,
        end: TokenSpan,
    },
    Num,
}

impl LitBody {
    pub fn one_tk(kind: SyntaxKind) -> Option<Self> {
        match kind {
            SyntaxKind::True => Some(Self::Bool),
            SyntaxKind::False => Some(Self::Bool),
            SyntaxKind::Nil => Some(Self::Nil),
            SyntaxKind::Num => Some(Self::Num),
            _ => None,
        }
    }
}

/// Function call
pub type List = Spanned<ListBody>;

#[derive(Debug, Clone, PartialEq)]
pub struct ListBody {
    pub operands: Vec<Sx>,
}

pub type Symbol = Spanned<SymbolBody>;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SymbolBody {
    Ident,
}
