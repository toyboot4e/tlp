/*!
Abstract syntax tree, layered on top of CST

All AST nodes have the same internal structure, i.e, CST. As a result, AST nodes are just wrappers
around CST nodes. Each component is lazily and temporary retrieved via accessors traversing the
internal CST.
*/

use crate::syntax::cst::{
    data::{SyntaxKind, SyntaxNode, SyntaxToken},
    parse::ParseError,
};

pub fn parse(src: &str) -> (Ast, Vec<ParseError>) {
    let (cst, errs) = crate::syntax::cst::parse::from_str(src);
    (Ast::new(cst).unwrap(), errs)
}

/// Syntax node → AST node (semantic node)
pub trait Node: Sized {
    fn cast(syn: SyntaxNode) -> Option<Self>;
}

/// Root node of syntax tree
pub struct Ast {
    syn: SyntaxNode,
}

impl Ast {
    fn new(syn: SyntaxNode) -> Option<Self> {
        if syn.kind() == SyntaxKind::ROOT {
            Some(Self { syn })
        } else {
            None
        }
    }
}

/// [`List`] | [`Atom`]
#[derive(Debug, Clone, PartialEq)]
pub struct Sexp {
    syn: SyntaxNode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SexpKind {
    List,
    Atom,
}

impl Node for Sexp {
    fn cast(syn: SyntaxNode) -> Option<Self> {
        if Atom::cast(syn.clone()).is_some() || List::cast(syn.clone()).is_some() {
            Some(Self { syn })
        } else {
            None
        }
    }
}

impl Sexp {
    fn kind(&self) -> SexpKind {
        Atom::cast(self.syn.clone())
            .map(|_| SexpKind::Atom)
            .or_else(|| List::cast(self.syn.clone()).map(|_| SexpKind::List))
            .unwrap()
    }
}

fn find_token(syn: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxToken> {
    syn.children_with_tokens()
        .filter_map(|elem| elem.into_token())
        .find(|tk| tk.kind() == SyntaxKind::LParen)
}

macro_rules! impl_display {
    ($name:ident) => {
        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                std::fmt::Display::fmt(&self.syn, f)
            }
        }
    };
}

macro_rules! def_node {
    ($name:ident) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub struct $name {
            syn: SyntaxNode,
        }

        impl_display!($name);
    };
}

def_node!(List);

impl List {
    fn l_paren(&self) -> Option<SyntaxToken> {
        self::find_token(&self.syn, SyntaxKind::LParen)
    }

    fn r_paren(&self) -> Option<SyntaxToken> {
        self::find_token(&self.syn, SyntaxKind::RParen)
    }

    fn terms(&self) -> impl Iterator<Item = Sexp> {
        self.syn.children().filter_map(Sexp::cast)
    }
}

impl Node for List {
    fn cast(syn: SyntaxNode) -> Option<Self> {
        if syn.kind() == SyntaxKind::List {
            Some(Self { syn })
        } else {
            None
        }
    }
}

// Atom = Symbol for now
def_node!(Atom);

impl Node for Atom {
    fn cast(syn: SyntaxNode) -> Option<Self> {
        if syn.kind() == SyntaxKind::List {
            Some(Self { syn })
        } else {
            None
        }
    }
}
