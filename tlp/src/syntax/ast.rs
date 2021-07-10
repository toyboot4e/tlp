/*!
Abstract syntax tree, typed tree layered on top of CST

All AST nodes have the same internal structure, i.e, CST. As a result, AST nodes are just wrappers
around CST nodes. Each component is lazily retrieved via accessors traversing the internal CST.
*/

use crate::syntax::cst::{
    data::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken},
    parse::{self, ParseError},
};

const DEF_FN: &'static str = "def:fn";

pub fn parse(src: &str) -> (Document, Vec<ParseError>) {
    let (cst, errs) = parse::from_str(src);
    (Document::from_root(cst).unwrap(), errs)
}

/// Syntax node → AST node (semantic node)
pub trait Node: Sized {
    fn cast(syn: SyntaxNode) -> Option<Self>;
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
    ($name:ident, $doc:expr) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
        #[doc = $doc]
        pub struct $name {
            syn: SyntaxNode,
        }

        impl_display!($name);
    };
}

fn find_token(syn: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxToken> {
    syn.children_with_tokens()
        .filter_map(|elem| elem.into_token())
        .find(|tk| tk.kind() == SyntaxKind::LParen)
}

def_node!(Document, "toylisp representation of a file");

impl Document {
    pub fn from_root(syn: SyntaxNode) -> Option<Self> {
        if syn.kind() == SyntaxKind::ROOT {
            Some(Self { syn })
        } else {
            None
        }
    }

    pub fn items(&self) -> impl Iterator<Item = Form> {
        self.syn.children().filter_map(Form::cast)
    }
}

def_node!(
    Form,
    "Data that forms the program

Call | DefFn | Atom"
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FormKind {
    Call,
    DefFn,
    Atom,
}

impl Node for Form {
    fn cast(syn: SyntaxNode) -> Option<Self> {
        if false
            || Atom::cast(syn.clone()).is_some()
            || Call::cast(syn.clone()).is_some()
            || DefFn::cast(syn.clone()).is_some()
        {
            Some(Self { syn })
        } else {
            None
        }
    }
}

impl Form {
    pub fn kind(&self) -> FormKind {
        None.or_else(|| DefFn::cast(self.syn.clone()).map(|_| FormKind::DefFn))
            .or_else(|| Call::cast(self.syn.clone()).map(|_| FormKind::Call))
            .or_else(|| Atom::cast(self.syn.clone()).map(|_| FormKind::Atom))
            .unwrap()
    }

    pub fn as_call(&self) -> Option<Call> {
        Call::cast(self.syn.clone())
    }

    pub fn as_def_fn(&self) -> Option<DefFn> {
        DefFn::cast(self.syn.clone())
    }

    pub fn as_atom(&self) -> Option<Atom> {
        Atom::cast(self.syn.clone())
    }
}

def_node!(Call, "Function call");

impl Node for Call {
    fn cast(syn: SyntaxNode) -> Option<Self> {
        let mut c = syn.children_with_tokens();

        if c.next()?.kind() != SyntaxKind::LParen {
            return None;
        }

        let mut c = c.skip_while(|e| e.kind() == SyntaxKind::Ws);

        if c.next()?.kind() != SyntaxKind::Ident {
            return None;
        }

        Some(Self { syn })
    }
}

impl Call {
    pub fn args(&self) -> impl Iterator<Item = Form> {
        self.syn
            .children_with_tokens()
            .filter(|elem| elem.kind() != SyntaxKind::Ws)
            .filter_map(|elem| match elem {
                SyntaxElement::Token(_tk) => None,
                SyntaxElement::Node(n) => Form::cast(n),
            })
    }
}

def_node!(DefFn, "Function definition");

impl Node for DefFn {
    fn cast(syn: SyntaxNode) -> Option<Self> {
        let mut c = syn.children_with_tokens();

        if c.next()?.kind() != SyntaxKind::LParen {
            return None;
        }

        let mut c = c.skip_while(|e| e.kind() == SyntaxKind::Ws);

        match c.next()? {
            SyntaxElement::Node(_n) => return None,
            SyntaxElement::Token(tk) => {
                if tk.kind() != SyntaxKind::Ident || tk.text() != DEF_FN {
                    return None;
                }
            }
        }

        Some(Self { syn })
    }
}

def_node!(Atom, "Atom = Symbol for now");

impl Node for Atom {
    fn cast(syn: SyntaxNode) -> Option<Self> {
        if syn.kind() == SyntaxKind::List {
            Some(Self { syn })
        } else {
            None
        }
    }
}
