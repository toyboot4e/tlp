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

/// Semantic node casted from syntax node
pub trait AstNode: Sized {
    fn cast_node(syn: SyntaxNode) -> Option<Self>;
}

/// Semantic token casted from syntax token
pub trait AstToken: Sized {
    fn cast_tk(syn: SyntaxToken) -> Option<Self>;
}

/// Semantic element casted from syntax element
pub trait AstElement: Sized {
    fn cast_elem(syn: SyntaxElement) -> Option<Self>;
}

/// AST of a file
#[derive(Debug, Clone, PartialEq)]
pub struct Document {
    syn: SyntaxNode,
}

impl Document {
    pub fn from_root(syn: SyntaxNode) -> Option<Self> {
        if syn.kind() == SyntaxKind::ROOT {
            Some(Self { syn })
        } else {
            None
        }
    }

    pub fn items(&self) -> impl Iterator<Item = Form> {
        self.syn.children_with_tokens().filter_map(Form::cast_elem)
    }
}

/// S-expression, which forms the program
///
/// Call | DefFn | Atom
#[derive(Debug, Clone, PartialEq)]
pub struct Form {
    syn: SyntaxElement,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FormKind {
    Call,
    DefFn,
    Atom,
}

impl AstElement for Form {
    fn cast_elem(syn: SyntaxElement) -> Option<Self> {
        if let Some(node) = syn.clone().into_node() {
            if Call::cast_node(node.clone()).is_some() || DefFn::cast_node(node.clone()).is_some() {
                Some(Self { syn })
            } else {
                None
            }
        } else {
            if Atom::cast_elem(syn.clone()).is_some() {
                Some(Self { syn })
            } else {
                None
            }
        }
    }
}

impl Form {
    pub fn kind(&self) -> FormKind {
        if let Some(node) = self.syn.clone().into_node() {
            None.or_else(|| DefFn::cast_node(node.clone()).map(|_| FormKind::DefFn))
                .or_else(|| Call::cast_node(node.clone()).map(|_| FormKind::Call))
                .or_else(|| Atom::cast_elem(self.syn.clone()).map(|_| FormKind::Atom))
                .unwrap()
        } else {
            Atom::cast_elem(self.syn.clone())
                .map(|_| FormKind::Atom)
                .unwrap()
        }
    }

    pub fn as_call(&self) -> Option<Call> {
        Call::cast_node(self.syn.clone().into_node()?)
    }

    pub fn as_def_fn(&self) -> Option<DefFn> {
        DefFn::cast_node(self.syn.clone().into_node()?)
    }
}

/// Function call
#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    syn: SyntaxNode,
}

impl AstNode for Call {
    fn cast_node(syn: SyntaxNode) -> Option<Self> {
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
            .filter_map(Form::cast_elem)
    }
}

/// Function definition
#[derive(Debug, Clone, PartialEq)]
pub struct DefFn {
    syn: SyntaxNode,
}

impl AstNode for DefFn {
    fn cast_node(syn: SyntaxNode) -> Option<Self> {
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

impl DefFn {
    pub fn params(&self) -> Option<Params> {
        self.syn
            .first_child()
            .filter(|node| node.kind() == SyntaxKind::List)
            .map(|node| Params { syn: node.clone() })
    }
}

/// Function paramaters
#[derive(Debug, Clone, PartialEq)]
pub struct Params {
    syn: SyntaxNode,
}

impl Params {
    pub fn vars(&self) -> impl Iterator<Item = SyntaxToken> {
        self.syn
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .filter(|tk| tk.kind() == SyntaxKind::Ident)
    }

    /// Number of arguments
    pub fn arity(&self) -> usize {
        self.vars().count()
    }
}

/// Atom = Symbol for now
#[derive(Debug, Clone, PartialEq)]
pub struct Atom {
    syn: SyntaxElement,
}

impl AstElement for Atom {
    fn cast_elem(syn: SyntaxElement) -> Option<Self> {
        if Symbol::cast_elem(syn.clone()).is_some() {
            Some(Self { syn })
        } else {
            None
        }
    }
}

/// Non-list S-expression
#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    syn: SyntaxElement,
}

pub enum SymbolKind {
    Num,
    Str,
    Bool,
}

impl AstElement for Symbol {
    fn cast_elem(syn: SyntaxElement) -> Option<Self> {
        if matches!(
            syn.kind(),
            SyntaxKind::Num | SyntaxKind::String | SyntaxKind::True | SyntaxKind::False
        ) {
            Some(Self { syn })
        } else {
            None
        }
    }
}

impl Symbol {
    pub fn kind(&self) -> SymbolKind {
        match self.syn.clone() {
            SyntaxElement::Token(tk) => None
                .or_else(|| Num::cast_tk(tk.clone()).map(|_| SymbolKind::Num))
                .or_else(|| Bool::cast_tk(tk.clone()).map(|_| SymbolKind::Bool))
                .unwrap(),
            SyntaxElement::Node(n) => Str::cast_node(n.clone()).map(|_| SymbolKind::Str).unwrap(),
        }
    }
}

/// Number
#[derive(Debug, Clone, PartialEq)]
pub struct Num {
    syn: SyntaxToken,
}

impl AstToken for Num {
    fn cast_tk(syn: SyntaxToken) -> Option<Self> {
        if syn.kind() == SyntaxKind::Num {
            Some(Self { syn })
        } else {
            None
        }
    }
}

/// String
#[derive(Debug, Clone, PartialEq)]
pub struct Str {
    syn: SyntaxNode,
}

impl AstNode for Str {
    fn cast_node(syn: SyntaxNode) -> Option<Self> {
        if syn.kind() == SyntaxKind::String {
            Some(Self { syn })
        } else {
            None
        }
    }
}

/// Boolean
#[derive(Debug, Clone, PartialEq)]
pub struct Bool {
    syn: SyntaxToken,
}

impl AstToken for Bool {
    fn cast_tk(syn: SyntaxToken) -> Option<Self> {
        if matches!(syn.kind(), SyntaxKind::True | SyntaxKind::False) {
            Some(Self { syn })
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn defun() {
        let src = "(def:fn (x y z) (run-some-code))";
        let (ast, errs) = crate::syntax::ast::parse(src);

        if !errs.is_empty() {
            let mut s = String::new();
            for e in errs {
                s.push_str(&format!("{:?}", e));
            }
            panic!(s);
        }

        let defun = ast.items().next().unwrap().as_def_fn().unwrap();
        assert_eq!(defun.params().unwrap().arity(), 3);
    }
}
