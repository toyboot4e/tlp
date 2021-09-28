use crate::syntax::cst::data::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

const PROC: &'static str = "proc";

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

    pub fn item_nodes(&self) -> impl Iterator<Item = Form> {
        self.syn.children_with_tokens().filter_map(Form::cast_elem)
    }
}

/// Any kind of S-expression, which forms the program
///
/// Call | DefProc | Atom
#[derive(Debug, Clone, PartialEq)]
pub struct Form {
    pub(crate) syn: SyntaxElement,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FormKind {
    Call,
    Proc,
    Atom,
}

impl AstElement for Form {
    fn cast_elem(syn: SyntaxElement) -> Option<Self> {
        if let Some(node) = syn.clone().into_node() {
            if Call::cast_node(node.clone()).is_some() || DefProc::cast_node(node.clone()).is_some()
            {
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
            None.or_else(|| DefProc::cast_node(node.clone()).map(|_| FormKind::Proc))
                .or_else(|| Call::cast_node(node.clone()).map(|_| FormKind::Call))
                .or_else(|| Atom::cast_elem(self.syn.clone()).map(|_| FormKind::Atom))
                .unwrap()
        } else {
            Atom::cast_elem(self.syn.clone())
                .map(|_| FormKind::Atom)
                .unwrap()
        }
    }

    /// WARNING: Cast as DefProc first
    pub fn as_call(&self) -> Option<Call> {
        Call::cast_node(self.syn.clone().into_node()?)
    }

    pub fn as_proc(&self) -> Option<DefProc> {
        DefProc::cast_node(self.syn.clone().into_node()?)
    }

    pub fn as_atom(&self) -> Option<Atom> {
        Atom::cast_elem(self.syn.clone())
    }
}

/// Function call
#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub(crate) syn: SyntaxNode,
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
    pub fn name_tk(&self) -> SyntaxToken {
        match self
            .syn
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .filter(|tk| tk.kind() != SyntaxKind::Ws)
            .next()
        {
            Some(tk) if tk.kind() == SyntaxKind::Ident => tk.clone(),
            Some(tk) => unreachable!("Not identifier? {}", tk),
            None => unreachable!("No token?"),
        }
    }

    pub fn arg_forms(&self) -> impl Iterator<Item = Form> {
        self.syn
            .children_with_tokens()
            .filter(|elem| elem.kind() != SyntaxKind::Ws)
            .filter_map(Form::cast_elem)
    }
}

/// (proc name (params) (block)..)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefProc {
    pub(crate) syn: SyntaxNode,
}

impl AstNode for DefProc {
    fn cast_node(syn: SyntaxNode) -> Option<Self> {
        let mut c = syn.children_with_tokens();

        // (
        if c.next()?.kind() != SyntaxKind::LParen {
            return None;
        }

        let mut c = c.skip_while(|e| e.kind() == SyntaxKind::Ws);

        // proc
        match c.next()? {
            SyntaxElement::Node(_n) => return None,
            SyntaxElement::Token(tk) => {
                if tk.kind() != SyntaxKind::Ident || tk.text() != PROC {
                    return None;
                }
            }
        }

        Some(Self { syn })
    }
}

impl DefProc {
    pub fn proc_tk(&self) -> SyntaxToken {
        self.syn
            .children_with_tokens()
            .filter_map(|e| e.into_token())
            .find(|t| t.kind() == SyntaxKind::Ident && t.text() == PROC)
            .unwrap_or_else(|| unreachable!())
    }

    pub fn name_tk(&self) -> SyntaxToken {
        let mut c = self
            .syn
            .children_with_tokens()
            .filter(|elem| elem.kind() != SyntaxKind::Ws);

        assert_eq!(c.next().unwrap().kind(), SyntaxKind::LParen);

        match c.next().unwrap() {
            SyntaxElement::Token(tk) => {
                assert_eq!(tk.text(), "proc");
            }
            SyntaxElement::Node(_n) => panic!("Not (proc ..) !"),
        };

        match c.next() {
            Some(elem) if elem.kind() == SyntaxKind::Ident => elem.clone().into_token().unwrap(),
            Some(elem) => unreachable!("Not identifier? {}", elem),
            None => unreachable!("No token?"),
        }
    }

    pub fn params(&self) -> Option<Params> {
        self.syn
            .first_child()
            .filter(|node| node.kind() == SyntaxKind::List)
            .map(|node| Params { syn: node.clone() })
    }

    pub fn body(&self) -> impl Iterator<Item = Form> {
        let mut nodes = self
            .syn
            .children_with_tokens()
            .filter(|elem| elem.kind() != SyntaxKind::Ws);

        // skip until parameter
        match nodes.next().unwrap() {
            SyntaxElement::Token(t) => assert_eq!(t.text(), "proc"),
            _ => unreachable!(),
        }

        nodes.filter_map(Form::cast_elem)
    }
}

/// A function parameter
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub(crate) syn: SyntaxToken,
}

impl AstToken for Param {
    fn cast_tk(syn: SyntaxToken) -> Option<Self> {
        if syn.kind() == SyntaxKind::Ident {
            Some(Self { syn })
        } else {
            None
        }
    }
}

impl Param {
    pub fn text(&self) -> &str {
        self.syn.text()
    }
}

/// Function parameters
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Params {
    pub(crate) syn: SyntaxNode,
}

impl Params {
    pub fn param_tks(&self) -> impl Iterator<Item = Param> {
        self.syn
            .children_with_tokens()
            .filter_map(|elem| elem.into_token())
            .filter_map(Param::cast_tk)
    }

    /// Number of arguments
    pub fn arity(&self) -> usize {
        self.param_tks().count()
    }
}

/// Atom = Symbol for now
#[derive(Debug, Clone, PartialEq)]
pub struct Atom {
    pub(crate) syn: SyntaxElement,
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
    pub(crate) syn: SyntaxElement,
}

pub enum SymbolKind {
    /// TODO: Nat, Int, Float
    Num,
    /// String literal
    Str,
    /// `true` | `false`
    Bool,
    // /// ident | path
    // Var
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
    pub(crate) syn: SyntaxToken,
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
    pub(crate) syn: SyntaxNode,
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
    pub(crate) syn: SyntaxToken,
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
