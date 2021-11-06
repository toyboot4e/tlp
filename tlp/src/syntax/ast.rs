/*!
Abstract syntax tree, typed tree layered on top of CST

All AST nodes have the same internal structure, i.e, CST. As a result, AST nodes are just wrappers
around CST nodes. Each component is lazily retrieved via accessors traversing the internal CST.
*/

pub mod validate;

pub use crate::syntax::cst::ParseError;

use crate::syntax::cst::{self, SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

const STR_PROC: &'static str = "proc";

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
    fn cast_tk(syn: SyntaxToken) -> Option<Self>;
    fn syntax(&self) -> &SyntaxToken;
}

/// Semantic element casted from syntax element
///
/// RA doesn't have this trait.
pub trait AstElement: Sized {
    /// Method for "syntax pointers"
    fn can_cast(kind: SyntaxKind) -> bool;
    fn cast_elem(syn: SyntaxElement) -> Option<Self>;
    fn syntax(&self) -> SyntaxElement;
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
pub enum Form {
    DefProc(DefProc),
    Call(Call),
    Atom(Atom),
}

impl AstElement for Form {
    fn can_cast(kind: SyntaxKind) -> bool {
        DefProc::can_cast(kind) || Call::can_cast(kind) || Atom::can_cast(kind)
    }

    fn cast_elem(syn: SyntaxElement) -> Option<Self> {
        if let Some(node) = syn.clone().into_node() {
            if let Some(proc) = DefProc::cast_node(node.clone()) {
                return Some(Self::DefProc(proc));
            } else if let Some(call) = Call::cast_node(node.clone()) {
                return Some(Self::Call(call));
            }
        }

        if let Some(atom) = Atom::cast_elem(syn.clone()) {
            Some(Self::Atom(atom))
        } else {
            None
        }
    }

    fn syntax(&self) -> SyntaxElement {
        match self {
            // TODO: maybe don't create the node
            Self::Call(x) => SyntaxElement::Node(x.syntax().clone()),
            Self::DefProc(x) => SyntaxElement::Node(x.syntax().clone()),
            Self::Atom(x) => x.syntax(),
        }
    }
}

/// Function call
#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub(crate) syn: SyntaxNode,
}

impl AstNode for Call {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(kind, SyntaxKind::Call)
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.syn
    }

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

/// (proc name (params?) (block)..)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefProc {
    pub(crate) syn: SyntaxNode,
}

impl AstNode for DefProc {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(kind, SyntaxKind::DefProc)
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.syn
    }

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
                if tk.kind() != SyntaxKind::Ident || tk.text() != STR_PROC {
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
            .find(|t| t.kind() == SyntaxKind::Ident && t.text() == STR_PROC)
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
            .filter(|node| node.kind() == SyntaxKind::Params)
            .map(|node| Params { syn: node.clone() })
    }

    pub fn body_forms(&self) -> impl Iterator<Item = Form> {
        let mut nodes = self
            .syn
            .children_with_tokens()
            .filter(|elem| elem.kind() != SyntaxKind::Ws);

        // skip until parameter
        loop {
            match nodes.next().unwrap() {
                SyntaxElement::Token(t) if t.text() == "proc" => break,
                _ => {}
            }
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
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(kind, SyntaxKind::Params)
    }

    fn cast_tk(syn: SyntaxToken) -> Option<Self> {
        if syn.kind() == SyntaxKind::Ident {
            Some(Self { syn })
        } else {
            None
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        &self.syn
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

/// Atom (any non-list)
#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Literal(Literal),
}

impl AstElement for Atom {
    fn can_cast(kind: SyntaxKind) -> bool {
        Literal::can_cast(kind)
    }

    fn cast_elem(syn: SyntaxElement) -> Option<Self> {
        match syn {
            SyntaxElement::Token(tk) => {
                if let Some(x) = Literal::cast_tk(tk.clone()) {
                    return Some(Self::Literal(x));
                }
            }
            _ => {}
        }

        None
    }

    fn syntax(&self) -> SyntaxElement {
        match self {
            Self::Literal(x) => SyntaxElement::Token(x.syntax().clone()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Num(Num),
    Str(Str),
    Bool(Bool),
}

impl AstToken for Literal {
    fn can_cast(kind: SyntaxKind) -> bool {
        Num::can_cast(kind) || Str::can_cast(kind) || Bool::can_cast(kind)
    }

    fn cast_tk(tk: SyntaxToken) -> Option<Self> {
        None.or_else(|| Num::cast_tk(tk.clone()).map(Self::Num))
            .or_else(|| Str::cast_tk(tk.clone()).map(Self::Str))
            .or_else(|| Bool::cast_tk(tk.clone()).map(Self::Bool))
    }

    fn syntax(&self) -> &SyntaxToken {
        match self {
            Self::Num(x) => x.syntax(),
            Self::Str(x) => x.syntax(),
            Self::Bool(x) => x.syntax(),
        }
    }
}

macro_rules! def_tk {
    ($name:ident, $kind:path $(| $kind2:path)*, $doc:expr) => {
        #[derive(Debug, Clone, PartialEq)]
        #[doc = $doc]
        pub struct $name {
            pub(crate) syn: SyntaxToken,
        }

        impl AstToken for $name {
            fn can_cast(kind: SyntaxKind) -> bool {
                matches!(kind, $kind $(| $kind2)*)
            }

            fn cast_tk(syn: SyntaxToken) -> Option<Self> {
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
    };
}

def_tk!(
    Num,
    SyntaxKind::Num,
    "Untyped, not validated number type (integers and floats)"
);

def_tk!(
    Str,
    SyntaxKind::String,
    "String, including the surroundings"
);

def_tk!(Bool, SyntaxKind::True | SyntaxKind::False, "true | false");

impl Bool {
    pub fn truthy(&self) -> bool {
        match self.syn.text() {
            "true" => true,
            "false" => false,
            _ => unreachable!("bool token has to be true or false"),
        }
    }
}
