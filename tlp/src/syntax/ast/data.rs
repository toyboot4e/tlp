use crate::syntax::cst::data::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

const STR_PROC: &'static str = "proc";

/// Semantic node casted from syntax node
pub trait AstNode: Sized {
    fn cast_node(syn: SyntaxNode) -> Option<Self>;
    fn syntax(&self) -> &SyntaxNode;
}

/// Semantic token casted from syntax token
pub trait AstToken: Sized {
    fn cast_tk(syn: SyntaxToken) -> Option<Self>;
    fn syntax(&self) -> &SyntaxToken;
}

/// Semantic element casted from syntax element
pub trait AstElement: Sized {
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
    fn cast_elem(syn: SyntaxElement) -> Option<Self> {
        if let Some(x) = Literal::cast_elem(syn.clone()) {
            return Some(Self::Literal(x));
        }

        None
    }

    fn syntax(&self) -> SyntaxElement {
        match self {
            Self::Literal(x) => x.syntax(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Num(Num),
    Str(Str),
    Bool(Bool),
}

impl AstElement for Literal {
    fn cast_elem(syn: SyntaxElement) -> Option<Self> {
        if let Some(num) = Num::cast_elem(syn.clone()) {
            return Some(Self::Num(num));
        }

        match syn {
            SyntaxElement::Node(n) => Str::cast_node(n).map(Self::Str),
            SyntaxElement::Token(t) => Bool::cast_tk(t).map(Self::Bool),
        }
    }

    fn syntax(&self) -> SyntaxElement {
        match self {
            Self::Num(x) => x.syntax(),
            Self::Str(s) => SyntaxElement::Node(s.syntax().clone()),
            Self::Bool(b) => SyntaxElement::Token(b.syntax().clone()),
        }
    }
}

/// Temp access to the literal content
#[derive(Clone, Debug, PartialEq)]
pub enum LiteralKind {
    Str(Str),
    // ByteString(ast::ByteString),
    // IntNumber(IntNumber),
    // FloatNumber(FloatNumber),
    // Char,
    // Byte,
    Bool(bool),
}

/// Untyped, not validated number type (integers and floats)
#[derive(Debug, Clone, PartialEq)]
pub struct Num {
    pub(crate) syn: SyntaxElement,
}

impl AstElement for Num {
    fn cast_elem(syn: SyntaxElement) -> Option<Self> {
        if syn.kind() == SyntaxKind::Num {
            Some(Self { syn })
        } else {
            None
        }
    }

    fn syntax(&self) -> SyntaxElement {
        self.syn.clone()
    }
}

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

    fn syntax(&self) -> &SyntaxNode {
        &self.syn
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Bool {
    pub(crate) syn: SyntaxToken,
}

impl AstToken for Bool {
    fn cast_tk(syn: SyntaxToken) -> Option<Self> {
        match syn.kind() {
            SyntaxKind::True => Some(Self { syn }),
            SyntaxKind::False => Some(Self { syn }),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxToken {
        &self.syn
    }
}

impl Bool {
    pub fn truthy(&self) -> bool {
        match self.syn.text() {
            "true" => true,
            "false" => false,
            _ => unreachable!("bool token has to be true or false"),
        }
    }
}
