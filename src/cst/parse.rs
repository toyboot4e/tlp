/*!
Syntax tree: untyped, homogeneous token tree
*/

use thiserror::Error;

use crate::{
    cst::{
        data::{SyntaxKind, SyntaxNode},
        lex::{self, LexError, Token},
    },
    span::ByteSpan,
};

use rowan::{GreenNode, GreenNodeBuilder};

#[derive(Debug, Clone, Error)]
pub enum ParseError {
    // TODO: use line:column representation
    #[error("It doesn't make any sense: {sp:?}")]
    Unreachable { sp: ByteSpan },
    #[error("{err}")]
    LexError {
        #[from]
        err: LexError,
    },
    #[error("Unexpected EoF while parsing")]
    Eof,
    #[error("Expected {expected}, found {found}")]
    Unexpected { expected: String, found: String },
}

pub fn from_str<'s>(src: &'s str) -> (SyntaxNode, Vec<ParseError>) {
    let (tks, lex_errs) = lex::lex(src);

    let (tree, mut errs) = self::from_tks(src, &tks);
    errs.extend(lex_errs.into_iter().map(ParseError::from));

    (SyntaxNode::new_root(tree), errs)
}

pub fn from_tks<'s, 't>(src: &'s str, tks: &'t [Token]) -> (GreenNode, Vec<ParseError>) {
    let pcx = ParseContext { src, tks };
    let state = ParseState::new();
    state.run(&pcx)
}

/// Referred to as `pcx`
#[derive(Debug, Clone)]
struct ParseContext<'s, 't>
where
    's: 't,
{
    src: &'s str,
    tks: &'t [Token],
}

/// Span of [`Token`] s in range `(lo, hi]` referred to as `tsp`
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
struct TokenSpan {
    pub lo: usize,
    pub hi: usize,
}

/// Referred to as `pcx`.
#[derive(Debug)]
struct ParseState {
    tsp: TokenSpan,
    builder: GreenNodeBuilder<'static>,
    errs: Vec<ParseError>,
}

impl ParseState {
    pub fn new() -> Self {
        Self {
            tsp: Default::default(),
            builder: GreenNodeBuilder::new(),
            errs: vec![],
        }
    }

    pub fn run(mut self, pcx: &ParseContext) -> (GreenNode, Vec<ParseError>) {
        self.builder.start_node(SyntaxKind::ROOT.into());

        while self.tsp.lo < pcx.tks.len() {
            self.maybe_sx(&pcx);
        }

        self.builder.finish_node();

        (self.builder.finish(), self.errs)
    }
}

/// Helpers
impl ParseState {
    #[inline(always)]
    fn peek<'t>(&mut self, pcx: &'t ParseContext) -> Option<&'t Token> {
        if self.tsp.hi < pcx.tks.len() {
            Some(&pcx.tks[self.tsp.hi])
        } else {
            None
        }
    }

    #[inline(always)]
    fn try_peek<'t>(&mut self, pcx: &'t ParseContext) -> Option<&'t Token> {
        match self.peek(pcx) {
            Some(tk) => Some(tk),
            None => {
                self.errs.push(ParseError::Eof);
                None
            }
        }
    }

    /// Consume the next element as a token
    #[inline(always)]
    fn bump(&mut self, pcx: &ParseContext) {
        let tk = &pcx.tks[self.tsp.hi];
        self.builder.token(tk.kind.into(), tk.slice(pcx.src));

        self.tsp.hi += 1;
        self.tsp.lo = self.tsp.hi;
    }

    fn bump_kind(&mut self, pcx: &ParseContext, kind: SyntaxKind) {
        assert_eq!(self.peek(pcx).map(|t| t.kind), Some(kind));
        self.bump(pcx);
    }

    #[inline(always)]
    fn maybe_bump<'t>(&mut self, pcx: &'t ParseContext) -> Option<&'t Token> {
        if self.tsp.hi < pcx.tks.len() {
            let tk = &pcx.tks[self.tsp.hi];
            self.tsp.hi += 1;
            Some(tk)
        } else {
            None
        }
    }

    #[inline(always)]
    fn maybe_bump_kind(&mut self, pcx: &ParseContext, kind: SyntaxKind) -> Option<()> {
        let top = self.peek(pcx)?;
        if top.kind == kind {
            self.bump(pcx);
            Some(())
        } else {
            None
        }
    }

    #[inline(always)]
    fn maybe_bump_ws(&mut self, pcx: &ParseContext) -> Option<()> {
        self.maybe_bump_kind(pcx, SyntaxKind::Ws)
    }
}

/// High-level syntax items
impl ParseState {
    /// sexp → list | symbol
    #[inline(always)]
    pub fn maybe_sx(&mut self, pcx: &ParseContext) -> Option<()> {
        self.maybe_bump_ws(pcx);

        if self.peek(pcx)?.kind == SyntaxKind::LParen {
            self.try_list(pcx);
        } else {
            self.try_symbol(pcx);
        }

        Some(())
    }

    /// list → "(" sexp* ")"
    #[inline(always)]
    fn try_list(&mut self, pcx: &ParseContext) {
        self.builder.start_node(SyntaxKind::List.into());

        self.bump_kind(pcx, SyntaxKind::LParen);
        loop {
            self.maybe_bump_ws(pcx);

            if self.maybe_bump_kind(pcx, SyntaxKind::RParen).is_some() {
                break;
            }

            if self.maybe_sx(pcx).is_none() {
                self.errs.push(ParseError::Eof);
                break;
            }
        }

        self.builder.finish_node();
    }

    /// symbol → atom
    ///
    /// atom → ident | lit
    #[inline(always)]
    fn try_symbol(&mut self, pcx: &ParseContext) {
        if self.maybe_ident(pcx).is_some() || self.maybe_lit(pcx).is_some() {
            return;
        }

        let found = self.peek(pcx);
        self.errs.push(ParseError::Unexpected {
            expected: "symbol".to_string(),
            found: format!("{:?}", found),
        });
        return;
    }
}

/// Lower-level syntactic items
impl ParseState {
    #[inline(always)]
    fn maybe_ident(&mut self, pcx: &ParseContext) -> Option<()> {
        self.maybe_bump_kind(pcx, SyntaxKind::Ident)
    }

    /// lit → num
    #[inline(always)]
    fn maybe_lit(&mut self, pcx: &ParseContext) -> Option<()> {
        self.maybe_bump_kind(pcx, SyntaxKind::Num)
    }
}

#[cfg(test)]
mod test {
    use crate::cst::{self, data::SyntaxElement};

    fn flat_match(src: &str, expected: Vec<&str>) {
        let (tree, errs) = cst::parse::from_str(src);

        if !errs.is_empty() {
            panic!("{:#?}", errs);
        }

        // root
        assert_eq!(format!("{:?}", tree), format!("ROOT@0..{}", src.len()));

        let flat_repr = tree
            .children_with_tokens()
            .flat_map(|elem| match elem {
                SyntaxElement::Node(node) => node
                    .preorder_with_tokens()
                    .filter_map(|ev| match ev {
                        rowan::WalkEvent::Enter(node) => Some(node),
                        rowan::WalkEvent::Leave(_) => None,
                    })
                    .collect::<Vec<_>>(),
                SyntaxElement::Token(_) => vec![elem],
            })
            .map(|child| format!("{:?}@{:?}", child.kind(), child.text_range()))
            .collect::<Vec<_>>();

        assert_eq!(flat_repr, expected);
    }

    #[test]
    fn add() {
        let src = "(+ 1 2)";
        //         0123456

        self::flat_match(
            src,
            vec![
                "List@0..7",
                "LParen@0..1",
                "Ident@1..2",
                "Ws@2..3",
                "Num@3..4",
                "Ws@4..5",
                "Num@5..6",
                "RParen@6..7",
            ],
        );
    }

    #[test]
    fn nest() {
        let src = "(+ 1 (* 2 3))";
        //         0 2 4 6 8 0 2
        //         0         1

        self::flat_match(
            src,
            vec![
                "List@0..13",
                "LParen@0..1",
                "Ident@1..2",
                "Ws@2..3",
                "Num@3..4",
                "Ws@4..5",
                "List@5..12",
                "LParen@5..6",
                "Ident@6..7",
                "Ws@7..8",
                "Num@8..9",
                "Ws@9..10",
                "Num@10..11",
                "RParen@11..12",
                "RParen@12..13",
            ],
        );
    }
}
