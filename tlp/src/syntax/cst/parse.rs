/*!
Tree of tokens
*/

use std::fmt;

use salsa::ParallelDatabase;
use thiserror::Error;

use crate::syntax::{
    cst::{
        data::{SyntaxKind, SyntaxNode},
        lex::{self, LexError, Token},
    },
    span::{ByteLocation, ByteSpan, TextPos},
};

use rowan::{GreenNode, GreenNodeBuilder};

/// Error type accumulated while lexing
#[derive(Debug, Clone, Error, PartialEq, Eq)]
pub enum ParseError {
    // TODO: use line:column representation
    #[error("It doesn't make any sense: {sp:?}")]
    Unreachable { sp: ByteSpan },
    #[error("LexError: {err}")]
    LexError {
        #[from]
        err: LexError,
    },
    /// TODO: context "Expected Symbol as argument, found .."
    #[error("Expected {expected}, found {found}")]
    Unexpected {
        // TODO: add text length
        at: TextPos,
        expected: String,
        found: String,
    },
    #[error("Unterminated string")]
    UnterminatedString { sp: ByteSpan },
    #[error("Path must end with identifier")]
    PathNotEndWithIdent { sp: ByteSpan },
}

/// API for showing diagnostics via LSP
impl ParseError {
    pub fn span(&self) -> ByteSpan {
        match self {
            Self::Unreachable { sp } => sp.clone(),
            Self::LexError { err } => err.span(),
            Self::Unexpected { at: pos, .. } => ByteSpan::at(*pos),
            Self::UnterminatedString { sp } => *sp,
            Self::PathNotEndWithIdent { sp } => *sp,
        }
    }

    pub fn with_loc(&self, src: &str) -> ParseErrorWithLocation {
        ParseErrorWithLocation {
            err: self.clone(),
            loc: ByteLocation::from_pos(self.span().lo, src),
        }
    }
}

/// Display of [`ParseError`] with prefix `ln:col`
///
/// NOTE: In text editors, diagnostics are automatically displayed with `line:column`information, so
/// this type is only for termianl output.
#[derive(Debug, Clone)]
pub struct ParseErrorWithLocation {
    err: ParseError,
    loc: ByteLocation,
}

impl fmt::Display for ParseErrorWithLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{} {}", self.loc.ln, self.loc.col, self.err)
    }
}

/// Creates a CST and optionally errors. It won't fail even if the given input is invalid in toylisp
/// grammer.
pub fn from_str<'s>(src: &'s str) -> (SyntaxNode, Vec<ParseError>) {
    let (tks, lex_errs) = lex::from_str(src);

    let (tree, mut errs) = self::from_tks(src, &tks);
    errs.extend(lex_errs.into_iter().map(ParseError::from));

    (SyntaxNode::new_root(tree), errs)
}

/// Creates a CST and optionally errors from output of lexer
pub fn from_tks<'s, 't>(src: &'s str, tks: &'t [Token]) -> (GreenNode, Vec<ParseError>) {
    let pcx = ParseContext { src, tks };
    let parser = ParseState::new();
    parser.run(&pcx)
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
            self.maybe_bump_ws(pcx);
        }

        self.builder.finish_node();

        (self.builder.finish(), self.errs)
    }
}

/// Helpers
impl ParseState {
    #[inline(always)]
    fn peek<'pcx>(&mut self, pcx: &'pcx ParseContext) -> Option<&'pcx Token> {
        if self.tsp.hi < pcx.tks.len() {
            Some(&pcx.tks[self.tsp.hi])
        } else {
            None
        }
    }

    /// Consume the next element as a token
    #[inline(always)]
    fn bump<'pcx>(&mut self, pcx: &'pcx ParseContext) -> &'pcx Token {
        let tk = &pcx.tks[self.tsp.hi];
        self.builder.token(tk.kind.into(), tk.slice(pcx.src));

        self.tsp.hi += 1;
        self.tsp.lo = self.tsp.hi;
        tk
    }

    fn bump_kind<'pcx>(&mut self, pcx: &'pcx ParseContext, kind: SyntaxKind) -> &'pcx Token {
        assert_eq!(self.peek(pcx).map(|t| t.kind), Some(kind));
        self.bump(pcx)
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
        let mut res = false;

        loop {
            let bump = self
                .maybe_bump_kind(pcx, SyntaxKind::Ws)
                .or_else(|| self.maybe_bump_kind(pcx, SyntaxKind::Comment))
                .is_some();

            if bump {
                res = true;
                continue;
            }

            break;
        }

        if res {
            Some(())
        } else {
            None
        }
    }
}

/// High-level syntax items
impl ParseState {
    /// sexp → list | symbol
    pub fn maybe_sx(&mut self, pcx: &ParseContext) -> Option<()> {
        self.maybe_bump_ws(pcx);

        if self.peek(pcx)?.kind == SyntaxKind::LParen {
            self.always_list(pcx);
        } else {
            self.try_symbol(pcx);
        }

        Some(())
    }

    /// list → Call | DefProc
    ///
    /// TODO: allow unit?
    #[inline(always)]
    fn always_list(&mut self, pcx: &ParseContext) {
        // We don't know the node kind yet, so let's wrap the tokens later
        let checkpoint = self.builder.checkpoint();

        self.bump_kind(pcx, SyntaxKind::LParen);
        self.maybe_bump_ws(pcx);

        let kind = match self.peek(pcx) {
            Some(tk) if tk.kind == SyntaxKind::Ident && tk.slice(pcx.src) == "proc" => {
                self.bump_kind(pcx, SyntaxKind::Ident);
                SyntaxKind::DefProc
            }
            Some(tk) if tk.kind == SyntaxKind::Ident => {
                self.bump_kind(pcx, SyntaxKind::Ident);
                SyntaxKind::Call
            }
            Some(tk) => {
                self.errs.push(ParseError::Unexpected {
                    at: pcx.src.len(),
                    expected: "<call or special form>".to_string(),
                    found: format!("{:?}", tk.kind),
                });

                // FIXME: recovery on wrong form
                return;
            }
            None => {
                self.errs.push(ParseError::Unexpected {
                    at: pcx.src.len(),
                    expected: "<call or special form>".to_string(),
                    found: "EoF".to_string(),
                });

                // FIXME: recovery on wrong form
                return;
            }
        };

        // Now we know the syntax kind: `DefProc`
        // TODO: support more special forms
        self.builder.start_node_at(checkpoint, kind.into());

        // DefProc
        loop {
            if kind != SyntaxKind::DefProc {
                break;
            }
            self.maybe_bump_ws(pcx);

            // proc name
            match self.peek(pcx) {
                Some(tk) if tk.kind == SyntaxKind::Ident => {
                    self.maybe_path_or_ident(pcx).unwrap();
                }
                _ => {
                    break;
                }
            }

            self.maybe_bump_ws(pcx);

            // params
            match self.peek(pcx) {
                Some(tk) if tk.kind == SyntaxKind::LParen => {
                    self.always_param(pcx);
                }
                _ => {}
            }

            break;
        }

        // list
        loop {
            self.maybe_bump_ws(pcx);

            if self.maybe_bump_kind(pcx, SyntaxKind::RParen).is_some() {
                break;
            }

            if self.maybe_sx(pcx).is_none() {
                self.errs.push(ParseError::Unexpected {
                    // todo: figure out why it's at this point
                    // todo: use span for error location
                    at: pcx.src.len(),
                    expected: ")".to_string(),
                    found: "eof".to_string(),
                });
                break;
            }
        }

        self.builder.finish_node();
    }

    fn always_param(&mut self, pcx: &ParseContext) {
        self.builder.start_node(SyntaxKind::Params.into());

        self.bump_kind(pcx, SyntaxKind::LParen);
        // TODO: refactor duplicate loops
        loop {
            self.maybe_bump_ws(pcx);

            if self.maybe_bump_kind(pcx, SyntaxKind::RParen).is_some() {
                break;
            }

            if self.maybe_sx(pcx).is_none() {
                self.errs.push(ParseError::Unexpected {
                    // todo: figure out why it's at this point
                    // todo: use span for error location
                    at: pcx.src.len(),
                    expected: ")".to_string(),
                    found: "eof".to_string(),
                });
                break;
            }
        }

        self.builder.finish_node();
    }

    fn maybe_symbol(&mut self, pcx: &ParseContext) -> Option<()> {
        if self.maybe_path_or_ident(pcx).is_some() || self.maybe_lit(pcx).is_some() {
            Some(())
        } else {
            None
        }
    }

    /// symbol → atom
    ///
    /// atom → ident | lit
    #[inline(always)]
    fn try_symbol(&mut self, pcx: &ParseContext) -> bool {
        if self.maybe_symbol(pcx).is_some() {
            return true;
        }

        match self.peek(pcx) {
            Some(tk) => {
                self.errs.push(ParseError::Unexpected {
                    at: tk.sp.lo,
                    expected: "symbol".to_string(),
                    found: format!("{:?}", tk),
                });

                // NOTE: Discard this symbol so that we won't enter infinite loop
                self.tsp.hi += 1;
                self.tsp.lo = self.tsp.hi;
            }
            None => {
                self.errs.push(ParseError::Unexpected {
                    at: pcx.src.len(),
                    expected: "symbol".to_string(),
                    found: "EoF".to_string(),
                });
            }
        };

        return false;
    }

    /// path → ident ( (:|.)+ ident )*
    fn maybe_path_or_ident(&mut self, pcx: &ParseContext) -> Option<()> {
        self.maybe_bump_ws(pcx);

        let lo = if self.tsp.hi > 0 {
            pcx.tks[self.tsp.hi - 1].sp.hi
        } else {
            0
        };

        let checkpoint = self.builder.checkpoint();
        self.maybe_bump_kind(pcx, SyntaxKind::Ident)?;

        match self.peek(pcx) {
            // path
            Some(tk) if matches!(tk.kind, SyntaxKind::Colon | SyntaxKind::Dot) => {}
            // ident
            _ => return Some(()),
        };

        // path
        self.builder
            .start_node_at(checkpoint, SyntaxKind::Path.into());

        loop {
            self.maybe_bump_ws(pcx);

            // delimiter
            if self.maybe_bump_kind(pcx, SyntaxKind::Colon).is_none()
                && self.maybe_bump_kind(pcx, SyntaxKind::Dot).is_none()
            {
                break;
            }

            self.maybe_bump_ws(pcx);

            // ident
            if self.maybe_bump_kind(pcx, SyntaxKind::Ident).is_none() {
                let hi = match self.peek(pcx) {
                    Some(tk) => tk.sp.lo,
                    None => pcx.src.len(),
                };

                self.errs.push(ParseError::PathNotEndWithIdent {
                    sp: ByteSpan { lo, hi },
                });
                break;
            }
        }

        self.builder.finish_node();
        return Some(());
    }
}

/// Lower-level syntactic items
impl ParseState {
    #[inline(always)]
    fn maybe_ident(&mut self, pcx: &ParseContext) -> Option<()> {
        self.maybe_bump_kind(pcx, SyntaxKind::Ident)
    }

    /// lit → num | str
    #[inline(always)]
    fn maybe_lit(&mut self, pcx: &ParseContext) -> Option<()> {
        if self.maybe_bump_kind(pcx, SyntaxKind::Num).is_some() {
            return Some(());
        }

        if self.maybe_str(pcx).is_some() {
            return Some(());
        }

        None
    }

    #[inline(always)]
    fn maybe_str(&mut self, pcx: &ParseContext) -> Option<()> {
        let top = self.peek(pcx)?;

        if top.kind != SyntaxKind::String {
            return None;
        }

        self.bump_kind(pcx, SyntaxKind::String);

        Some(())
    }
}
