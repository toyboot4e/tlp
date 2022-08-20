//! Tree of tokens
//!
//! # Implementation rules
//!
//! - Parse function users are responsibile of bumping white spaces. Parse functions do not call
//! `maybe_bump_ws` at the beginning.

use std::fmt;

use thiserror::Error;

use crate::syntax::{
    cst::{
        lex::{self, LexError, Token},
        SyntaxKind, SyntaxNode,
    },
    span::{ByteLocation, ByteSpan, TextPos},
};

use rowan::{GreenNode, GreenNodeBuilder};

/// Parse / lexing error
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
pub fn parse_str<'s>(src: &'s str) -> (SyntaxNode, Vec<ParseError>) {
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

        self.maybe_bump_ws(pcx);
        while self.tsp.lo < pcx.tks.len() {
            self.maybe_bump_sepx(&pcx);
            self.maybe_bump_ws(pcx);
        }

        self.builder.finish_node();

        (self.builder.finish(), self.errs)
    }
}

/// Helpers
impl ParseState {
    fn peek<'pcx>(&mut self, pcx: &'pcx ParseContext) -> Option<&'pcx Token> {
        self.peek_n(pcx, 0)
    }

    fn peek_n<'pcx>(&mut self, pcx: &'pcx ParseContext, n: usize) -> Option<&'pcx Token> {
        pcx.tks.get(self.tsp.hi + n)
    }

    /// Consume the next element as a token
    fn bump<'pcx>(&mut self, pcx: &'pcx ParseContext) -> &'pcx Token {
        let tk = &pcx.tks[self.tsp.hi];
        self.builder.token(tk.kind.into(), tk.slice(pcx.src));

        self.tsp.hi += 1;
        self.tsp.lo = self.tsp.hi;
        tk
    }

    fn bump_as_node<'pcx>(&mut self, pcx: &'pcx ParseContext, node_kind: SyntaxKind) {
        self.builder.start_node(node_kind.into());
        self.bump(pcx);
        self.builder.finish_node();
    }

    fn bump_kind<'pcx>(&mut self, pcx: &'pcx ParseContext, kind: SyntaxKind) -> &'pcx Token {
        assert_eq!(self.peek(pcx).map(|t| t.kind), Some(kind));
        self.bump(pcx)
    }

    fn maybe_bump_kind(&mut self, pcx: &ParseContext, kind: SyntaxKind) -> Option<()> {
        let top = self.peek(pcx)?;
        if top.kind == kind {
            self.bump(pcx);
            Some(())
        } else {
            None
        }
    }

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
            } else {
                break;
            }
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
    pub fn maybe_bump_sepx(&mut self, pcx: &ParseContext) -> Option<()> {
        if self.peek(pcx)?.kind == SyntaxKind::LParen {
            self.bump_list(pcx);
        } else {
            self.try_bump_symbol(pcx);
        }

        Some(())
    }

    /// list → Call | Let | DefProc
    fn bump_list(&mut self, pcx: &ParseContext) {
        // We don't know the node kind yet, so let's wrap the tokens later
        let checkpoint = self.builder.checkpoint();

        self.bump_kind(pcx, SyntaxKind::LParen);
        self.maybe_bump_ws(pcx);

        let peek = match self.peek(pcx) {
            Some(tk) => tk,
            None => {
                self.errs.push(ParseError::Unexpected {
                    at: pcx.src.len(),
                    expected: "<call or special form>".to_string(),
                    found: "EoF".to_string(),
                });

                // TODO: recovery
                return;
            }
        };

        if peek.kind != SyntaxKind::Ident {
            self.errs.push(ParseError::Unexpected {
                at: pcx.src.len(),
                expected: "<call or special form>".to_string(),
                found: format!("{:?}", peek.kind),
            });

            // TODO: recovery
            return;
        }

        match peek.slice(pcx.src) {
            "proc" => self.bump_list_proc(pcx, checkpoint),
            "let" => self.bump_list_let(pcx, checkpoint),
            _ => self.bump_list_call(pcx, checkpoint),
        }
    }

    /// list-proc → "proc" Ident params form* ")"
    /// params → "(" ")"
    fn bump_list_proc(&mut self, pcx: &ParseContext, checkpoint: rowan::Checkpoint) {
        let tk = self.bump_kind(pcx, SyntaxKind::Ident);
        assert_eq!(tk.slice(pcx.src), "proc");

        // wrap the list
        self.builder
            .start_node_at(checkpoint, SyntaxKind::DefProc.into());
        self.maybe_bump_ws(pcx);

        let found_rparen = {
            // start `Body` node
            self._bump_to_proc_param(pcx);

            // maybe other list items
            let found_rparen = self._bump_sexps_to_end_paren(pcx).is_some();

            // end `Body` node just before `)`
            self.builder.finish_node();

            found_rparen
        };

        if found_rparen {
            self.maybe_bump_kind(pcx, SyntaxKind::RParen);
        }

        // end `DefProc`
        self.builder.finish_node();
    }

    /// Parses params and starts proc body
    fn _bump_to_proc_param(&mut self, pcx: &ParseContext) {
        // proc name
        if let Some(tk) = self.peek(pcx) {
            if tk.kind == SyntaxKind::Ident {
                self.builder.start_node(SyntaxKind::ProcName.into());
                self.bump_kind(pcx, SyntaxKind::Ident);
                self.builder.finish_node();
            }
        } else {
            self.builder.start_node(SyntaxKind::Block.into());
            return;
        }

        self.maybe_bump_ws(pcx);

        // params
        if let Some(tk) = self.peek(pcx) {
            if tk.kind == SyntaxKind::LParen {
                self._proc_params(pcx);
                self.maybe_bump_ws(pcx);
            }
        }

        self.builder.start_node(SyntaxKind::Block.into());
    }

    fn _proc_params(&mut self, pcx: &ParseContext) {
        self.builder.start_node(SyntaxKind::Params.into());
        self.bump_kind(pcx, SyntaxKind::LParen);

        loop {
            self.maybe_bump_ws(pcx);

            if self.maybe_bump_kind(pcx, SyntaxKind::RParen).is_some() {
                break;
            }

            match self.peek(pcx) {
                Some(tk) if tk.kind == SyntaxKind::Ident => {
                    self.bump_as_node(pcx, SyntaxKind::Param);
                    continue;
                }
                _ => {}
            }

            self.errs.push(ParseError::Unexpected {
                // todo: figure out why it's at this point
                // todo: use span for error location
                at: pcx.src.len(),
                expected: ")".to_string(),
                found: "eof".to_string(),
            });

            // consume the syntax
            self.maybe_bump_sepx(pcx);
        }

        self.builder.finish_node();
    }

    /// Sexp* ")"
    ///
    /// Advances until it peeks a right paren. Returns if the end of the list was found.
    fn _bump_sexps_to_end_paren(&mut self, pcx: &ParseContext) -> Option<()> {
        loop {
            self.maybe_bump_ws(pcx);

            let peek = self.peek(pcx)?;
            if peek.kind == SyntaxKind::RParen {
                return Some(());
            }

            if self.maybe_bump_sepx(pcx).is_none() {
                self.errs.push(ParseError::Unexpected {
                    // todo: figure out why it's at this point
                    // todo: use span for error location
                    at: pcx.src.len(),
                    expected: ")".to_string(),
                    found: "eof".to_string(),
                });
                return None;
            }
        }
    }

    /// "let" Pat Expr ")"
    fn bump_list_let(&mut self, pcx: &ParseContext, checkpoint: rowan::Checkpoint) {
        let tk = self.bump_kind(pcx, SyntaxKind::Ident);
        assert_eq!(tk.slice(pcx.src), "let");

        self.maybe_bump_ws(pcx);

        // wrap the list
        self.builder
            .start_node_at(checkpoint, SyntaxKind::Let.into());

        self.maybe_bump_pat(pcx);
        self.maybe_bump_ws(pcx);

        if self._bump_sexps_to_end_paren(pcx).is_some() {
            self.maybe_bump_kind(pcx, SyntaxKind::RParen);
        }

        // end `Let`
        self.builder.finish_node();
    }

    /// Call → "(" Path Sexp* ")"
    fn bump_list_call(&mut self, pcx: &ParseContext, checkpoint: rowan::Checkpoint) {
        self.maybe_bump_path(pcx).unwrap();

        // wrap the list
        self.builder
            .start_node_at(checkpoint, SyntaxKind::Call.into());
        self.maybe_bump_ws(pcx);

        if self._bump_sexps_to_end_paren(pcx).is_some() {
            self.maybe_bump_kind(pcx, SyntaxKind::RParen);
        }

        // end `Call`
        self.builder.finish_node();
    }

    fn try_bump_symbol(&mut self, pcx: &ParseContext) -> bool {
        if self.maybe_bump_symbol(pcx).is_some() {
            return true;
        }

        // emit error
        match self.peek(pcx) {
            Some(tk) => {
                self.errs.push(ParseError::Unexpected {
                    at: tk.sp.lo,
                    expected: "symbol".to_string(),
                    found: format!("{:?}", tk),
                });

                // Discard this token so that we won't enter infinite loop
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

    /// Symbol → Path | Literal
    fn maybe_bump_symbol(&mut self, pcx: &ParseContext) -> Option<()> {
        if self.maybe_bump_path(pcx).is_some() || self.maybe_bump_lit(pcx).is_some() {
            Some(())
        } else {
            None
        }
    }

    /// Path node
    // TODO: refactor
    fn maybe_bump_path(&mut self, pcx: &ParseContext) -> Option<()> {
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
            // ident (wrap as Path node)
            _ => {
                self.builder
                    .start_node_at(checkpoint, SyntaxKind::Path.into());
                self.builder.finish_node();
                return Some(());
            }
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

    /// Pat → PatIdent | PatPath
    fn maybe_bump_pat(&mut self, pcx: &ParseContext) -> Option<()> {
        let checkpoint = self.builder.checkpoint();

        self.maybe_bump_kind(pcx, SyntaxKind::Ident)?;

        // TODO: handle path
        self.builder
            .start_node_at(checkpoint, SyntaxKind::PatIdent.into());
        self.builder.finish_node();

        Some(())
    }
}

/// Lower-level syntactic items
impl ParseState {
    /// Literal → Number | String
    fn maybe_bump_lit(&mut self, pcx: &ParseContext) -> Option<()> {
        let checkpoint = self.builder.checkpoint();

        let wrap = loop {
            if self.maybe_bump_kind(pcx, SyntaxKind::Num).is_some() {
                break true;
            }

            if self.maybe_bump_str(pcx).is_some() {
                break true;
            }

            break false;
        };

        if wrap {
            self.builder
                .start_node_at(checkpoint, SyntaxKind::Literal.into());
            self.builder.finish_node();
            Some(())
        } else {
            None
        }
    }

    fn maybe_bump_str(&mut self, pcx: &ParseContext) -> Option<()> {
        let top = self.peek(pcx)?;

        if top.kind != SyntaxKind::Str {
            return None;
        }

        self.bump_kind(pcx, SyntaxKind::Str);

        Some(())
    }
}
