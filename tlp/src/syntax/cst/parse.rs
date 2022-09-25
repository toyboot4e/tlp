//! Tree of tokens
//!
//! # Implementation rules
//!
//! - Parse function users are responsibile of bumping white spaces. Parse functions do not call
//! `maybe_bump_ws` at the beginning.

use base::span::{Offset, Span};
use thiserror::Error;

use crate::syntax::cst::{
    lex::{self, LexError, Token},
    SyntaxKind, SyntaxNode,
};

use rowan::{GreenNode, GreenNodeBuilder};

// FIXME: make self-contained error type

/// Parse / lexing error
#[derive(Debug, Clone, Error, PartialEq, Eq)]
pub enum ParseError {
    // TODO: use line:column representation
    #[error("It doesn't make any sense: {sp:?}")]
    Unreachable { sp: Span },
    #[error("LexError: {err}")]
    LexError {
        #[from]
        err: LexError,
    },
    /// TODO: context "Expected Symbol as argument, found .."
    #[error("Expected {expected}, found `{found}`")]
    Unexpected {
        // TODO: add text length
        at: Offset,
        expected: String,
        found: String,
    },
    #[error("Unterminated string")]
    UnterminatedString { sp: Span },
    #[error("Path must end with identifier")]
    PathNotEndWithIdent { sp: Span },
}

/// API for showing diagnostics via LSP
impl ParseError {
    pub fn span(&self) -> Span {
        match self {
            Self::Unreachable { sp } => sp.clone(),
            Self::LexError { err } => err.span(),
            Self::Unexpected { at: pos, .. } => Span::from(*pos, 1u32),
            Self::UnterminatedString { sp } => *sp,
            Self::PathNotEndWithIdent { sp } => *sp,
        }
    }

    // pub fn with_loc(&self, src: &str) -> ParseErrorWithLocation {
    //     ParseErrorWithLocation {
    //         err: self.clone(),
    //         loc: LineColumn::from_pos(self.span().end, src),
    //     }
    // }
}

// /// Display of [`ParseError`] with prefix `ln:col`
// ///
// /// NOTE: In text editors, diagnostics are automatically displayed with `line:column`information, so
// /// this type is only for termianl output.
// #[derive(Debug, Clone)]
// pub struct ParseErrorWithLocation {
//     err: ParseError,
//     loc: LineColumn,
// }
//
// impl fmt::Display for ParseErrorWithLocation {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "{}:{} {}", self.loc.ln, self.loc.col, self.err)
//     }
// }

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

/// Referred to as `pcx`.
#[derive(Debug)]
struct ParseState {
    tsp: Span,
    builder: GreenNodeBuilder<'static>,
    errs: Vec<ParseError>,
}

impl ParseState {
    pub fn new() -> Self {
        Self {
            tsp: Span::from(0u32, 0u32),
            builder: GreenNodeBuilder::new(),
            errs: vec![],
        }
    }

    pub fn run(mut self, pcx: &ParseContext) -> (GreenNode, Vec<ParseError>) {
        self.builder.start_node(SyntaxKind::ROOT.into());

        self.maybe_bump_ws(pcx);
        while self.tsp.end.into_usize() < pcx.tks.len() {
            self.maybe_bump_sexp(&pcx);
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
        pcx.tks.get(self.tsp.end.into_usize() + n)
    }

    /// Consume the next element as a token
    fn bump<'pcx>(&mut self, pcx: &'pcx ParseContext) -> &'pcx Token {
        let tk = &pcx.tks[self.tsp.end.into_usize()];
        self.builder.token(tk.kind.into(), tk.slice(pcx.src));

        self.tsp.end += 1u32;
        self.tsp.start = self.tsp.end;
        tk
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
    pub fn maybe_bump_sexp(&mut self, pcx: &ParseContext) -> Option<()> {
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
                    at: pcx.src.len().into(),
                    expected: "<call or special form>".to_string(),
                    found: "EoF".to_string(),
                });

                // TODO: recovery
                return;
            }
        };

        // REMARK: keywords other than literals are not lexed
        if !matches!(peek.kind, SyntaxKind::Ident) {
            self.errs.push(ParseError::Unexpected {
                at: pcx.src.len().into(),
                expected: "<call or special form>".to_string(),
                found: format!("{:?}", peek.kind),
            });

            // TODO: recovery
            return;
        }

        match peek.slice(pcx.src) {
            "proc" => self.bump_list_proc(pcx, checkpoint),
            "let" => self.bump_list_let(pcx, checkpoint),
            "set" => self.bump_list_set(pcx, checkpoint),
            "when" | "unless" => self.bump_list_when_unless(pcx, checkpoint),
            "cond" => self.bump_list_cond(pcx, checkpoint),
            "loop" => self.bump_list_loop(pcx, checkpoint),
            "while" => self.bump_list_while(pcx, checkpoint),
            "and" | "or" => self.bump_list_logic_oper(pcx, checkpoint),
            _ => self.bump_list_call(pcx, checkpoint),
        }
    }

    /// list-proc → "proc" Ident params form* ")"
    ///
    /// - params → "(" param ")"
    /// - param → Pat ":" Ident
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
            self.builder.start_node(SyntaxKind::Block.into());

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

    /// Ident Params
    fn _bump_to_proc_param(&mut self, pcx: &ParseContext) {
        // proc name
        if let Some(tk) = self.peek(pcx) {
            if tk.kind == SyntaxKind::Ident {
                self.builder.start_node(SyntaxKind::ProcName.into());
                self.bump_kind(pcx, SyntaxKind::Ident);
                self.builder.finish_node();
            }
        } else {
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
    }

    /// Param* ")"
    fn _proc_params(&mut self, pcx: &ParseContext) {
        self.builder.start_node(SyntaxKind::Params.into());
        self.bump_kind(pcx, SyntaxKind::LParen);

        loop {
            self.maybe_bump_ws(pcx);

            if self.maybe_bump_kind(pcx, SyntaxKind::RParen).is_some() {
                break;
            }

            let checkpoint = self.builder.checkpoint();
            if self.maybe_bump_pat(pcx).is_some() {
                self.builder
                    .start_node_at(checkpoint, SyntaxKind::Param.into());

                // `:`
                self.maybe_bump_ws(pcx);
                if self.maybe_bump_kind(pcx, SyntaxKind::Colon).is_none() {
                    let text = self.peek(pcx).unwrap().slice(pcx.src);

                    self.errs.push(ParseError::Unexpected {
                        at: pcx.src.len().into(),
                        expected: ":".to_string(),
                        found: text.to_string(),
                    });

                    self.maybe_bump_sexp(pcx);
                } else {
                    // `<Type>`
                    self.maybe_bump_ws(pcx);
                    self.maybe_bump_type(pcx);
                }

                self.builder.finish_node();
            } else {
                let text = self.peek(pcx).unwrap().slice(pcx.src);

                self.errs.push(ParseError::Unexpected {
                    // TODO: figure out why it's at this point
                    // TODO: use span for error location
                    at: pcx.src.len().into(),
                    expected: "`)` or parameter".to_string(),
                    found: text.to_string(),
                });

                // anyway consume the syntax
                self.maybe_bump_sexp(pcx);
            }
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

            if self.maybe_bump_sexp(pcx).is_none() {
                self.errs.push(ParseError::Unexpected {
                    // todo: figure out why it's at this point
                    // todo: use span for error location
                    at: pcx.src.len().into(),
                    expected: ")".to_string(),
                    found: "eof".to_string(),
                });
                return None;
            }
        }
    }

    fn maybe_bump_type(&mut self, pcx: &ParseContext) {
        if self.maybe_bump_path(pcx).is_some() {
            return;
        }

        let text = if let Some(peek) = self.peek(pcx) {
            peek.slice(pcx.src).to_string()
        } else {
            "<EoF>".to_string()
        };

        self.errs.push(ParseError::Unexpected {
            at: pcx.src.len().into(),
            expected: "<type>".to_string(),
            found: text,
        });
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

    /// "set" place Sexp ")"
    fn bump_list_set(&mut self, pcx: &ParseContext, checkpoint: rowan::Checkpoint) {
        let tk = self.bump_kind(pcx, SyntaxKind::Ident);
        assert_eq!(tk.slice(pcx.src), "set");

        // wrap the list
        self.builder
            .start_node_at(checkpoint, SyntaxKind::Set.into());

        // place
        self.maybe_bump_ws(pcx);
        if self.maybe_bump_path(pcx).is_none() {
            let err = ParseError::Unexpected {
                at: pcx.src.len().into(),
                expected: "<place>".to_string(),
                found: format!("{:?}", self.peek(pcx)),
            };
            self.errs.push(err);
        }

        // rhs
        self.maybe_bump_ws(pcx);
        self.maybe_bump_sexp(pcx);

        // ")"
        self.maybe_bump_ws(pcx);
        self.maybe_bump_kind(pcx, SyntaxKind::RParen);

        self.builder.finish_node();
    }

    /// ("and" | "or") Pat Sexp* ")"
    fn bump_list_logic_oper(&mut self, pcx: &ParseContext, checkpoint: rowan::Checkpoint) {
        let tk = self.bump_kind(pcx, SyntaxKind::Ident);

        let kind = match tk.slice(pcx.src) {
            "and" => SyntaxKind::And,
            "or" => SyntaxKind::Or,
            x => unreachable!("not `and` or `or`: {}", x),
        };

        self._bump_rest_list_wrapping(pcx, checkpoint, kind);
    }

    /// When → "when" Sexp* ")"
    ///
    /// Unless → "unless" Sexp* ")"
    fn bump_list_when_unless(&mut self, pcx: &ParseContext, checkpoint: rowan::Checkpoint) {
        let tk = self.bump_kind(pcx, SyntaxKind::Ident);

        let kind = match tk.slice(pcx.src) {
            "when" => SyntaxKind::When,
            "unless" => SyntaxKind::Unless,
            x => unreachable!("not `and` or `or`: {}", x),
        };

        self.maybe_bump_ws(pcx);

        // pred
        self.maybe_bump_sexp(pcx);
        self.maybe_bump_ws(pcx);

        // inline block
        let block_checkpoint = self.builder.checkpoint();
        self._bump_to_r_paren_wrapping(pcx, block_checkpoint, SyntaxKind::Block);

        self._bump_rest_list_wrapping(pcx, checkpoint, kind);
    }

    /// Cond → "cond" CondCase* ")"
    fn bump_list_cond(&mut self, pcx: &ParseContext, checkpoint: rowan::Checkpoint) {
        let tk = self.bump_kind(pcx, SyntaxKind::Ident);
        assert_eq!(tk.slice(pcx.src), "cond");

        // CondCase *
        loop {
            self.maybe_bump_ws(pcx);
            let checkpoint = self.builder.checkpoint();
            if self.maybe_bump_cond_case(pcx, checkpoint).is_none() {
                break;
            }
        }

        if self.maybe_bump_kind(pcx, SyntaxKind::RParen).is_none() {
            // TODO: better recovery on failure
        }

        self._bump_rest_list_wrapping(pcx, checkpoint, SyntaxKind::Cond);
    }

    /// CondCase → "(" sexp InlineBlock ")"
    fn maybe_bump_cond_case(
        &mut self,
        pcx: &ParseContext,
        checkpoint: rowan::Checkpoint,
    ) -> Option<()> {
        if self.maybe_bump_kind(pcx, SyntaxKind::LParen).is_none() {
            return None;
        }

        self.builder
            .start_node_at(checkpoint, SyntaxKind::CondCase.into());

        {
            // pred
            self.maybe_bump_sexp(pcx);
            self.maybe_bump_ws(pcx);

            // inline block
            let block_checkpoint = self.builder.checkpoint();
            self._bump_to_r_paren_wrapping(pcx, block_checkpoint, SyntaxKind::Block);

            self.maybe_bump_ws(pcx);
            if self.maybe_bump_kind(pcx, SyntaxKind::RParen).is_none() {
                // TODO: better recovery on failure
            }
        }

        self.builder.finish_node();
        Some(())
    }

    /// "loop" sexp* ")"
    fn bump_list_loop(&mut self, pcx: &ParseContext, checkpoint: rowan::Checkpoint) {
        let tk = self.bump_kind(pcx, SyntaxKind::Ident);
        assert_eq!(tk.slice(pcx.src), "loop");

        self.builder
            .start_node_at(checkpoint, SyntaxKind::Loop.into());

        self.maybe_bump_ws(pcx);

        // inline block
        let block_checkpoint = self.builder.checkpoint();
        self._bump_to_r_paren_wrapping(pcx, block_checkpoint, SyntaxKind::Block);

        if self.maybe_bump_kind(pcx, SyntaxKind::RParen).is_none() {
            // TODO: better recovery on failure
        }

        self.builder.finish_node();
    }

    /// "while" sexp sexp* ")"
    fn bump_list_while(&mut self, pcx: &ParseContext, checkpoint: rowan::Checkpoint) {
        let tk = self.bump_kind(pcx, SyntaxKind::Ident);
        assert_eq!(tk.slice(pcx.src), "while");

        self.builder
            .start_node_at(checkpoint, SyntaxKind::While.into());

        self.maybe_bump_ws(pcx);

        // pred
        if self.maybe_bump_sexp(pcx).is_none() {
            // TODO: better recovery on failure
        }

        self.maybe_bump_ws(pcx);

        // inline block
        let block_checkpoint = self.builder.checkpoint();
        self._bump_to_r_paren_wrapping(pcx, block_checkpoint, SyntaxKind::Block);

        if self.maybe_bump_kind(pcx, SyntaxKind::RParen).is_none() {
            // TODO: better recovery on failure
        }

        self.builder.finish_node();
    }

    /// Call → "(" Path Sexp* ")"
    fn bump_list_call(&mut self, pcx: &ParseContext, checkpoint: rowan::Checkpoint) {
        self.maybe_bump_path(pcx).unwrap();
        self._bump_rest_list_wrapping(pcx, checkpoint, SyntaxKind::Call);
    }

    /// Bumps to just before `)`
    fn _bump_to_r_paren_wrapping(
        &mut self,
        pcx: &ParseContext,
        checkpoint: rowan::Checkpoint,
        kind: SyntaxKind,
    ) {
        self.builder.start_node_at(checkpoint, kind.into());

        {
            self.maybe_bump_ws(pcx);
            self._bump_sexps_to_end_paren(pcx);
        }

        self.builder.finish_node();
    }

    /// Bumps to `)`
    fn _bump_rest_list_wrapping(
        &mut self,
        pcx: &ParseContext,
        checkpoint: rowan::Checkpoint,
        kind: SyntaxKind,
    ) {
        self.builder.start_node_at(checkpoint, kind.into());

        {
            self.maybe_bump_ws(pcx);

            if self._bump_sexps_to_end_paren(pcx).is_some() {
                self.maybe_bump_kind(pcx, SyntaxKind::RParen);
            }
        }

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
                    at: tk.sp.start,
                    expected: "symbol".to_string(),
                    found: format!("{:?}", tk),
                });

                // Discard this token so that we won't enter infinite loop
                self.tsp.end += 1u32;
                self.tsp.start = self.tsp.end;
            }
            None => {
                self.errs.push(ParseError::Unexpected {
                    at: pcx.src.len().into(),
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
        let start = if self.tsp.end.into_usize() > 0 {
            pcx.tks[self.tsp.end.into_usize() - 1].sp.end
        } else {
            Offset::default()
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
                let end = match self.peek(pcx) {
                    Some(tk) => tk.sp.start,
                    None => pcx.src.len().into(),
                };

                self.errs.push(ParseError::PathNotEndWithIdent {
                    sp: Span { start, end },
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

        loop {
            if self.maybe_bump_kind(pcx, SyntaxKind::Num).is_some() {
                break;
            }

            if self.maybe_bump_kind(pcx, SyntaxKind::True).is_some() {
                break;
            }

            if self.maybe_bump_kind(pcx, SyntaxKind::False).is_some() {
                break;
            }

            if self.maybe_bump_str(pcx).is_some() {
                break;
            }

            return None;
        }

        self.builder
            .start_node_at(checkpoint, SyntaxKind::Literal.into());
        self.builder.finish_node();

        Some(())
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
