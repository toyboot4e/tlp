//! Tree of tokens
//!
//! # Implementation rules
//!
//! - Parse function users are responsibile of bumping white spaces. Parse functions do not call
//! `maybe_bump_ws` at the beginning.

use std::fmt;

use base::span::{Offset, Span};
use thiserror::Error;

use crate::syntax::cst::{lex::Token, SyntaxKind, SyntaxNode};

use rowan::{GreenNode, GreenNodeBuilder};

// FIXME: make self-contained error type

/// Parse / lexing error
#[derive(Debug, Clone, Error, PartialEq, Eq)]
pub enum ParseError {
    #[error("expected {expected}, found `{found:?}` while parsing `{ctx:?}`")]
    UnexpectedToken {
        expected: String,
        found: Token,
        ctx: ErrorContext,
    },
    #[error("expected {expected}, found EoF")]
    UnexpectedEof { expected: String },
    #[error("unclosed parentheses")]
    UnclosedParentheses { span: Span },
    #[error("path must end with identifier")]
    PathNotEndWithIdent { span: Span },
    #[error("missing procedure name")]
    MissingProcName { ctx: ProcErrorContext },
}

/// FIXME: Duplicate messages
impl ParseError {
    /// Returns a simplified error message, useful for diagnostic header
    pub fn detailed_message(&self, src: &str) -> String {
        match self {
            ParseError::UnexpectedToken {
                expected,
                found,
                ctx,
            } => format!(
                "expected {expected}, found `{}` in {}",
                found.span.slice(src),
                ctx.format(src),
            ),
            ParseError::UnexpectedEof { expected } => format!("expected {expected}"),
            ParseError::UnclosedParentheses { .. } => "unclosed parentheses".to_string(),
            ParseError::PathNotEndWithIdent { .. } => "path must end with identifier".to_string(),
            ParseError::MissingProcName { .. } => "missing procedure name".to_string(),
        }
    }

    /// Returns a simplified error message, useful when quoting source text.
    pub fn simple_message(&self, _src: &str) -> String {
        match self {
            ParseError::UnexpectedToken { expected, .. } => format!("expected {expected}"),
            ParseError::UnexpectedEof { expected } => format!("expected {expected}"),
            ParseError::UnclosedParentheses { .. } => "unclosed parentheses".to_string(),
            ParseError::PathNotEndWithIdent { .. } => "path must end with identifier".to_string(),
            ParseError::MissingProcName { .. } => "missing procedure name".to_string(),
        }
    }
}

/// Surrounding context of a [`ParseError`], e.g., "while parsing a procedure"
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorContext {
    /// Top-level parse entry. TODO: Needs more context
    Sexp,
    /// While parsing a list
    List {
        /// Span of the `(`
        l_paren_span: Span,
    },
    /// While parsing a procedure
    Proc(ProcErrorContext),
    /// While parsing a `let` statement
    Let {
        /// Span of the `let` identifier
        let_span: Span,
    },
    /// While parsing a procedure call
    Call {
        /// Span of the `<callee identifier>`
        callee_span: Span,
    },
    /// While parsing a `set` statement
    Set {
        /// Span of the `set` identifier
        set_span: Span,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ProcErrorContext {
    /// Span of the `proc` keyword
    pub proc_span: Span,
    /// Span of the `<procedure name>`
    pub name_span: Option<Span>,
}

impl ProcErrorContext {
    pub fn into_ctx(self) -> ErrorContext {
        ErrorContext::Proc(self)
    }
}

impl ErrorContext {
    /// Casts into format type for error context printing
    pub fn format<'s, 'c>(&'c self, source: &'s str) -> ErrorContextWithSource<'s, 'c> {
        ErrorContextWithSource { source, ctx: self }
    }
}

pub struct ErrorContextWithSource<'s, 'c> {
    source: &'s str,
    ctx: &'c ErrorContext,
}

impl<'s, 'c> fmt::Display for ErrorContextWithSource<'s, 'c> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match &self.ctx {
            ErrorContext::Sexp => "S-expression",
            ErrorContext::List { .. } => "list",
            ErrorContext::Proc(ctx) => {
                if let Some(name_span) = ctx.name_span {
                    return write!(f, "procedure `{}`", name_span.slice(self.source));
                } else {
                    return write!(f, "<name-missing procedure>");
                }
            }
            ErrorContext::Let { .. } => "let",
            ErrorContext::Call { .. } => "call",
            ErrorContext::Set { .. } => "set",
        };

        write!(f, "{}", s)
    }
}

/// Creates a CST
pub fn parse<'s, 't>(src: &'s str, tks: &'t [Token]) -> (SyntaxNode, Vec<ParseError>) {
    let pcx = ParseContext { src, tks };
    let parser = ParseState::new();
    let (root, errs) = parser.run(&pcx);
    (SyntaxNode::new_root(root), errs)
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
    /// Token span
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

        let paren_start = self.tsp.end;
        let l_paren_tk = self.bump_kind(pcx, SyntaxKind::LParen);
        self.maybe_bump_ws(pcx);

        let peek = match self.peek(pcx) {
            Some(tk) => tk,
            None => {
                self.errs.push(ParseError::UnclosedParentheses {
                    span: Span {
                        start: paren_start,
                        end: pcx.src.len().into(),
                    },
                });

                // TODO: recovery
                return;
            }
        };

        // REMARK: keywords other than literals are not lexed
        if !matches!(peek.kind, SyntaxKind::Ident) {
            self.errs.push(ParseError::UnexpectedToken {
                expected: "<call or special form>".to_string(),
                found: *peek,
                ctx: ErrorContext::List {
                    l_paren_span: l_paren_tk.span,
                },
            });

            // TODO: for recovery, go to `)`?
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
        // for diagnostics
        let list_start = self.tsp.end;

        let proc_tk = self.bump_kind(pcx, SyntaxKind::Ident);
        assert_eq!(proc_tk.slice(pcx.src), "proc");

        // wrap the list
        self.builder
            .start_node_at(checkpoint, SyntaxKind::DefProc.into());

        let found_rparen = {
            // start `Body` node
            self.maybe_bump_ws(pcx);
            let name_span = self._bump_to_proc_param(pcx, proc_tk.span);

            let ctx = ErrorContext::Proc(ProcErrorContext {
                proc_span: proc_tk.span,
                name_span,
            });

            self.maybe_bump_ws(pcx);
            self._maybe_bump_return_type(pcx, ctx);

            self.maybe_bump_ws(pcx);
            self.builder.start_node(SyntaxKind::Block.into());

            // maybe other list items
            let found_rparen = if self._bump_sexps_to_end_paren(pcx).is_ok() {
                true
            } else {
                self.errs.push(ParseError::UnclosedParentheses {
                    span: Span {
                        start: list_start,
                        end: self.tsp.end,
                    },
                });
                false
            };

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
    ///
    /// Returns name token's span if it exists.
    fn _bump_to_proc_param(&mut self, pcx: &ParseContext, proc_span: Span) -> Option<Span> {
        let (ctx, name_span) = {
            let name_tk = self.peek(pcx)?;

            if name_tk.kind == SyntaxKind::Ident {
                self.builder.start_node(SyntaxKind::ProcName.into());
                self.bump_kind(pcx, SyntaxKind::Ident);
                self.builder.finish_node();

                (
                    ProcErrorContext {
                        proc_span,
                        name_span: Some(name_tk.span),
                    },
                    Some(name_tk.span),
                )
            } else {
                (
                    ProcErrorContext {
                        proc_span,
                        name_span: None,
                    },
                    None,
                )
            }
        };

        if name_span.is_none() {
            self.errs.push(ParseError::MissingProcName { ctx });
        }

        self.maybe_bump_ws(pcx);

        // params
        if let Some(tk) = self.peek(pcx) {
            if tk.kind == SyntaxKind::LParen {
                self._proc_params(pcx, ctx);
                self.maybe_bump_ws(pcx);
            }
        }

        name_span
    }

    /// Param* ")"
    fn _proc_params(&mut self, pcx: &ParseContext, ctx: ProcErrorContext) {
        let ctx = ErrorContext::Proc(ctx);

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

                self.maybe_bump_ws(pcx);

                // `:`
                if self.maybe_bump_kind(pcx, SyntaxKind::Colon).is_none() {
                    let tk = self.peek(pcx).unwrap();

                    // FIXME: duplicate of item pass errors
                    self.errs.push(ParseError::UnexpectedToken {
                        expected: "`:`".to_string(),
                        found: *tk,
                        ctx,
                    });
                } else {
                    // `<Type>`
                    self.maybe_bump_ws(pcx);
                    self.maybe_bump_type(pcx, ctx, "<type>");
                }

                self.builder.finish_node();
            } else {
                let tk = self.peek(pcx).unwrap();

                self.errs.push(ParseError::UnexpectedToken {
                    // TODO: figure out why it's at this point
                    expected: "`)` or parameter".to_string(),
                    found: *tk,
                    ctx,
                });

                // anyway consume the syntax
                self.maybe_bump_sexp(pcx);
            }
        }

        self.builder.finish_node();
    }

    /// Sexp* until ")"
    ///
    /// Advances until it peeks a right paren. Returns if the end of the list was found.
    fn _bump_sexps_to_end_paren(&mut self, pcx: &ParseContext) -> Result<(), ()> {
        loop {
            self.maybe_bump_ws(pcx);

            if let Some(peek) = self.peek(pcx) {
                if peek.kind == SyntaxKind::RParen {
                    return Ok(());
                }
            } else {
                return Err(());
            }

            assert!(self.maybe_bump_sexp(pcx).is_some());
        }
    }

    /// -> Type
    fn _maybe_bump_return_type(&mut self, pcx: &ParseContext, ctx: ErrorContext) -> Option<()> {
        self.maybe_bump_kind(pcx, SyntaxKind::RightArrow)?;

        self.maybe_bump_ws(pcx);

        let checkpoint = self.builder.checkpoint();
        self.maybe_bump_type(pcx, ctx, "<return type>")?;
        self.builder
            .start_node_at(checkpoint, SyntaxKind::ReturnType.into());
        self.builder.finish_node();

        Some(())
    }

    fn maybe_bump_type(
        &mut self,
        pcx: &ParseContext,
        ctx: ErrorContext,
        expected: &'static str,
    ) -> Option<()> {
        let checkpoint = self.builder.checkpoint();

        if self.maybe_bump_path(pcx).is_some() {
            self.builder
                .start_node_at(checkpoint, SyntaxKind::TypePath.into());
            self.builder.finish_node();
            return Some(());
        }

        if let Some(peek) = self.peek(pcx) {
            self.errs.push(ParseError::UnexpectedToken {
                expected: expected.to_string(),
                found: *peek,
                ctx,
            });
        } else {
            self.errs.push(ParseError::UnexpectedEof {
                expected: expected.to_string(),
            });
        };

        None
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

        if self._bump_sexps_to_end_paren(pcx).is_ok() {
            self.maybe_bump_kind(pcx, SyntaxKind::RParen);
        } else {
            todo!("emit error");
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
            let err = if let Some(peek) = self.peek(pcx) {
                ParseError::UnexpectedToken {
                    expected: "<place>".to_string(),
                    found: *peek,
                    ctx: ErrorContext::Set { set_span: tk.span },
                }
            } else {
                ParseError::UnexpectedEof {
                    expected: "<place>".to_string(),
                }
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

        // `)`
        if self.maybe_bump_kind(pcx, SyntaxKind::RParen).is_none() {
            // TODO: better recovery on failure
            self._bump_rest_list_wrapping(pcx, checkpoint, SyntaxKind::Cond);
        }

        // wrap the `cond` node
        self.builder.start_node_at(checkpoint, SyntaxKind::Cond.into());
        self.builder.finish_node();
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
    ) -> Result<(), ()> {
        self.builder.start_node_at(checkpoint, kind.into());

        self.maybe_bump_ws(pcx);
        let res = self._bump_sexps_to_end_paren(pcx);

        self.builder.finish_node();
        res
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

            if self._bump_sexps_to_end_paren(pcx).is_ok() {
                self.maybe_bump_kind(pcx, SyntaxKind::RParen);
            } else {
                todo!("emit error\nsource: {}", pcx.src);
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
                self.errs.push(ParseError::UnexpectedToken {
                    expected: "<symbol>".to_string(),
                    found: *tk,
                    ctx: ErrorContext::Sexp,
                });

                // Discard this token so that we won't enter infinite loop
                self.tsp.end += 1u32;
                self.tsp.start = self.tsp.end;
            }
            None => {
                self.errs.push(ParseError::UnexpectedEof {
                    expected: "<symbol>".to_string(),
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
            pcx.tks[self.tsp.end.into_usize() - 1].span.end
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
                    Some(tk) => tk.span.start,
                    None => pcx.src.len().into(),
                };

                self.errs.push(ParseError::PathNotEndWithIdent {
                    span: Span { start, end },
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
