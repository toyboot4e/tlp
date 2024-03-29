//! Lowers code block into [`Body`](body::Body)

use la_arena::Idx;

use base::{
    jar::InputFile,
    span::Span,
    tbl::{origin_table::PushOriginIn, InternValue},
};

use crate::{
    ir::{
        body::{
            self,
            expr::{self, Expr, ExprData, TypeSyntax},
            expr_scope::{ExprScopeMap, ExprScopeMapData, ScopeData},
            pat::{Pat, PatData},
            BodyData, SyntheticSyntax,
        },
        ir_diag::{self, ItemDiagnostics},
        item::{self, Item},
        item_scope::{ItemScope, ItemScopeData},
        jar::ParsedFile,
        InputFileExt, IrDb, IrJar,
    },
    syntax::{
        ast::{self, AstNode},
        cst,
    },
};

#[salsa::tracked(return_ref, jar = IrJar)]
pub(crate) fn lower_items(db: &dyn IrDb, file: base::jar::InputFile) -> ParsedFile {
    let mut items = Vec::new();

    let src = file.source_text(db.base());
    let (tks, lex_errs) = cst::lex::from_str(src);

    if !lex_errs.is_empty() {
        return ParsedFile::new(db, file, lex_errs, Vec::new(), Vec::new());
    }

    let parse = ast::from_tks(src, &tks);
    let ast = parse.doc;

    for item in ast.items() {
        let item = match item {
            ast::Item::DefProc(ast) => {
                let proc = match item::Proc::from_ast(db, file, ast.clone()) {
                    Some(proc) => {
                        let mut diag = ir_diag::ProcDiagnostic::new(proc);

                        for (i, param) in proc.params(db).iter().enumerate() {
                            if let TypeSyntax::Missing = param.ty {
                                diag.params.push(ir_diag::ProcParamDiagnostic {
                                    index: i,
                                    type_missing: true,
                                });
                            }
                        }

                        if matches!(proc.return_ty(db), &Some(TypeSyntax::Missing)) {
                            diag.missing_ret_ty = true;
                        }

                        if diag.any_error() {
                            ItemDiagnostics::push(db, diag.into());
                        }

                        proc
                    }
                    None => {
                        let span = Span::from_rowan_range(ast.proc_tk().text_range());
                        let diag = ir_diag::MissingProcName { span };
                        ir_diag::ItemDiagnostics::push(db, diag.into());

                        continue;
                    }
                };

                Item::Proc(proc)
            }
        };

        items.push(item);
    }

    ParsedFile::new(db, file, lex_errs, parse.errs, items)
}

#[salsa::tracked(jar = IrJar)]
pub(crate) fn lower_body(db: &dyn IrDb, proc: item::Proc) -> body::Body {
    let mut lower = LowerBody {
        db,
        spans: Default::default(),
        tables: Default::default(),
        param_pats: Vec::new(),
        root_block: Default::default(),
    };

    lower.lower_proc(proc);
    lower.into_body()
}

struct LowerBody<'a> {
    db: &'a dyn IrDb,
    spans: body::BodySpans,
    tables: body::BodyTables,
    param_pats: Vec<Pat>,
    root_block: Option<Expr>,
}

/// # Components
impl<'a> LowerBody<'a> {
    pub fn into_body(self) -> body::Body {
        let data = body::BodyData {
            tables: self.tables,
            param_pats: self.param_pats,
            root_block: self.root_block.unwrap(),
        };

        body::Body::new(self.db, data, self.spans)
    }

    pub fn lower_proc(&mut self, proc: item::Proc) {
        // params
        self.lower_params(proc);

        // body
        let ast = proc.ast(self.db);
        let ast_block = ast.block();
        let block = self.lower_block(ast_block);
        self.root_block = Some(block);
    }

    fn lower_params(&mut self, proc: item::Proc) {
        // REMARK: We're not using IR parameter type
        let ast_proc = proc.ast(self.db);

        let params = match ast_proc.params() {
            Some(x) => x,
            _ => return,
        };

        for param in params.nodes() {
            let pat = self.lower_opt_ast_pat(param.pat());
            self.param_pats.push(pat);
        }
    }

    fn lower_block(&mut self, ast_block: ast::Block) -> Expr {
        let block = {
            let mut children = Vec::new();

            for ast_expr in ast_block.exprs() {
                let expr = self.lower_ast_expr(ast_expr);
                children.push(expr);
            }

            expr::Block {
                exprs: children.into_boxed_slice(),
            }
        };

        let block_data = ExprData::Block(block);
        let span = Span::from_rowan_range(ast_block.syn.text_range());
        self.alloc(block_data, Ok(span))
    }
}

/// Basic lowering components
impl<'a> LowerBody<'a> {
    fn lower_opt_ast_expr(&mut self, expr: Option<ast::Expr>) -> Expr {
        match expr {
            Some(expr) => self.lower_ast_expr(expr),
            None => self.alloc_missing_expr(),
        }
    }

    fn lower_opt_ast_pat(&mut self, pat: Option<ast::Pat>) -> Pat {
        match pat {
            Some(pat) => self.lower_ast_pat(pat),
            None => self.alloc_missing_pat(),
        }
    }

    fn lower_ast_pat(&mut self, ast_pat: ast::Pat) -> Pat {
        let span = Span::from_rowan_range(ast_pat.syntax().text_range());
        if let Some(pat) = PatData::from_ast(self.db, ast_pat) {
            self.alloc(pat, Ok(span))
        } else {
            todo!()
        }
    }
}

/// # Allocators
impl<'a> LowerBody<'a> {
    fn alloc<D, K>(&mut self, data: D, span: Result<Span, SyntheticSyntax>) -> K
    where
        D: std::hash::Hash + Eq + std::fmt::Debug,
        D: InternValue<Table = body::BodyTables, Key = K>,
        K: PushOriginIn<body::BodySpans, Origin = Result<Span, SyntheticSyntax>> + salsa::AsId,
    {
        let key = self.tables.add(data);
        self.spans.push(key, span);
        key
    }

    fn alloc_missing_expr(&mut self) -> Expr {
        let expr = self.tables.add(ExprData::Missing);
        self.spans.push(expr, Err(SyntheticSyntax));
        expr
    }

    fn alloc_missing_pat(&mut self) -> Pat {
        let pat = self.tables.add(PatData::Missing);
        self.spans.push(pat, Err(SyntheticSyntax));
        pat
    }
}

/// # [`Expr`]
impl<'a> LowerBody<'a> {
    fn lower_ast_expr(&mut self, ast_expr: ast::Expr) -> Expr {
        let span = Ok(Span::from_rowan_range(ast_expr.syntax().text_range()));

        match ast_expr {
            // expressions
            ast::Expr::Call(call) => {
                let path = self.lower_ast_expr(call.path().into());
                let args = call.args().map(|expr| self.lower_ast_expr(expr)).collect();

                loop {
                    // override bulint function call
                    let path_data = self.tables[path].clone().into_path();

                    if path_data.segments.len() == 1 {
                        if let Some(kind) =
                            expr::OpKind::parse(path_data.segments[0].as_str(self.db.base()))
                        {
                            let args = call.args().map(|expr| self.lower_ast_expr(expr)).collect();

                            let op_expr = {
                                let path_span =
                                    Span::from_rowan_range(call.path().syn.text_range());
                                self.alloc(ExprData::Op(kind), Ok(path_span))
                            };

                            let expr = expr::CallOp { op_expr, args };
                            break self.alloc(ExprData::CallOp(expr), span);
                        }
                    }

                    // ordinary funtion call
                    let expr = expr::Call { path, args };
                    break self.alloc(ExprData::Call(expr), span);
                }
            }
            ast::Expr::And(and) => {
                let exprs = and.exprs().map(|expr| self.lower_ast_expr(expr)).collect();
                let expr = expr::And { exprs };
                self.alloc(ExprData::And(expr), span)
            }
            ast::Expr::Or(or) => {
                let exprs = or.exprs().map(|expr| self.lower_ast_expr(expr)).collect();
                let expr = expr::Or { exprs };
                self.alloc(ExprData::Or(expr), span)
            }
            ast::Expr::When(when) => {
                let pred = self.lower_opt_ast_expr(when.pred());
                let block = self.lower_block(when.block());

                let expr = expr::When { pred, block };
                self.alloc(ExprData::When(expr), span)
            }
            ast::Expr::Unless(unless) => {
                let pred = self.lower_opt_ast_expr(unless.pred());
                let block = self.lower_block(unless.block());

                let expr = expr::Unless { pred, block };
                self.alloc(ExprData::Unless(expr), span)
            }
            ast::Expr::Cond(cond) => {
                let mut cases = Vec::new();
                for case in cond.cases() {
                    let pred = self.lower_opt_ast_expr(case.pred());
                    let block = self.lower_block(case.block());

                    let case = expr::CondCase { pred, block };
                    cases.push(case);
                }

                // If there's any case that has `true` test case, it's an expression.
                // Otherwise it's a statement
                let is_expr = cond.cases().any(|case| {
                    if let Some(ast::Expr::Literal(lit)) = case.pred() {
                        matches!(lit.kind(), ast::LiteralKind::True(_))
                    } else {
                        false
                    }
                });

                let expr = expr::Cond {
                    can_be_expr: is_expr,
                    cases,
                };
                self.alloc(ExprData::Cond(expr), span)
            }
            ast::Expr::Loop(loop_) => {
                let block = self.lower_opt_ast_expr(loop_.block().map(|b| b.into()));

                let loop_ = expr::Loop { block };
                self.alloc(ExprData::Loop(loop_), span)
            }
            ast::Expr::While(while_) => {
                assert!(while_.pred().is_some());
                let pred = self.lower_opt_ast_expr(while_.pred().map(|b| b.into()));
                let block = self.lower_opt_ast_expr(while_.block().map(|b| b.into()));

                let while_ = expr::While { pred, block };
                self.alloc(ExprData::While(while_), span)
            }
            ast::Expr::Set(set) => {
                let place = self.lower_opt_ast_expr(set.place().map(|ast_path| ast_path.into()));
                let rhs = self.lower_opt_ast_expr(set.rhs());

                let set = expr::Set { place, rhs };
                self.alloc(ExprData::Set(set), span)
            }
            ast::Expr::Let(let_) => {
                let pat = self.lower_opt_ast_pat(let_.pat());
                let rhs = self.lower_opt_ast_expr(let_.rhs());

                let let_ = expr::Let { pat, rhs };
                self.alloc(ExprData::Let(let_), span)
            }
            ast::Expr::Path(ast_path) => {
                let path = expr::Path::from_ast(self.db, ast_path);
                self.alloc(ExprData::Path(path), span)
            }
            ast::Expr::Literal(lit) => match lit.kind() {
                ast::LiteralKind::Num(x) => {
                    let literal = expr::Literal::parse_num(x).unwrap();
                    self.alloc(ExprData::Literal(literal), span)
                }
                ast::LiteralKind::Str(_str) => {
                    todo!()
                }
                ast::LiteralKind::True(_) | ast::LiteralKind::False(_) => {
                    let b = matches!(lit.kind(), ast::LiteralKind::True(_));
                    self.alloc(ExprData::Literal(expr::Literal::Bool(b)), span)
                }
            },
            ast::Expr::Block(block) => self.lower_block(block),
        }
    }
}

#[salsa::tracked(jar = IrJar)]
pub(crate) fn lower_item_scope(db: &dyn IrDb, file: InputFile) -> ItemScope {
    let items = file.items(db);
    let mut scope = ItemScopeData::default();

    for item in items {
        let proc = match item {
            Item::Proc(x) => x,
        };

        let name = proc.name(db);
        scope.declare_proc(name.word(db.base()), *proc);
    }

    ItemScope::new(db, scope)
}

#[salsa::tracked(jar = IrJar)]
pub(crate) fn lower_proc_expr_scope(db: &dyn IrDb, proc: item::Proc) -> ExprScopeMap {
    let body = proc.body(db);
    let body_data = body.data(db);
    let data = self::body_expr_scope(db, &body_data);
    ExprScopeMap::new(db, data)
}

fn body_expr_scope(_db: &dyn IrDb, body_data: &BodyData) -> ExprScopeMapData {
    let mut scopes = ExprScopeMapData::default();

    // start with the root block expression
    let root_scope = scopes.alloc_root_scope();

    // with paramter patterns
    for param_pat in &body_data.param_pats {
        scopes.add_bindings(body_data, root_scope, *param_pat);
    }

    self::compute_expr_scopes(body_data.root_block, body_data, &mut scopes, root_scope);

    scopes
}

/// Walks through body expressions, creates scopes on new binding patterns and tracks the scope for
/// each expression
///
/// Returns the last scope index for tracking the current scope.
fn compute_expr_scopes(
    expr: expr::Expr,
    body_data: &BodyData,
    scopes: &mut ExprScopeMapData,
    scope_idx: Idx<ScopeData>,
) -> Idx<ScopeData> {
    // Current scope is only modified by `Let` expression.
    // (Block scope creates a new scope, but it doesn't modify "current scope").
    let mut scope_idx = scope_idx;

    // track the expression scope for the parent expression
    scopes.track_expr_scope(expr, scope_idx);

    // call into the child expressions
    match &body_data.tables[expr] {
        // --------------------------------------------------------------------------------
        // Handle block and binding patterns
        // --------------------------------------------------------------------------------
        ExprData::Block(block) => {
            let block_scope_idx = scopes.new_block_scope(scope_idx);

            // FIXME:
            // Overwrite the block scope with the deepest child.
            // This is important for traverse as `ScopeData` only contains `parernt` index.
            scopes.track_expr_scope(expr, block_scope_idx);

            self::compute_block_scopes(&block.exprs, body_data, scopes, block_scope_idx);
        }
        ExprData::Let(let_) => {
            // expr: track scope
            scope_idx = self::compute_expr_scopes(let_.rhs, body_data, scopes, scope_idx);

            // pat: create new scope
            scope_idx = scopes.append_scope(scope_idx);
            scopes.add_bindings(body_data, scope_idx, let_.pat);
        }

        // --------------------------------------------------------------------------------
        // Walk child expressions and track their scopes
        // --------------------------------------------------------------------------------
        ExprData::Call(call) => {
            self::compute_expr_scopes(call.path, body_data, scopes, scope_idx);
            call.args.iter().for_each(|expr| {
                self::compute_expr_scopes(*expr, body_data, scopes, scope_idx);
            });
        }
        ExprData::CallOp(call_op) => {
            call_op.args.iter().for_each(|expr| {
                self::compute_expr_scopes(*expr, body_data, scopes, scope_idx);
            });
        }

        // builtin functions
        ExprData::Op(_) => {
            // no children
        }
        ExprData::And(and) => {
            and.exprs.iter().for_each(|expr| {
                self::compute_expr_scopes(*expr, body_data, scopes, scope_idx);
            });
        }
        ExprData::Or(or) => {
            or.exprs.iter().for_each(|expr| {
                self::compute_expr_scopes(*expr, body_data, scopes, scope_idx);
            });
        }

        ExprData::When(when) => {
            self::compute_expr_scopes(when.pred, body_data, scopes, scope_idx);
            self::compute_expr_scopes(when.block, body_data, scopes, scope_idx);
        }
        ExprData::Unless(unless) => {
            self::compute_expr_scopes(unless.pred, body_data, scopes, scope_idx);
            self::compute_expr_scopes(unless.block, body_data, scopes, scope_idx);
        }
        ExprData::Cond(cond) => {
            for case in &cond.cases {
                self::compute_expr_scopes(case.pred, body_data, scopes, scope_idx);
                self::compute_expr_scopes(case.block, body_data, scopes, scope_idx);
            }
        }
        ExprData::Loop(loop_) => {
            self::compute_expr_scopes(loop_.block, body_data, scopes, scope_idx);
        }
        ExprData::While(while_) => {
            self::compute_expr_scopes(while_.pred, body_data, scopes, scope_idx);
            self::compute_expr_scopes(while_.block, body_data, scopes, scope_idx);
        }
        ExprData::Set(set) => {
            self::compute_expr_scopes(set.place, body_data, scopes, scope_idx);
            self::compute_expr_scopes(set.rhs, body_data, scopes, scope_idx);
        }

        // --------------------------------------------------------------------------------
        // Terminals. No children, nothing to do
        // --------------------------------------------------------------------------------
        ExprData::Missing => {}
        ExprData::Path(_) => {}
        ExprData::Literal(_) => {}
    }

    scope_idx
}

/// Walks through body expressions, creates scopes and tracks the scope for each expression
fn compute_block_scopes(
    exprs: &[expr::Expr],
    body_data: &BodyData,
    scopes: &mut ExprScopeMapData,
    scope_idx: Idx<ScopeData>,
) -> Idx<ScopeData> {
    let mut scope_idx = scope_idx;

    for expr in exprs {
        scope_idx = self::compute_expr_scopes(*expr, body_data, scopes, scope_idx);
    }

    scope_idx
}
