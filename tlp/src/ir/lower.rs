//! Lowers code block into [`Body`](body::Body)

use la_arena::Idx;

use base::{
    jar::{InputFile, Word},
    span::Span,
    tbl::{origin_table::PushOriginIn, InternValue},
};

use crate::{
    ir::{
        body::{
            self,
            expr::{self, Expr, ExprData},
            expr_scope::{ExprScopeMap, ExprScopeMapData, ScopeData},
            pat::{Pat, PatData},
            BodyData, SyntheticSyntax,
        },
        item::{self, Item},
        item_scope::{ItemScope, ItemScopeData},
        jar::ParsedFile,
        InputFileExt, IrDb, IrJar,
    },
    syntax::ast::{self, AstNode},
};

#[salsa::tracked(return_ref, jar = IrJar)]
pub(crate) fn lower_items(db: &dyn IrDb, file: base::jar::InputFile) -> ParsedFile {
    let mut items = Vec::new();

    let src = file.source_text(db.base());
    let parse = ast::parse(src);
    let ast = parse.doc;

    for item in ast.items() {
        let item = match item {
            ast::Item::DefProc(ast) => {
                let proc = match item::Proc::from_ast(db, file, ast) {
                    Some(x) => x,
                    None => continue,
                };

                Item::Proc(proc)
            }
        };

        items.push(item);
    }

    ParsedFile::new(db, file, items)
}

// TODO: why not `return_ref`?
#[salsa::tracked(jar = IrJar)]
pub(crate) fn lower_body(db: &dyn IrDb, proc: item::Proc) -> body::Body {
    let mut lower = LowerBody {
        db,
        tables: Default::default(),
        spans: Default::default(),
        root_block: Default::default(),
    };

    lower.lower_root_proc(proc);
    lower.into_body()
}

struct LowerBody<'a> {
    db: &'a dyn IrDb,
    tables: body::BodyTables,
    spans: body::BodySpans,
    root_block: Option<Expr>,
}

/// # Components
impl<'a> LowerBody<'a> {
    pub fn into_body(self) -> body::Body {
        let data = body::BodyData {
            tables: self.tables,
            root_block: self.root_block.unwrap(),
        };

        body::Body::new(self.db, data, self.spans)
    }

    pub fn lower_root_proc(&mut self, proc: item::Proc) {
        let ast = proc.ast(self.db);
        let ast_block = ast.block();

        let block = self.lower_block(ast_block);
        self.root_block = Some(block);
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

/// # [`Expr`]
impl<'a> LowerBody<'a> {
    fn lower_ast_expr(&mut self, ast_expr: ast::Expr) -> Expr {
        let span = Ok(Span::from_rowan_range(ast_expr.syntax().text_range()));

        match ast_expr {
            // expressions
            ast::Expr::Call(call) => {
                let path = self.lower_ast_expr(call.path().into());
                let args = call.args().map(|expr| self.lower_ast_expr(expr)).collect();

                let expr = expr::Call { path, args };
                self.alloc(ExprData::Call(expr), span)
            }
            ast::Expr::Let(let_) => {
                let pat = self.lower_opt_ast_pat(let_.pat());
                let rhs = self.lower_opt_ast_expr(let_.rhs());

                let let_ = expr::Let { pat, rhs };
                self.alloc(ExprData::Let(let_), span)
            }
            ast::Expr::Path(ast_path) => {
                let path = expr::Path::parse(self.db, ast_path);
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

    fn lower_opt_ast_expr(&mut self, expr: Option<ast::Expr>) -> Expr {
        match expr {
            Some(expr) => self.lower_ast_expr(expr),
            None => self.alloc_missing_expr(),
        }
    }
}

/// # [`Pat`]
impl<'a> LowerBody<'a> {
    fn lower_ast_pat(&mut self, ast_pat: ast::Pat) -> Pat {
        let span = Span::from_rowan_range(ast_pat.syntax().text_range());

        match ast_pat {
            ast::Pat::PatPath(_) => todo!(),
            ast::Pat::PatIdent(ident) => {
                let name = Word::intern(self.db.base(), ident.ident_token().text());
                let pat = PatData::Bind { name };
                self.alloc(pat, Ok(span))
            }
        }
    }

    fn lower_opt_ast_pat(&mut self, pat: Option<ast::Pat>) -> Pat {
        match pat {
            Some(pat) => self.lower_ast_pat(pat),
            None => self.alloc_missing_pat(),
        }
    }
}

/// # Allocators
impl<'a> LowerBody<'a> {
    fn alloc<D, K>(&mut self, data: D, span: Result<Span, SyntheticSyntax>) -> K
    where
        D: std::hash::Hash + Eq + std::fmt::Debug,
        D: InternValue<Table = body::BodyTables, Key = K>,
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

#[salsa::tracked(jar = IrJar)]
pub(crate) fn lower_item_scope(db: &dyn IrDb, file: InputFile) -> ItemScope {
    let items = file.items(db);
    let mut scope = ItemScopeData::default();

    for item in items {
        let proc = match item {
            Item::Proc(x) => x,
            // TODO: imports
            // TODO: imports
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
        // Walk child expressions and track scope for them.
        // It should not modify curernt scope.
        // --------------------------------------------------------------------------------
        ExprData::Call(call) => {
            self::compute_expr_scopes(call.path, body_data, scopes, scope_idx);
            call.args.iter().for_each(|expr| {
                self::compute_expr_scopes(*expr, body_data, scopes, scope_idx);
            });
        }

        // --------------------------------------------------------------------------------
        // Terminals. No children, nothing to do
        // --------------------------------------------------------------------------------
        ExprData::Missing => {}
        ExprData::Path(_) => {}
        ExprData::Literal(_) => {}
        _ => todo!(),
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
