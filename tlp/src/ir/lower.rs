//! Lowers code block into [`Body`](body::Body)

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
            pat::{Pat, PatData},
            SyntheticSyntax,
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
                children: children.into_boxed_slice(),
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
                    let literal = expr::Literal::parse(x).unwrap();
                    self.alloc(ExprData::Literal(literal), span)
                }
                ast::LiteralKind::Str(_str) => {
                    todo!()
                }
                ast::LiteralKind::True(_) | ast::LiteralKind::False(_) => {
                    todo!()
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
