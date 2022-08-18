//! Tests of toylisp HIR

use std::sync::Arc;

use tlp::hir_def::{
    db::{
        self,
        ids::{HirItemLoc, Id},
        vfs, *,
    },
    expr::{self, Expr},
    item::{self, Name},
};

use crate::util;

#[test]
fn main_literal() {
    let mut db = db::DB::default();
    let mut vfs = vfs::Vfs::default();

    let path = "my-module.tlp".into();
    let vfs_file_id = vfs.intern(path);

    let src = r#"
(proc main (a b)
    12
    (+ 1 2))"#;
    db.set_input(vfs_file_id.clone(), Arc::new(String::from(src)));

    let krate = vfs_file_id.clone();
    let crate_data = db.crate_data(krate.clone());

    let file_data_id = crate_data.root_file_data_id();
    let file = crate_data.sub_file(file_data_id);
    let file_item_scope = &file.item_scope;

    // 3. name
    let name = item::Name::from_str("main");

    let proc_id = file_item_scope.lookup_proc(&name).unwrap();
    let proc_loc = proc_id.lookup_loc(&db);
    let tree = db.file_item_list(proc_loc.file);
    let proc_data = &tree[proc_loc.idx];

    assert_eq!(proc_data.name(), Some(&name));

    // 4. Parameters
    let params = proc_data
        .params()
        .iter()
        .map(|p| p.name())
        .collect::<Vec<_>>();
    assert_eq!(params, [&Name::from_str("a"), &Name::from_str("b")]);

    // 5. Body
    let body = db.proc_body(proc_id);
    let mut exprs = body.root_block().children.iter().cloned();

    // 12
    assert_eq!(
        &body.exprs[exprs.next().unwrap()],
        &Expr::Literal(expr::Literal::Int(12))
    );

    // (+ 1 2)
    let node = &body.exprs[exprs.next().unwrap()];
    match node {
        Expr::Call(call) => {
            let path_expr = match &body.exprs[call.path] {
                expr::Expr::Path(p) => p,
                _ => unreachable!(),
            };
            let path_data = path_expr.lookup(&db);

            assert_eq!(path_data.segments[0].as_str(), "+");
            assert_eq!(
                &body.exprs[call.args[0]],
                &Expr::Literal(expr::Literal::Int(1)),
            );
            assert_eq!(
                &body.exprs[call.args[1]],
                &Expr::Literal(expr::Literal::Int(2)),
            );
        }
        _ => panic!("not a call node: {:?}", node),
    };
}

#[test]
fn expr_scopes() {
    do_check(
        r"(proc main ()
              (test)
              (let x 0)
              (let y 1)
              (let z 2)
              )",
        &[],
    );

    do_check(
        r"(proc main ()
              (let x 0)
              (test)
              (let y 1)
              (let z 2)
              )",
        &["x"],
    );

    do_check(
        r"(proc main ()
              (let x 0)
              (let y 1)
              (test)
              (let z 2)
              )",
        &["y", "x"],
    );

    do_check(
        r"(proc main ()
              (let x 0)
              (let y 1)
              (let z 2)
              (test))",
        &["z", "y", "x"],
    );
}

/// Tests expression scope at `test` function call
pub fn do_check(src: &str, expected_scope: &[&str]) {
    let mut db = DB::default();
    let mut vfs = vfs::Vfs::default();

    let path = "test-module.tlp".into();
    let vfs_file_id = vfs.intern(path);
    db.set_input(vfs_file_id.clone(), Arc::new(String::from(src)));

    // let parse = db.parse(vfs_file_id);

    let main_proc_id = self::find_procedure_in_crate(&db, vfs_file_id, &Name::from_str("main"));

    let body = db.proc_body(main_proc_id);
    let scope_map = db.proc_expr_scope_map(main_proc_id);

    // TODO: use source map and retrieve expression id at marker

    // find `test` function call
    let test_expr_id = body
        .root_block()
        .children
        .iter()
        .find(|expr| {
            let expr = &body.exprs[**expr];
            if let Expr::Call(call) = expr {
                // TODO: easier cast
                let path = match &body.exprs[call.path] {
                    Expr::Path(p) => p,
                    _ => unreachable!(),
                };

                let path_data = path.lookup(&db);
                path_data.segments[0] == Name::from_str("test")
            } else {
                false
            }
        })
        .unwrap();

    let test_expr_scope = scope_map.scope_for_expr(*test_expr_id).unwrap();

    let actual = scope_map
        .scope_chain(test_expr_scope)
        .flat_map(|scope| scope_map.entries(scope))
        .map(|entry| entry.name.as_str())
        .collect::<Vec<_>>()
        .join("\n");

    let expected = expected_scope.join("\n");
    util::assert_eq_text!(&expected, &actual);
}

fn find_procedure_in_crate(
    db: &dyn db::Def,
    krate: vfs::VfsFileId,
    name: &Name,
) -> Id<HirItemLoc<item::DefProc>> {
    let crate_data = db.crate_data(krate);
    let crate_file_data = crate_data.root_file_data();
    let item_scope = &crate_file_data.item_scope;

    item_scope.lookup_proc(name).unwrap()
}
