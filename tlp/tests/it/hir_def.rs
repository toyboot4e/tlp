/*!
Tests for toylisp intermediate representations
*/

use std::sync::Arc;

use tlp::hir_def::{
    body::expr::*,
    db::{vfs::*, *},
    item::{self, Name},
};

#[test]
fn main_literal() {
    let mut db = DB::default();
    let mut vfs = Vfs::default();

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
    let scope = file.scope();

    // 3. name
    let name = item::Name::from_str("main");

    let proc_id = scope.lookup_proc(&name).unwrap();
    let proc_loc = proc_id.lookup(&db);
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
    let mut exprs = body.root.children.iter().cloned();

    // 12
    assert_eq!(
        &body.exprs[exprs.next().unwrap()],
        &Expr::Literal(Literal::Int(12))
    );

    // (+ 1 2)
    let node = &body.exprs[exprs.next().unwrap()];
    match node {
        Expr::Call(call) => {
            assert_eq!(call.name.as_str(), "+");
            assert_eq!(&body.exprs[call.args[0]], &Expr::Literal(Literal::Int(1)),);
            assert_eq!(&body.exprs[call.args[1]], &Expr::Literal(Literal::Int(2)),);
        }
        _ => panic!("not a call node: {:?}", node),
    };
}
