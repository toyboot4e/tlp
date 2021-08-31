/*!
Tests for toylisp intermediate representations
*/

use std::sync::Arc;

use tlp::ir::db::{input::*, *};

#[test]
fn module_tree() {
    let mut db = DB::default();

    let file = FileId::new("my-module.tlp".into());
    let src = r#"
(proc f (x y z) (+ x y z))
(proc g (x y z) (+ x y z))
(proc h (x y z) (+ x y z))
"#;

    db.set_input(file.clone(), Arc::new(String::from(src)));
    let tree = db.item_tree(file);

    let proc = &tree.procs()[0];
    let params = proc.params();

    assert_eq!(proc.name().as_str(), "f");
    assert_eq!(params.len(), 3);
    assert_eq!(params[0].name().as_str(), "x");
    assert_eq!(params[1].name().as_str(), "y");
    assert_eq!(params[2].name().as_str(), "z");

    assert_eq!(tree.procs()[1].name().as_str(), "g");
    assert_eq!(tree.procs()[2].name().as_str(), "h");
}
