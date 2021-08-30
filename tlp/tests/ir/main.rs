/*!
Tests for toylisp intermediate representations
*/

use camino::Utf8PathBuf;
use std::sync::Arc;

use tlp::ir::{
    data::loc::*,
    db::{queries::*, DB},
};

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

    assert_eq!(tree.procs()[0].name().as_str(), "f");
    assert_eq!(tree.procs()[1].name().as_str(), "g");
    assert_eq!(tree.procs()[2].name().as_str(), "h");
}
