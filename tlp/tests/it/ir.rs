/*!
Tests for toylisp intermediate representations
*/

use std::sync::Arc;

use tlp::ir::{
    data::decl,
    db::{vfs::*, *},
};

#[test]
fn module_tree() {
    let mut db = DB::default();
    let mut vfs = Vfs::default();

    let path = "my-module.tlp".into();
    let file = vfs.intern(path);

    let src = r#"
(proc f (x y z) (+ x y z))
(proc g (x y z) (+ x y z))
(proc h (x y z) (+ x y z))
(proc atom () 10)
"#;

    db.set_input(file.clone(), Arc::new(String::from(src)));

    // 1. ItemTree
    let item_tree = db.file_item_tree(file.clone());

    let mut procs = item_tree.procs().iter();
    let (_ix, proc) = procs.next().expect("no procedure detected!");
    let params = proc.params();

    assert_eq!(proc.name().as_str(), "f");
    assert_eq!(params.len(), 3);
    assert_eq!(params[0].name().as_str(), "x");
    assert_eq!(params[1].name().as_str(), "y");
    assert_eq!(params[2].name().as_str(), "z");

    let (_ix, proc) = procs.next().unwrap();
    assert_eq!(proc.name().as_str(), "g");
    let (_ix, proc) = procs.next().unwrap();
    assert_eq!(proc.name().as_str(), "h");
    let (_ix, proc) = procs.next().unwrap();
    assert_eq!(proc.name().as_str(), "atom");

    // 2. DefMap
    let krate = file.clone();
    let def_map = db.crate_def_map(krate.clone());

    let root = def_map.root();
    let module = def_map.module(root);
    let scope = module.scope();

    {
        let names = ["f", "g", "h", "atom"].map(|s| decl::Name::from_str(s));
        let mut i = 0;

        for name in &names {
            i += 1;

            let proc_id = scope.lookup_proc(name).unwrap();
            let proc_loc = proc_id.lookup(&db);
            let proc = &item_tree[proc_loc.item];

            assert!(matches!(proc.name().as_str(), "f" | "g" | "h" | "atom"));
        }
    }

    // 3-1. HIR definition data
    let name = decl::Name::from_str("atom");
    let proc_id = scope.lookup_proc(&name).unwrap();

    let proc_data = db.proc_data(proc_id);
    assert_eq!(proc_data.name, name);

    // 3-2. HIR body
    // let body = db.proc_body(proc_id);
}

#[test]
fn main_literal() {
    let mut db = DB::default();
    let mut vfs = Vfs::default();

    let path = "my-module.tlp".into();
    let file = vfs.intern(path);

    // => 7
    let src = r#"(proc main () 12)"#;
    db.set_input(file.clone(), Arc::new(String::from(src)));

    let krate = file.clone();
    let def_map = db.crate_def_map(krate.clone());

    let root = def_map.root();
    let module = def_map.module(root);
    let scope = module.scope();

    // 3. HIR definition data
    let name = decl::Name::from_str("main");

    let proc_id = scope.lookup_proc(&name).unwrap();
    let proc_data = db.proc_data(proc_id);

    assert_eq!(proc_data.name, name);
}
