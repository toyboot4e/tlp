use tlp::{
    base::BaseDb,
    ir::{item::Item, InputFileExt},
    Db,
};

#[test]
fn items() {
    let db = &mut Db::default();

    let src = r"(proc f ())
                (proc g ())
                (proc h ())";

    let f = db.new_input_file("main.tlp", src.to_string());
    let items = f.items(db);

    assert_eq!(
        &items
            .iter()
            .map(|i| i.name(db).as_str(db))
            .collect::<Vec<_>>(),
        &["f", "g", "h"],
    );
}

#[test]
fn body() {
    let db = &mut Db::default();

    let src = r"(proc main ()
                    (let x 3)
                    (* x 4))";

    let f = db.new_input_file("main.tlp", src.to_string());
    let items = f.items(db);

    let proc = match items[0] {
        Item::Proc(proc) => proc,
    };

    assert_eq!(proc.name(db).as_str(db.as_base_db()), "main");

    let body = proc.body(db);
}
