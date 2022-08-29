use tlp::Db;

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
