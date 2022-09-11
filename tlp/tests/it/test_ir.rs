use tlp::{
    base::{jar::Word, BaseDb},
    ir::{
        body::{
            expr::{self, ExprData},
            pat::PatData,
        },
        item::Item,
        InputFileExt,
    },
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
        _ => unreachable!(),
    };

    assert_eq!(proc.name(db).as_str(db.base()), "main");

    let body = proc.body_data(db);
    let exprs = body
        .root_block()
        .iter()
        .map(|x| &body.tables[*x])
        .collect::<Vec<_>>();

    match &exprs[0] {
        ExprData::Let(let_) => {
            let pat = &body.tables[let_.pat];

            assert_eq!(
                pat,
                &PatData::Bind {
                    name: Word::intern(db.base(), "x")
                }
            );

            let rhs = &body.tables[let_.rhs];
            match rhs {
                ExprData::Literal(lit) => match lit {
                    expr::Literal::I32(x) => assert_eq!(*x, 3),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            };
        }
        _ => unreachable!(),
    }
}
