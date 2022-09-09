use tlp::{
    ir::{
        body::expr::{Expr, ExprData},
        item::{self, Item},
        ty::{self, PrimitiveType, TypeData},
        InputFileExt, IrDb,
    },
    Db,
};

fn new_main(src: &str) -> (Db, item::Proc) {
    let mut db = Db::default();
    let file = db.new_input_file("main.tlp", src.to_string());

    let proc = match file.items(&mut db)[0] {
        Item::Proc(proc) => proc,
        _ => unreachable!(),
    };

    (db, proc)
}

fn exprs(db: &dyn IrDb, proc: item::Proc) -> Vec<Expr> {
    let body = proc.body_data(db);
    body.root_block().iter().map(|e| *e).collect::<Vec<_>>()
}

fn expr_data(db: &dyn IrDb, proc: item::Proc) -> Vec<ExprData> {
    let body = proc.body_data(db);
    body.root_block()
        .iter()
        .map(|x| body.tables[*x].clone())
        .collect::<Vec<_>>()
}

fn expr_vec(db: &dyn IrDb, proc: item::Proc) -> Vec<(Expr, ExprData)> {
    let body_data = proc.body_data(db);
    body_data
        .root_block()
        .iter()
        .map(|&e| (e, body_data.tables[e].clone()))
        .collect::<Vec<_>>()
}

#[test]
fn type_of_plus_i32_f32() {
    let src = r"(proc main () (+ 10 15.0))";
    let (db, proc) = self::new_main(src);

    let expr_data = self::expr_data(&db, proc);
    let types = proc.type_table(&db);

    match &expr_data[0] {
        ExprData::Call(call) => {
            assert_eq!(
                &types[call.args[0]],
                &TypeData::Primitive(PrimitiveType::I32)
            );

            assert_eq!(
                &types[call.args[1]],
                &TypeData::Primitive(PrimitiveType::F32)
            );

            assert_eq!(
                &types[call.path],
                &TypeData::Op(ty::OpType {
                    kind: ty::OpKind::Add,
                    target_ty: ty::OpTargetType::I32,
                })
            );
        }

        _ => unreachable!(),
    }
}

#[test]
fn type_of_mul_f32_i32() {
    let src = r"(proc main () (* 15.0 10))";
    let (db, proc) = self::new_main(src);

    let expr_data = self::expr_data(&db, proc);
    let types = proc.type_table(&db);

    match &expr_data[0] {
        ExprData::Call(call) => {
            assert_eq!(
                &types[call.args[0]],
                &TypeData::Primitive(PrimitiveType::F32)
            );

            assert_eq!(
                &types[call.args[1]],
                &TypeData::Primitive(PrimitiveType::I32)
            );

            assert_eq!(
                &types[call.path],
                &TypeData::Op(ty::OpType {
                    kind: ty::OpKind::Mul,
                    target_ty: ty::OpTargetType::F32,
                })
            );
        }

        _ => unreachable!(),
    }
}

#[test]
fn let_type() {
    let src = r"(proc main () (let x (* 1.0 2.5)) (+ x 13.2))";
    let (db, proc) = self::new_main(src);

    let exprs = self::expr_vec(&db, proc);
    let types = proc.type_table(&db);

    match &exprs[0].1 {
        ExprData::Let(let_) => {
            match &types[let_.rhs] {
                TypeData::Primitive(ty::PrimitiveType::F32) => {}
                x => unreachable!("{x:?}"),
            }

            match &types[let_.pat] {
                TypeData::Primitive(prim) => {
                    assert_eq!(prim, &ty::PrimitiveType::F32,);
                }
                x => unreachable!("{x:?}"),
            }

            //
        }

        _ => unreachable!(),
    }
}
