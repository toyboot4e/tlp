//! Type analysis tests

use tlp::{
    ir::{
        body::expr::{self, Expr, ExprData},
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
    };

    (db, proc)
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
        ExprData::CallOp(call_op) => {
            assert_eq!(
                types[call_op.args[0]].data(&db),
                &TypeData::Primitive(PrimitiveType::I32)
            );

            assert_eq!(
                types[call_op.args[1]].data(&db),
                &TypeData::Primitive(PrimitiveType::F32)
            );

            assert_eq!(
                types[call_op.op_expr].data(&db),
                &TypeData::Op(ty::OpType {
                    kind: expr::OpKind::Add,
                    operand_ty: ty::OpOperandType::I32,
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
        ExprData::CallOp(call_op) => {
            assert_eq!(
                types[call_op.args[0]].data(&db),
                &TypeData::Primitive(PrimitiveType::F32)
            );

            assert_eq!(
                types[call_op.args[1]].data(&db),
                &TypeData::Primitive(PrimitiveType::I32)
            );

            assert_eq!(
                types[call_op.op_expr].data(&db),
                &TypeData::Op(ty::OpType {
                    kind: expr::OpKind::Mul,
                    operand_ty: ty::OpOperandType::F32,
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

    let body_data = proc.body_data(&db);
    let root_exprs = self::expr_vec(&db, proc);
    let types = proc.type_table(&db);

    let x_pat = match &root_exprs[0].1 {
        ExprData::Let(let_) => {
            match types[let_.rhs].data(&db) {
                TypeData::Primitive(ty::PrimitiveType::F32) => {}
                x => unreachable!("{x:?}"),
            }

            match types[let_.pat].data(&db) {
                TypeData::Primitive(prim) => {
                    assert_eq!(prim, &ty::PrimitiveType::F32,);
                }
                x => unreachable!("{x:?}"),
            }

            let_.pat
        }

        _ => unreachable!(),
    };

    match &root_exprs[1].1 {
        ExprData::CallOp(op_call) => {
            assert_eq!(
                body_data.tables[op_call.args[0]]
                    .clone()
                    .into_path()
                    .segments[0]
                    .as_str(&db),
                "x"
            );
            // both `x` have to point to the same type data
            assert_eq!(types[op_call.args[0]], types[x_pat]);
        }

        x => unreachable!("{x:?}"),
    }
}
