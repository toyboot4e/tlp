//! Type inference

use crate::ir::ty::TypeData;

// let rec occur r1 = function (* occur check *)
//   | Type.Fun(t2s, t2) -> List.exists (occur r1) t2s || occur r1 t2
//   | Type.Tuple(t2s) -> List.exists (occur r1) t2s
//   | Type.Array(t2) -> occur r1 t2
//   | Type.Var(r2) when r1 == r2 -> true
//   | Type.Var({ contents = None }) -> false
//   | Type.Var({ contents = Some(t2) }) -> occur r1 t2
//   | _ -> false

// let rec unify t1 t2 = (* 型が合うように、型変数への代入をする *)
//   match t1, t2 with
//   | Type.Unit, Type.Unit | Type.Bool, Type.Bool | Type.Int, Type.Int | Type.Float, Type.Float -> ()
//   | Type.Fun(t1s, t1'), Type.Fun(t2s, t2') ->
//       (try List.iter2 unify t1s t2s
//       with Invalid_argument("List.iter2") -> raise (Unify(t1, t2)));
//       unify t1' t2'
//   | Type.Tuple(t1s), Type.Tuple(t2s) ->
//       (try List.iter2 unify t1s t2s
//       with Invalid_argument("List.iter2") -> raise (Unify(t1, t2)))
//   | Type.Array(t1), Type.Array(t2) -> unify t1 t2
//   | Type.Var(r1), Type.Var(r2) when r1 == r2 -> ()
//   | Type.Var({ contents = Some(t1') }), _ -> unify t1' t2
//   | _, Type.Var({ contents = Some(t2') }) -> unify t1 t2'
//   | Type.Var({ contents = None } as r1), _ -> (* 一方が未定義の型変数の場合 *)
//       if occur r1 t2 then raise (Unify(t1, t2));
//       r1 := Some(t2)
//   | _, Type.Var({ contents = None } as r2) ->
//       if occur r2 t1 then raise (Unify(t1, t2));
//       r2 := Some(t1)
//   | _, _ -> raise (Unify(t1, t2))

#[cfg(test)]
mod test {
    //
}
