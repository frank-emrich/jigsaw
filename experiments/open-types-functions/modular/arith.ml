open Shared

type arith_typ =
  | IntT



type arith_value =
  | IntV of int



type 'term arith_term =
  | IntE of int
  | PlusE of 'term * 'term
  | IntEq of 'term * 'term






type ('term, 'typ) arith_tc_deps =
{
  atcd_lift_core_typ_to_typ : 'typ Core.core_typ -> 'typ;
  atcd_lift_arith_typ_to_typ :  arith_typ -> 'typ;
  atcd_unlift_typ_to_arith_typ : 'typ -> (arith_typ) option;
  atcd_lift_arith_term_to_term : 'term   arith_term -> 'term;
  atcd_unlift_term_to_arith_term : 'term -> (('term)  arith_term) option;
  atcd_typecheck :  'typ tenv -> 'term -> 'typ
}

let arith_typecheck
      (deps : ('term, 'typ) arith_tc_deps)
      (env : 'typ tenv)
      (term : ('term)  arith_term) : 'typ =
  let utc x =  deps.atcd_unlift_typ_to_arith_typ  (deps.atcd_typecheck env x) in
  match term with
    | IntE _ -> deps.atcd_lift_arith_typ_to_typ IntT
    | (PlusE (l, r)) -> (
      match (utc l, utc r) with
      | Some IntT, Some IntT -> deps.atcd_lift_arith_typ_to_typ IntT
      | _ -> raise (TypeError "adding non-ints") )
    | (IntEq (t1, t2) ) -> (
      match (utc t1, utc t2) with
      | Some (IntT), Some IntT  -> deps.atcd_lift_core_typ_to_typ Core.BoolT
      | _ -> raise (TypeError "Wrong funcall type") )



type ('term, 'typ, 'value) arith_eval_deps =
{
  aed_lift_arith_value_to_value :  arith_value -> 'value;
  aed_unlift_value_to_arith_value :  'value -> arith_value option ;
  aed_lift_arith_term_to_term : ('term) arith_term -> 'term;
  aed_unlift_term_to_arith_term : 'term -> (('term) arith_term) option;
  aed_eval : 'value  venv -> 'term -> 'value;
  aed_lift_core_value_to_value : ('value, 'term) Core.core_value -> 'value

}




let arith_eval
      (deps: ('term, 'typ, 'value) arith_eval_deps)
      (env : 'value venv)
      (term : ('term) arith_term ) : 'value =
    let ue x = deps.aed_unlift_value_to_arith_value (deps.aed_eval env x) in
    match term with
    | (IntE i) -> deps.aed_lift_arith_value_to_value (IntV i)
    | (PlusE (t1, t2)) -> ( match (ue t1, ue t2) with Some (IntV i1), Some (IntV i2) -> deps.aed_lift_arith_value_to_value (IntV (i1 + i2)) | _ -> failwith "eval fail" )
    | (IntEq (t1, t2)) ->
      (match ue t1, ue t2 with
        | Some (IntV i1), Some (IntV i2) ->
          deps.aed_lift_core_value_to_value (Core.BoolV (i1 = i2))
        | _ -> failwith "Evaluation error")


