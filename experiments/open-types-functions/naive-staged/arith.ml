open Shared
open Arith_types




type ('term, 'typ) arith_tc_deps =
{
  atcd_lift_core_typ_to_typ : ('typ Core_types.core_typ -> 'typ) code;
  atcd_lift_arith_typ_to_typ :  (arith_typ -> 'typ) code;
  atcd_unlift_typ_to_arith_typ : ('typ -> (arith_typ) option) code;
  atcd_lift_arith_term_to_term : ('term   arith_term -> 'term) code;
  atcd_unlift_term_to_arith_term : ('term -> (('term)  arith_term) option) code;
  atcd_typecheck :  ('typ tenv -> 'term -> 'typ) code
}

let arith_typecheck_code (deps : ('term, 'typ) arith_tc_deps) : ( 'typ tenv -> ('term)  arith_term -> 'typ) code =
  .<
    fun
      (env )
      (term ) : 'typ ->
  let utc x =  .~(deps.atcd_unlift_typ_to_arith_typ)  (.~(deps.atcd_typecheck) env x) in
  match .~(deps.atcd_unlift_term_to_arith_term) (.~(deps.atcd_lift_arith_term_to_term) term) with
    | Some IntE _ -> .~(deps.atcd_lift_arith_typ_to_typ) IntT
    | Some (PlusE (l, r)) -> (
      match (utc l, utc r) with
      | Some IntT, Some IntT -> .~(deps.atcd_lift_arith_typ_to_typ) IntT
      | _ -> raise (TypeError "adding non-ints") )
    | Some (IntEq (t1, t2) ) -> (
      match (utc t1, utc t2) with
      | Some (IntT), Some IntT  -> .~(deps.atcd_lift_core_typ_to_typ) Core_types.BoolT
      | _ -> raise (TypeError "Wrong funcall type") )
    | None -> failwith "impossible"
  >.


type ('term, 'typ, 'value) arith_eval_deps =
{
  aed_lift_arith_value_to_value :  (arith_value -> 'value) code;
  aed_unlift_value_to_arith_value :  ('value -> arith_value option) code ;
  aed_lift_arith_term_to_term : (('term) arith_term -> 'term) code;
  aed_unlift_term_to_arith_term : ('term -> (('term) arith_term) option) code;
  aed_eval : ('value  venv -> 'term -> 'value) code;
  aed_lift_core_value_to_value : (('value, 'term) Core_types.core_value -> 'value) code

}




let arith_eval_code (deps: ('term, 'typ, 'value) arith_eval_deps) : ('value venv -> ('term) arith_term -> 'value ) code =
  .<
    fun
      (env  )
      (term   ) : 'value ->
    let ue x = .~(deps.aed_unlift_value_to_arith_value) (.~(deps.aed_eval) env x) in
    match .~(deps.aed_unlift_term_to_arith_term) (.~(deps.aed_lift_arith_term_to_term) term) with
    | Some (IntE i) -> .~(deps.aed_lift_arith_value_to_value) (IntV i)
    | Some (PlusE (t1, t2)) -> ( match (ue t1, ue t2) with Some (IntV i1), Some (IntV i2) -> .~(deps.aed_lift_arith_value_to_value) (IntV (i1 + i2)) | _ -> failwith "eval fail" )
    | Some (IntEq (t1, t2)) ->
      (match ue t1, ue t2 with
        | Some (IntV i1), Some (IntV i2) ->
          .~(deps.aed_lift_core_value_to_value) (Core_types.BoolV (i1 = i2))
        | _ -> failwith "Evaluation error")
    | None -> failwith "Impossible"
  >.

