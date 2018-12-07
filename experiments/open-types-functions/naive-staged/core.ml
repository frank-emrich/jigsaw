open Shared
open Core_types

(* Core *)


type ('term, 'typ) core_tc_deps =
{
  cctd_lift_core_typ_to_typ : ('typ core_typ -> 'typ) code;
  cctd_unlift_typ_to_core_typ : ('typ -> ('typ core_typ) option) code;
  cctd_lift_core_term_to_term : (('term, 'typ) core_term -> 'term) code;
  cctd_unlift_term_to_core_term : ('term -> (('term,'typ) core_term) option) code;
  cctd_typecheck :  ('typ tenv -> 'term -> 'typ) code
}


 let core_typecheck_code (deps : ('term, 'typ) core_tc_deps) : ('typ tenv -> ('term, 'typ) core_term -> 'typ ) code =
  .<
    fun
      env
      term
        (*: ('typ tenv -> ('term, 'typ) core_term -> 'typ)*) ->
    let utc x =  .~(deps.cctd_unlift_typ_to_core_typ)  (.~(deps.cctd_typecheck)  env x ) in
    let tc x = ( .~(deps.cctd_typecheck) env x) in
    match .~(deps.cctd_unlift_term_to_core_term) ( .~(deps.cctd_lift_core_term_to_term) term) with
    | Some BoolE _ -> .~(deps.cctd_lift_core_typ_to_typ) BoolT
    | Some StringE _ -> .~(deps.cctd_lift_core_typ_to_typ) StringT
    | Some (LamE (v, vt, b)) ->
        let env' = (v, vt) :: env in
        let bt = .~(deps.cctd_typecheck) env' b in
        .~(deps.cctd_lift_core_typ_to_typ) (Arrow (vt, bt))
    | Some (RecLamE (f, rt, v, vt, b)) ->
        let ft = .~(deps.cctd_lift_core_typ_to_typ) (Arrow (vt, rt)) in
        let env' = (f, ft) :: (v, vt) :: env in
        let bt = .~(deps.cctd_typecheck) env' b in
        if bt = rt then
          ft
        else
          raise (TypeError "wrong type annotation in RecLamE")

    | Some (IfE (cond, t1, t2)) -> (
      match (utc cond, tc t1, tc t2) with
      | Some BoolT, typ1, typ2 when typ1 = typ2 -> typ1
      | _, typ1, typ2 when typ1 = typ2 -> raise (TypeError "Expecting bool in cond")
      | _ -> raise (TypeError "Expecting same types ") )
    | Some (AppE (t1, t2)) -> (
      match (utc t1, tc t2) with
      | Some (Arrow (t11, t12)), t2 when t11 = t2 -> t12
      | _ -> raise (TypeError "Wrong funcall type") )
    | Some (VarE x) ->
      ( try ListLabels.assoc x env with Not_found -> raise (TypeError ("Variable " ^ x ^ "Not found")) )

    | None -> failwith "Impossible"
  >.

type ('term, 'typ, 'value) core_eval_deps =
{
  ced_lift_core_typ_to_typ : ('typ core_typ -> 'typ) code;
  ced_unlift_typ_to_core_typ : ('typ -> ('typ core_typ) option) code;
  ced_lift_core_term_to_term : (('term, 'typ) core_term -> 'term) code;
  ced_unlift_term_to_core_term : ('term -> (('term,'typ) core_term) option) code;
  ced_lift_core_value_to_value : (('value, 'term) core_value -> 'value) code;
  ced_unlift_value_to_core_value : ('value -> (('value, 'term) core_value) option) code;
  ced_eval : ('value  venv -> 'term -> 'value) code
}


let core_eval_code (deps: ('term, 'typ, 'value) core_eval_deps) : ('value venv -> ('term, 'typ) core_term -> 'value) code =
  .< fun
      env
      term ->
    let ue x = .~(deps.ced_unlift_value_to_core_value) ( .~(deps.ced_eval) env x) in
    let e x = .~(deps.ced_eval) env x in
    match .~(deps.ced_unlift_term_to_core_term) ( .~(deps.ced_lift_core_term_to_term) term) with
    | Some (BoolE b) -> .~(deps.ced_lift_core_value_to_value) (BoolV b)
    | Some (StringE s) -> .~(deps.ced_lift_core_value_to_value) (StringV s)
    | Some (LamE (v, _t, b)) -> .~(deps.ced_lift_core_value_to_value) (LamV (v,  b, env))
    | Some (RecLamE (f, _rt, v, _vt, b)) -> .~(deps.ced_lift_core_value_to_value) (RecLamV (f, v, b, env))
    | Some (IfE (c, t1, t2)) -> ( match ue c with Some (BoolV b) -> if b then e t1 else e t2 | _ -> failwith "eval fail" )
    | Some (AppE (t1, t2)) -> (
      match ue t1 with
      | Some (LamV (v, b, env' )) ->
          let env'' = (v, e t2) :: env' in
          .~(deps.ced_eval) env'' b
      | Some (RecLamV (f, v, b, env' )) ->
          let lifted_recv = .~(deps.ced_lift_core_value_to_value)  (RecLamV (f,v,b, env')) in
          let env'' = (v, e t2) :: (f, lifted_recv) :: env' in
          .~(deps.ced_eval) env'' b
      | _ -> failwith "eval fail" )
    | Some (VarE v) -> List.assoc v env
    | None -> failwith "Impossible"
  >.
