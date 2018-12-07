open Shared

(* Core *)

type 'typ core_typ =
  | BoolT
  | StringT
  | Arrow of 'typ * 'typ



type ('value, 'term) core_value =
  | BoolV of bool
  | StringV of string
  | LamV of var * 'term * ('value venv)
  | RecLamV  of var * var  * 'term * ('value venv)







and ('term, 'typ) core_term =
  | BoolE of bool
  | StringE of string
  | LamE of var * 'typ * 'term
  | RecLamE of var * 'typ * var * 'typ  * 'term
  | VarE of var
  | AppE of 'term * 'term
  | IfE of 'term * 'term * 'term


type ('term, 'typ) core_tc_deps =
{
  cctd_lift_core_typ_to_typ : 'typ core_typ -> 'typ;
  cctd_unlift_typ_to_core_typ : 'typ -> ('typ core_typ) option;
  cctd_lift_core_term_to_term : ('term, 'typ) core_term -> 'term;
  cctd_unlift_term_to_core_term : 'term -> (('term,'typ) core_term) option;
  cctd_typecheck :  'typ tenv -> 'term -> 'typ
}


 let core_typecheck
      (deps : ('term, 'typ) core_tc_deps)
      (env : 'typ tenv)
      (term : ('term, 'typ) core_term) : 'typ =
    let utc x =  deps.cctd_unlift_typ_to_core_typ  (deps.cctd_typecheck env x) in
    let tc x = (deps.cctd_typecheck env x) in
    match term with
    | BoolE _ -> deps.cctd_lift_core_typ_to_typ BoolT
    | StringE _ -> deps.cctd_lift_core_typ_to_typ StringT
    | (LamE (v, vt, b)) ->
        let env' = (v, vt) :: env in
        let bt = deps.cctd_typecheck env' b in
        deps.cctd_lift_core_typ_to_typ (Arrow (vt, bt))
    | (RecLamE (f, rt, v, vt, b)) ->
        let ft = deps.cctd_lift_core_typ_to_typ (Arrow (vt, rt)) in
        let env' = (f, ft) :: (v, vt) :: env in
        let bt = deps.cctd_typecheck env' b in
        if bt = rt then
          ft
        else
          raise (TypeError "wrong type annotation in RecLamE")

    | (IfE (cond, t1, t2)) -> (
      match (utc cond, tc t1, tc t2) with
      | Some BoolT, typ1, typ2 when typ1 = typ2 -> typ1
      | _, typ1, typ2 when typ1 = typ2 -> raise (TypeError "Expecting bool in cond")
      | _ -> raise (TypeError "Expecting same types ") )
    | (AppE (t1, t2)) -> (
      match (utc t1, tc t2) with
      | Some (Arrow (t11, t12)), t2 when t11 = t2 -> t12
      | _ -> raise (TypeError "Wrong funcall type") )
    | (VarE x) ->
      ( try ListLabels.assoc x env with Not_found -> raise (TypeError ("Variable " ^ x ^ "Not found")) )


type ('term, 'typ, 'value) core_eval_deps =
{
  ced_lift_core_typ_to_typ : 'typ core_typ -> 'typ;
  ced_unlift_typ_to_core_typ : 'typ -> ('typ core_typ) option;
  ced_lift_core_term_to_term : ('term, 'typ) core_term -> 'term;
  ced_unlift_term_to_core_term : 'term -> (('term,'typ) core_term) option;
  ced_lift_core_value_to_value : ('value, 'term) core_value -> 'value;
  ced_unlift_value_to_core_value : 'value -> (('value, 'term) core_value) option;
  ced_eval : 'value  venv -> 'term -> 'value
}


  let core_eval
      (deps: ('term, 'typ, 'value) core_eval_deps)
      (env : 'value venv)
      (term : ('term, 'typ) core_term) : 'value =
    let ue x = deps.ced_unlift_value_to_core_value (deps.ced_eval env x) in
    let e x = deps.ced_eval env x in
    match term with
    | (BoolE b) -> deps.ced_lift_core_value_to_value (BoolV b)
    | (StringE s) -> deps.ced_lift_core_value_to_value (StringV s)
    | (LamE (v, _t, b)) -> deps.ced_lift_core_value_to_value (LamV (v,  b, env))
    | (RecLamE (f, _rt, v, _vt, b)) -> deps.ced_lift_core_value_to_value (RecLamV (f, v, b, env))
    | (IfE (c, t1, t2)) -> ( match ue c with Some (BoolV b) -> if b then e t1 else e t2 | _ -> failwith "eval fail" )
    | (AppE (t1, t2)) -> (
      match ue t1 with
      | Some (LamV (v, b, (env' : 'value venv ))) ->
          let env'' = (v, e t2) :: env' in
          deps.ced_eval env'' b
      | Some (RecLamV (f, v, b, (env' : 'value venv ))) ->
          let lifted_recv = deps.ced_lift_core_value_to_value  (RecLamV (f,v,b, env')) in
          let env'' = (v, e t2) :: (f, lifted_recv) :: env' in
          deps.ced_eval env'' b
      | _ -> failwith "eval fail" )
    | (VarE v) -> List.assoc v env
