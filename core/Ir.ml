open Types

type var = string [@@deriving show]

type value [@@extensible_type]
type term [@@extensible_type]


type ('term, 'typ) core_value =
  | IntV of int
  | UnitV
  | BoolV of bool
  | StringV of string
  | LamV of var * 'typ * 'term [@@extension_of value]

and ('term, 'typ) core_term =
  | IntE of int
  | UnitE
  | BoolE of bool
  | StringE of string
  | LamE of var * 'typ * 'term
  | PlusE of 'term * 'term
  | VarE of var
  | AppE of 'term * 'term
  | IfE of 'term * 'term * 'term
    (*| AssignE of var * ('ext_term, 'ext_type) core_term *)
[@@deriving show] [@@extension_of term]

type 'value venv = (var * 'value) list

type 'typ tenv = (var * 'typ) list



(* variant 1: polymorphic types with matching names *)
module type TYPECHECK =
sig
  val typecheck : 'typ tenv -> 'term -> 'typ
end [@feature_decl typecheck ]

(* variant 2: parameterized module type *)
module type TYPECHECK2 = functor(D : sig type typ  type term end) ->
sig
 val typecheck : D.typ tenv -> D.term -> D.typ
end [@feature_decl typecheck ]

(* variant 3: just create fake types *)
module type TYPECHECK3 =
sig
 type typ
 type term
 val typecheck : typ tenv -> term -> typ
end [@feature_decl typecheck ]

(* variant 4: use something like [@InjectSynthesizedTypes ] or module%injecttypes *)
module WithTypes [@InjectSynthesizedTypes ] = (* alternatively: module%injecttypes  *)
struct

  module type TYPECHECK4 =
  sig
  val typecheck : typ tenv -> term -> typ
  end [@feature_decl typecheck ]

end


(* variant 5: just use type names as if they were defined, signature is removed *)
(*module type TYPECHECK5 =
sig
 val typecheck : typ tenv -> term -> typ
end [@feature_decl typecheck ] *)




exception TypeError of string

let core_typecheck
    (typecheck : 'typ tenv -> 'term -> 'typ)
    (lift_core_typ : 'typ core_typ -> 'typ)
    (unlift_core_typ : 'typ -> 'typ core_typ option)
    (env : 'typ tenv)
    (term : ('term, 'typ) core_term) : 'typ =
  let utc x =  unlift_core_typ  (typecheck env x) in
  let tc x = (typecheck env x) in
  match term with
  | IntE _ -> lift_core_typ IntT
  | BoolE _ -> lift_core_typ BoolT
  | UnitE -> lift_core_typ UnitT
  | StringE _ -> lift_core_typ StringT
  | LamE (v, vt, b) ->
      let env' = (v, vt) :: env in
      let bt = typecheck env' b in
      lift_core_typ (Arrow (vt, bt))
  | PlusE (l, r) -> (
    match (utc l, utc r) with
    | Some IntT, Some IntT -> lift_core_typ IntT
    | _ -> raise (TypeError "adding non-ints") )
  | IfE (cond, t1, t2) -> (
    match (utc cond, tc t1, tc t2) with
    | Some BoolT, typ1, typ2 when typ1 = typ2 -> typ1
    | _, typ1, typ2 when typ1 = typ2 -> raise (TypeError "Expecting bool in cond")
    | _ -> raise (TypeError "Expecting same types ") )
  | AppE (t1, t2) -> (
    match (utc t1, tc t2) with
    | Some (Arrow (t11, t12)), t2 when t11 = t2 -> t12
    | _ -> raise (TypeError "Wrong funcall type") )
  | VarE x -> ( try ListLabels.assoc x env with Not_found -> raise (TypeError ("Variable " ^ x ^ "Not found")) )




let core_eval
    (eval : 'value  venv -> 'term -> 'value)
    (lift_core_value : ('term, 'typ) core_value -> 'value )
    (unlift_core_value :  'value -> ('term, 'typ) core_value option)
    (env : 'value venv)
    (term : ('term, 'typ) core_term) : 'value =
  let ue x = unlift_core_value (eval env x) in
  let e x = eval env x in
  match term with
  | IntE i -> lift_core_value (IntV i)
  | UnitE -> lift_core_value UnitV
  | BoolE b -> lift_core_value (BoolV b)
  | StringE s -> lift_core_value (StringV s)
  | LamE (v, t, b) -> lift_core_value (LamV (v, t, b))
  | PlusE (t1, t2) -> ( match (ue t1, ue t2) with Some (IntV i1), Some (IntV i2) -> lift_core_value (IntV (i1 + i2)) | _ -> failwith "eval fail" )
  | IfE (c, t1, t2) -> ( match ue c with Some (BoolV b) -> if b then e t1 else e t2 | _ -> failwith "eval fail" )
  | AppE (t1, t2) -> (
    match ue t1 with
    | Some (LamV (v, _, b)) ->
        let env' = (v, e t2) :: env in
        eval env' b
    | _ -> failwith "eval fail" )
  | VarE v -> List.assoc v env
