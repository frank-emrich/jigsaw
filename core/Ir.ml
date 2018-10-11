open Types

type var = string [@@deriving show]

type ('ext_value, 'ext_term, 'ext_type) core_value =
  | IntV of int
  | UnitV
  | BoolV of bool
  | StringV of string
  | LamV of var * 'ext_type core_type * ('ext_term, 'ext_type) core_term
  | ExtValue of 'ext_value

and ('ext_term, 'ext_type) core_term =
  | IntE of int
  | UnitE
  | BoolE of bool
  | StringE of string
  | LamE of var * 'ext_type core_type * ('ext_term, 'ext_type) core_term
  | PlusE of ('ext_term, 'ext_type) core_term * ('ext_term, 'ext_type) core_term
  | VarE of var
  | AppE of ('ext_term, 'ext_type) core_term * ('ext_term, 'ext_type) core_term
  | IfE of ('ext_term, 'ext_type) core_term * ('ext_term, 'ext_type) core_term * ('ext_term, 'ext_type) core_term
  (*| AssignE of var * ('ext_term, 'ext_type) core_term *)
  | ExtTerm of 'ext_term
[@@deriving show]

type ('ext_value, 'ext_term, 'ext_type) venv = (var * ('ext_value, 'ext_term, 'ext_type) core_value) list

type 'ext_type tenv = (var * 'ext_type core_type) list



exception TypeError of string

let rec core_typecheck (check_ext_term : 'ext_type tenv -> 'ext_term -> 'ext_type core_type) (env : 'ext_type tenv)
    (term : ('ext_term, 'ext_type) core_term) =
  match term with
  | ExtTerm ext -> check_ext_term env ext
  | IntE _ -> IntT
  | BoolE _ -> BoolT
  | UnitE -> UnitT
  | StringE _ -> StringT
  | LamE (v, vt, b) ->
      let env' = (v, vt) :: env in
      let bt = core_typecheck check_ext_term env' b in
      Arrow (vt, bt)
  | PlusE (l, r) -> (
    match (core_typecheck check_ext_term env l, core_typecheck check_ext_term env r) with
    | IntT, IntT -> IntT
    | _ -> raise (TypeError "adding non-ints") )
  | IfE (cond, t1, t2) -> (
    match (core_typecheck check_ext_term env cond, core_typecheck check_ext_term env t1, core_typecheck check_ext_term env t2) with
    | BoolT, typ1, typ2 when typ1 = typ2 -> typ1
    | _, typ1, typ2 when typ1 = typ2 -> raise (TypeError "Expecting bool in cond")
    | _ -> raise (TypeError "Expecting same types ") )
  | AppE (t1, t2) -> (
    match (core_typecheck check_ext_term env t1, core_typecheck check_ext_term env t2) with
    | Arrow (t11, t12), t2 when t11 = t2 -> t12
    | _ -> raise (TypeError "Wrong funcall type") )
  | VarE x -> ( try ListLabels.assoc x env with Not_found -> raise (TypeError ("Variable " ^ x ^ "Not found")) )



let rec core_eval (eval_ext_term : ('ext_value, 'ext_term, 'ext_type) venv -> 'ext_term -> ('ext_value, 'ext_term, 'ext_type) core_value)
    (env : ('ext_value, 'ext_term, 'ext_type) venv) (term : ('ext_term, 'ext_type) core_term) =
  let ce = core_eval eval_ext_term env in
  match term with
  | ExtTerm ext -> eval_ext_term env ext
  | IntE i -> IntV i
  | UnitE -> UnitV 
  | BoolE b -> BoolV b
  | StringE s -> StringV s
  | LamE (v, t, b) -> LamV (v, t, b)
  | PlusE (t1, t2) -> ( match (ce t1, ce t2) with IntV i1, IntV i2 -> IntV (i1 + i2) | _ -> failwith "eval fail" )
  | IfE (c, t1, t2) -> ( match ce c with BoolV b -> if b then ce t1 else ce t2 | _ -> failwith "eval fail" )
  | AppE (t1, t2) -> (
    match ce t1 with
    | LamV (v, _, b) ->
        let env' = (v, ce t2) :: env in
        core_eval eval_ext_term env' b
    | _ -> failwith "eval fail" )
  | VarE v -> List.assoc v env
