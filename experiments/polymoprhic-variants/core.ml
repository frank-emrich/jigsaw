type var = string [@@deriving show]

type 'value venv = (var * 'value) list [@@deriving show]

type 'typ tenv = (var * 'typ) list [@@deriving show]

exception TypeError of string

(* Core *)

type 'typ core_typ =
  [ `BoolT
  | `IntT
  | `StringT
  | `Arrow of 'typ * 'typ ]
    [@@deriving show]


type ('value, 'term) core_value =
  [ `IntV of int
  | `BoolV of bool
  | `StringV of string
  | `LamV of var * 'term * ('value venv)
  | `RecLamV  of var * var  * 'term * ('value venv) ]
    [@@deriving show]






and ('term, 'typ) core_term =
  [ `IntE of int
  | `BoolE of bool
  | `StringE of string
  | `LamE of var * 'typ * 'term
  | `RecLamE of var * 'typ * var * 'typ  * 'term
  | `PlusE of 'term * 'term
  | `VarE of var
  | `AppE of 'term * 'term
  | `IfE of 'term * 'term * 'term
  | `IntEq of 'term * 'term ]
    [@@deriving show]




 let core_typecheck
      typecheck
      (env : 'typ tenv)
      (term : ('term, 'typ) core_term) : 'typ =
    let tc x = (typecheck env x) in
    match term with
    | `IntE _ -> `IntT
    | `BoolE _ -> `BoolT
    | `StringE _ ->  `StringT
    | (`LamE (v, vt, b)) ->
        let env' = (v, vt) :: env in
        let bt = typecheck env' b in
        (`Arrow (vt, bt))
    | (`RecLamE (f, rt, v, vt, b)) ->
        let ft = (`Arrow (vt, rt)) in
        let env' = (f, ft) :: (v, vt) :: env in
        let bt = typecheck env' b in
        if bt = rt then
          ft
        else
          raise (TypeError "wrong type annotation in RecLamE")
    | (`PlusE (l, r)) -> (
      match (tc l, tc r) with
      |  `IntT,  `IntT ->  `IntT
      | _ -> raise (TypeError "adding non-ints") )
    |  (`IfE (cond, t1, t2)) -> (
      match (tc cond, tc t1, tc t2) with
      |  `BoolT, typ1, typ2 when typ1 = typ2 -> typ1
      | _, typ1, typ2 when typ1 = typ2 -> raise (TypeError "Expecting bool in cond")
      | _ -> raise (TypeError "Expecting same types ") )
    | (`AppE (t1, t2)) -> (
      match (tc t1, tc t2) with
      | (`Arrow (t11, t12)), t2 when t11 = t2 -> t12
      | _ -> raise (TypeError "Wrong funcall type") )
    | (`VarE x) ->
      ( try ListLabels.assoc x env with Not_found -> raise (TypeError ("Variable " ^ x ^ "Not found")) )
    |  (`IntEq (t1, t2) ) -> (
      match (tc t1, tc t2) with
      | (`IntT),  `IntT  -> `BoolT
      | _ -> raise (TypeError "Wrong funcall type") )




  let core_eval
      (eval : 'value venv -> 'term -> 'value)
      (env : 'value venv)
      (term : ('term, 'typ) core_term) : 'value =
    let e x = eval env x in
    match term with
    |  (`IntE i) -> (`IntV i)
    |  (`BoolE b) ->  (`BoolV b)
    |  (`StringE s) -> (`StringV s)
    |  (`LamE (v, _t, b)) -> (`LamV (v,  b, env))
    |  (`RecLamE (f, _rt, v, _vt, b)) ->  (`RecLamV (f, v, b, env))
    |  (`PlusE (t1, t2)) -> ( match (e t1, e t2) with (`IntV i1), (`IntV i2) ->  (`IntV (i1 + i2)) | _ -> failwith "eval fail" )
    |  (`IfE (c, t1, t2)) -> ( match e c with (`BoolV b) -> if b then e t1 else e t2 | _ -> failwith "eval fail" )
    |  (`AppE (t1, t2)) -> (
      match e t1 with
      |  (`LamV (v, b, (env' : 'value venv ))) ->
          let env'' = (v, e t2) :: env' in
          eval env'' b
      |  (`RecLamV (f, v, b, (env' : 'value venv ))) ->
          let lifted_recv =  (`RecLamV (f,v,b, env')) in
          let env'' = (v, e t2) :: (f, lifted_recv) :: env' in
          eval env'' b
      | _ -> failwith "eval fail" )
    | (`VarE v) -> List.assoc v env
    | (`IntEq (t1, t2)) ->
      (match e t1, e t2 with
        | (`IntV i1),  (`IntV i2) ->
          (`BoolV (i1 = i2))
        | _ -> failwith "Evaluation error")
