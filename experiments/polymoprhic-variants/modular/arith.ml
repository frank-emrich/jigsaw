open Shared

type arith_typ =
  [ `IntT ]
    [@@deriving show]


type arith_value =
  [ `IntV of int ]
    [@@deriving show]


type 'term arith_term =
  [ `IntE of int
  | `PlusE of 'term * 'term
  | `IntEq of 'term * 'term ]
    [@@deriving show]







let arith_typecheck
      (typecheck )
      (env : 'typ tenv)
      (term : ('term)  arith_term) : 'typ =
  let utc x =  (typecheck env x) in
  match term with
    | `IntE _ -> `IntT
    | (`PlusE (l, r)) -> (
      match (utc l, utc r) with
      | `IntT,  `IntT ->  `IntT
      | _ -> raise (TypeError "adding non-ints") )
    | (`IntEq (t1, t2) ) -> (
      match (utc t1, utc t2) with
      | (`IntT), `IntT  ->  `BoolT
      | _ -> raise (TypeError "Wrong funcall type") )




let arith_eval
      (eval)
      (env : 'value venv)
      (term : ('term) arith_term ) : 'value =
    let ue x =  (eval env x) in
    match term with
    | (`IntE i) -> (`IntV i)
    | (`PlusE (t1, t2)) -> ( match (ue t1, ue t2) with  (`IntV i1),  (`IntV i2) -> (`IntV (i1 + i2)) | _ -> failwith "eval fail" )
    | (`IntEq (t1, t2)) ->
      (match ue t1, ue t2 with
        | (`IntV i1), (`IntV i2) ->
          (`BoolV (i1 = i2))
        | _ -> failwith "Evaluation error")
