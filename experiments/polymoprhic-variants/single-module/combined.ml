(* TODO: This does not use the singled-out arith extension, yet *)


type var = string 

type 'value venv = (var * 'value) list 

type 'typ tenv = (var * 'typ) list 

exception TypeError of string

(* Core *)

type 'typ core_typ =
  [ `BoolT
  | `StringT
  | `Arrow of 'typ * 'typ ]
    


type ('value, 'term) core_value =
  [ `BoolV of bool
  | `StringV of string
  | `LamV of var * 'term * ('value venv)
  | `RecLamV  of var * var  * 'term * ('value venv) ]
    






and ('term, 'typ) core_term =
  [ `BoolE of bool
  | `StringE of string
  | `LamE of var * 'typ * 'term
  | `RecLamE of var * 'typ * var * 'typ  * 'term
  | `VarE of var
  | `AppE of 'term * 'term
  | `IfE of 'term * 'term * 'term ]
    




 let core_typecheck
      typecheck
      (env : 'typ tenv)
      (term : ('term, 'typ) core_term) : 'typ =
    let tc x = (typecheck env x) in
    match term with
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





  let core_eval
      (eval : 'value venv -> 'term -> 'value)
      (env : 'value venv)
      (term : ('term, 'typ) core_term) : 'value =
    let e x = eval env x in
    match term with
    |  (`BoolE b) ->  (`BoolV b)
    |  (`StringE s) -> (`StringV s)
    |  (`LamE (v, _t, b)) -> (`LamV (v,  b, env))
    |  (`RecLamE (f, _rt, v, _vt, b)) ->  (`RecLamV (f, v, b, env))
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





type arith_typ =
  [ `IntT ]
    


type arith_value =
  [ `IntV of int ]
    


type 'term arith_term =
  [ `IntE of int
  | `PlusE of 'term * 'term
  | `IntEq of 'term * 'term ]
    







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





type 'term let_term =
  [ `Let of var * 'term * 'term ]
    




let let_typecheck
      typecheck
      (env : 'typ tenv)
      (term : ('term)  let_term) : 'typ =
    match term with
      | (`Let (var, te1, te2)) ->
        let ty1 = typecheck env te1 in
        let env' = (var, ty1) :: env in
        typecheck env' te2


let let_eval
   eval
   (env : 'value venv)
   (term : ('term) let_term) : 'value =
   match term with
    | (`Let (var, te1, te2)) ->
      let v1 = eval env te1 in
      let env' = (var, v1) :: env in
      eval env' te2


type 'typ ptyp =
 [
  | `BoolT
  | `IntT
  | `StringT
  | `Arrow of 'typ * 'typ
 ]  


type ('value, 'term) pvalue = [
   `IntV of int
  | `BoolV of bool
  | `StringV of string
  | `LamV of var * 'term * ('value venv)
  | `RecLamV  of var * var  * 'term * ('value venv)
]  


type ('term, 'typ) pterm = [
   `IntE of int
  | `BoolE of bool
  | `StringE of string
  | `LamE of var * 'typ * 'term
  | `RecLamE of var * 'typ * var * 'typ  * 'term
  | `PlusE of 'term * 'term
  | `VarE of var
  | `AppE of 'term * 'term
  | `IfE of 'term * 'term * 'term
  | `IntEq of 'term * 'term
  |  `Let of var * 'term * 'term
]  


type typ = typ ptyp 
type term = (term, typ) pterm 
type value = (value, term) pvalue 




let rec typecheck (env : typ tenv) (term : term) : typ = match term with
     # core_term as term -> core_typecheck typecheck env term
    |# let_term as term -> let_typecheck typecheck env term
    |#arith_term as term -> arith_typecheck typecheck env term



let rec eval (env : value venv) (term: term) : value =
  match term with
    #core_term  as t2 -> core_eval eval env t2
    | #let_term  as t2 -> let_eval eval env t2
    |#arith_term as t2 -> arith_eval eval env t2



(* Same example program as for open types and functions *)






let else_branch : term =
  `Let (
    "m",
    `PlusE (
      `VarE "x",
      `IntE (-1)),
    `Let (
      "sm1",
      `AppE (
        `VarE "sum",
        `VarE "m"),
      `Let (
        "sm2",
         `AppE (
           `VarE "sum",
           `VarE "m"),
        `PlusE (
          `IntE 1,
          `VarE "sm1")
      )

    )
  )


let sum =
  `RecLamE ("sum", `IntT, "x", `IntT,
    `IfE (
      `IntEq (
       `VarE "x",
        `IntE 0),
      `IntE 0,
      else_branch
    ))


let program =
  `Let (
    "sum",
    sum,
    `AppE (
      `VarE "sum",
      `IntE 23
    )
  )

let t = typecheck [] program

(*let _ = print_endline (show_typ t)*)

let res = eval [] program

(*let _ = print_endline (show_value res)*)