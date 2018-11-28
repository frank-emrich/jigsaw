
type var = string [@@deriving show]

type 'value venv = (var * 'value) list [@@deriving show]

type 'typ tenv = (var * 'typ) list [@@deriving show]

exception TypeError of string

(* Core *)

type 'typ core_typ =
  | BoolT
  | IntT
  | StringT
  | Arrow of 'typ * 'typ
    [@@deriving show]


type ('value, 'term) core_value =
  | IntV of int
  | BoolV of bool
  | StringV of string
  | LamV of var * 'term * ('value venv)
  | RecLamV  of var * var  * 'term * ('value venv)
    [@@deriving show]






and ('term, 'typ) core_term =
  | IntE of int
  | BoolE of bool
  | StringE of string
  | LamE of var * 'typ * 'term
  | RecLamE of var * 'typ * var * 'typ  * 'term
  | PlusE of 'term * 'term
  | VarE of var
  | AppE of 'term * 'term
  | IfE of 'term * 'term * 'term
  | IntEq of 'term * 'term
    [@@deriving show]



type ('term, 'typ) core_tc_deps =
{
  cctd_lift_core_typ_to_typ : 'typ core_typ -> 'typ;
  cctd_unlift_typ_to_core_typ : 'typ -> ('typ core_typ) option;
  cctd_lift_core_term_to_term : ('term, 'typ) core_term -> 'term;
  cctd_unlift_term_to_core_term : 'term -> (('term,'typ) core_term) option;
  cctd_typecheck :  'typ tenv -> 'term -> 'typ
} [@@deriving show]


 let core_typecheck
      (deps : ('term, 'typ) core_tc_deps)
      (env : 'typ tenv)
      (term : ('term, 'typ) core_term) : 'typ =
    let utc x =  deps.cctd_unlift_typ_to_core_typ  (deps.cctd_typecheck env x) in
    let tc x = (deps.cctd_typecheck env x) in
    match deps.cctd_unlift_term_to_core_term (deps.cctd_lift_core_term_to_term term) with
    | Some IntE _ -> deps.cctd_lift_core_typ_to_typ IntT
    | Some BoolE _ -> deps.cctd_lift_core_typ_to_typ BoolT
    | Some StringE _ -> deps.cctd_lift_core_typ_to_typ StringT
    | Some (LamE (v, vt, b)) ->
        let env' = (v, vt) :: env in
        let bt = deps.cctd_typecheck env' b in
        deps.cctd_lift_core_typ_to_typ (Arrow (vt, bt))
    | Some (RecLamE (f, rt, v, vt, b)) ->
        let ft = deps.cctd_lift_core_typ_to_typ (Arrow (vt, rt)) in
        let env' = (f, ft) :: (v, vt) :: env in
        let bt = deps.cctd_typecheck env' b in
        if bt = rt then
          ft
        else
          raise (TypeError "wrong type annotation in RecLamE")
    | Some (PlusE (l, r)) -> (
      match (utc l, utc r) with
      | Some IntT, Some IntT -> deps.cctd_lift_core_typ_to_typ IntT
      | _ -> raise (TypeError "adding non-ints") )
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
    | Some (IntEq (t1, t2) ) -> (
      match (utc t1, utc t2) with
      | Some (IntT), Some IntT  -> deps.cctd_lift_core_typ_to_typ BoolT
      | _ -> raise (TypeError "Wrong funcall type") )
    | None -> failwith "Impossible"

type ('term, 'typ, 'value) core_eval_deps =
{
  ced_lift_core_typ_to_typ : 'typ core_typ -> 'typ;
  ced_unlift_typ_to_core_typ : 'typ -> ('typ core_typ) option;
  ced_lift_core_term_to_term : ('term, 'typ) core_term -> 'term;
  ced_unlift_term_to_core_term : 'term -> (('term,'typ) core_term) option;
  ced_lift_core_value_to_value : ('value, 'term) core_value -> 'value;
  ced_unlift_value_to_core_value : 'value -> (('value, 'term) core_value) option;
  ced_eval : 'value  venv -> 'term -> 'value
} [@@deriving show]


  let core_eval
      (deps: ('term, 'typ, 'value) core_eval_deps)
      (env : 'value venv)
      (term : ('term, 'typ) core_term) : 'value =
    let ue x = deps.ced_unlift_value_to_core_value (deps.ced_eval env x) in
    let e x = deps.ced_eval env x in
    match deps.ced_unlift_term_to_core_term (deps.ced_lift_core_term_to_term term) with
    | Some (IntE i) -> deps.ced_lift_core_value_to_value (IntV i)
    | Some (BoolE b) -> deps.ced_lift_core_value_to_value (BoolV b)
    | Some (StringE s) -> deps.ced_lift_core_value_to_value (StringV s)
    | Some (LamE (v, _t, b)) -> deps.ced_lift_core_value_to_value (LamV (v,  b, env))
    | Some (RecLamE (f, _rt, v, _vt, b)) -> deps.ced_lift_core_value_to_value (RecLamV (f, v, b, env))
    | Some (PlusE (t1, t2)) -> ( match (ue t1, ue t2) with Some (IntV i1), Some (IntV i2) -> deps.ced_lift_core_value_to_value (IntV (i1 + i2)) | _ -> failwith "eval fail" )
    | Some (IfE (c, t1, t2)) -> ( match ue c with Some (BoolV b) -> if b then e t1 else e t2 | _ -> failwith "eval fail" )
    | Some (AppE (t1, t2)) -> (
      match ue t1 with
      | Some (LamV (v, b, (env' : 'value venv ))) ->
          let env'' = (v, e t2) :: env' in
          deps.ced_eval env'' b
      | Some (RecLamV (f, v, b, (env' : 'value venv ))) ->
          let lifted_recv = deps.ced_lift_core_value_to_value  (RecLamV (f,v,b, env')) in
          let env'' = (v, e t2) :: (f, lifted_recv) :: env' in
          deps.ced_eval env'' b
      | _ -> failwith "eval fail" )
    | Some (VarE v) -> List.assoc v env
    | Some (IntEq (t1, t2)) ->
      (match ue t1, ue t2 with
        | Some (IntV i1), Some (IntV i2) ->
          deps.ced_lift_core_value_to_value (BoolV (i1 = i2))
        | _ -> failwith "Evaluation error")
    | None -> failwith "Impossible"

(* Let *)



type 'term let_term =
  | Let of var * 'term * 'term
    [@@deriving show]


type ('term, 'typ) let_tc_deps =
{
  ltcd_lift_let_term_to_term : 'term   let_term -> 'term;
  ltcd_unlift_term_to_let_term : 'term -> (('term)  let_term) option;
  ltcd_typecheck :  'typ tenv -> 'term -> 'typ
} [@@deriving show]

let let_typecheck
      (deps : ('term, 'typ) let_tc_deps)
      (env : 'typ tenv)
      (term : ('term)  let_term) : 'typ =
    match deps.ltcd_unlift_term_to_let_term (deps.ltcd_lift_let_term_to_term term) with
      | Some (Let (var, te1, te2)) ->
        let ty1 = deps.ltcd_typecheck env te1 in
        let env' = (var, ty1) :: env in
        deps.ltcd_typecheck env' te2
      | None -> failwith "Impossible"


type ('term, 'typ,  'value) let_eval_deps =
{
  led_lift_let_term_to_term : ('term)  let_term -> 'term;
  led_unlift_term_to_let_term : 'term -> (('term)  let_term) option;
  led_eval : 'value  venv -> 'term -> 'value
} [@@deriving show]

let let_eval
   (deps: ('term, 'typ, 'value) let_eval_deps)
   (env : 'value venv)
   (term : ('term) let_term) : 'value =
   match deps.led_unlift_term_to_let_term (deps.led_lift_let_term_to_term term) with
    | Some (Let (var, te1, te2)) ->
      let v1 = deps.led_eval env te1 in
      let env' = (var, v1) :: env in
      deps.led_eval env' te2
    | None -> failwith "Impossible"





type typ =
   | CoreTyp of typ core_typ
  [@@deriving show]

(* The union of all extensions of the core terms *)
type term =
   | CoreTerm of (term, typ) core_term
   | LetTerm of term let_term
  [@@deriving show]


(* The union of all extensions of the core values *)
type value =
   | CoreValue of (value, term) core_value
  [@@deriving show]


let lift_let_term (lt : term let_term) =  LetTerm lt
let lift_core_typ (ct : typ core_typ) : typ = CoreTyp ct
let lift_core_term (ct : (term, typ) core_term) : term = CoreTerm ct
let lift_core_value (v : (value, term) core_value) : value = CoreValue v
let unlift_core_value (v : value) = match v with
   | CoreValue v -> Some v
let unlift_core_typ (t: typ) = match t with
   | CoreTyp t -> Some t

let unlift_core_term (t : term) = match t with
  | CoreTerm t -> Some t
  | _ -> None

let unlift_let_term (t : term) = match t with
  | LetTerm t -> Some t
  | _ -> None

let rec typecheck env (term : term) : typ = match term with
    | CoreTerm (t : (term, typ) core_term) ->
      let (deps : (term, typ) core_tc_deps) = {
        cctd_lift_core_typ_to_typ =  lift_core_typ;
        cctd_unlift_typ_to_core_typ = unlift_core_typ;
        cctd_lift_core_term_to_term = lift_core_term;
        cctd_unlift_term_to_core_term = unlift_core_term;
        cctd_typecheck = typecheck
      } in
      let ct : ((term, typ) core_tc_deps -> typ tenv -> (term, typ) core_term -> typ) = core_typecheck in
      ct deps env t
    | LetTerm t ->
      let deps = {
        ltcd_lift_let_term_to_term = lift_let_term;
        ltcd_unlift_term_to_let_term = unlift_let_term;
        ltcd_typecheck = typecheck
      } in
    let_typecheck deps env t



let rec eval (env : value venv) (term: term) : value = match term with
   | CoreTerm t ->
      let deps =
      {
        ced_lift_core_typ_to_typ = lift_core_typ;
        ced_unlift_typ_to_core_typ = unlift_core_typ;
        ced_lift_core_term_to_term = lift_core_term;
        ced_unlift_term_to_core_term = unlift_core_term;
        ced_lift_core_value_to_value = lift_core_value;
        ced_unlift_value_to_core_value = unlift_core_value;
        ced_eval = eval;
      } in
    core_eval deps env t
   | LetTerm t ->
      let deps = {
        led_lift_let_term_to_term = lift_let_term;
        led_unlift_term_to_let_term = unlift_let_term;
        led_eval = eval;
      } in
   let_eval deps env t


(* example program:
  let rec sum x =
    if x = 1 then
      1
    else
      let m = x-1 in
      let sm1 = sum m in
      let sm2 = sum m in (* provoke exponential number of calls *)
      1 + sm
  sum 500

*)


let recLamE (a,b,c,d,e) = lift_core_term (RecLamE (a,b,c,d,e))

let ifE (a,b,c) = lift_core_term (IfE (a,b,c))
let intEq (a,b) = lift_core_term (IntEq (a,b))
let varE x = lift_core_term (VarE x)
let intE x = lift_core_term (IntE x)
let plus (a,b) = lift_core_term (PlusE (a,b))
let app (a,b) = lift_core_term (AppE (a,b))

let letE (a,b,c) = lift_let_term (Let (a,b,c))

let intT = lift_core_typ IntT




let else_branch : term =
  letE (
    "m",
    plus (
      varE "x",
      intE (-1)),
    letE (
      "sm1",
      app (
        varE "sum",
        varE "m"),
      letE (
        "sm2",
         app (
           varE "sum",
           varE "m"),
        plus (
          intE 1,
          varE "sm1")
      )

    )
  )


let sum =
  recLamE ("sum", intT, "x", intT,
    ifE (
      intEq (
        varE "x",
        intE 0),
      intE 0,
      else_branch
    ))


let program =
  letE (
    "sum",
    sum,
    app (
      varE "sum",
      intE 22
    )
  )

let t = typecheck [] program

let _ = print_endline (show_typ t)

let res = eval [] program

let _ = print_endline (show_value res)