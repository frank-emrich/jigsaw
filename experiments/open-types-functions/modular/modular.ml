open Shared


type typ =
   | CoreTyp of typ Core.core_typ
   | ArithTyp of Arith.arith_typ
  

(* The union of all extensions of the core terms *)
type term =
   | CoreTerm of (term, typ) Core.core_term
   | LetTerm of term Let.let_term
   | ArithTerm of term Arith.arith_term
  


(* The union of all extensions of the core values *)
type value =
   | CoreValue of (value, term) Core.core_value
   | ArithValue of Arith.arith_value
  


let lift_let_term (lt : term Let.let_term) =  LetTerm lt
let lift_core_typ (ct : typ Core.core_typ) : typ = CoreTyp ct
let lift_core_term (ct : (term, typ) Core.core_term) : term = CoreTerm ct
let lift_core_value (v : (value, term) Core.core_value) : value = CoreValue v
let unlift_core_value (v : value) = match v with
   | CoreValue v -> Some v
   | _ -> None
let unlift_core_typ (t: typ) = match t with
   | CoreTyp t -> Some t
   | _ -> None
let unlift_core_term (t : term) = match t with
  | CoreTerm t -> Some t
  | _ -> None

let unlift_let_term (t : term) = match t with
  | LetTerm t -> Some t
  | _ -> None


let unlift_arith_term (t :term) = match t with
  | ArithTerm t -> Some t
  | _ -> None

let unlift_arith_typ (t :typ) = match t with
  | ArithTyp t -> Some t
  | _ -> None


let unlift_arith_value (t :value) = match t with
  | ArithValue t -> Some t
  | _ -> None

let lift_arith_term (t : term Arith.arith_term) : term = ArithTerm t
let lift_arith_value (v : Arith.arith_value) : value = ArithValue v
let lift_arith_typ (t : Arith.arith_typ) : typ = ArithTyp t


let rec typecheck env (term : term) : typ = match term with
    | CoreTerm (t : (term, typ) Core.core_term) ->
      let (deps : (term, typ) Core.core_tc_deps) = {
        cctd_lift_core_typ_to_typ =  lift_core_typ;
        cctd_unlift_typ_to_core_typ = unlift_core_typ;
        cctd_lift_core_term_to_term = lift_core_term;
        cctd_unlift_term_to_core_term = unlift_core_term;
        cctd_typecheck = typecheck
      } in
      let ct : ((term, typ) Core.core_tc_deps -> typ tenv -> (term, typ) Core.core_term -> typ) = Core.core_typecheck in
      ct deps env t
    | LetTerm t ->
      let deps : (term, typ) Let.let_tc_deps = {
        ltcd_lift_let_term_to_term = lift_let_term;
        ltcd_unlift_term_to_let_term = unlift_let_term;
        ltcd_typecheck = typecheck
      } in
      Let.let_typecheck deps env t
    | ArithTerm t ->
      let deps : (term, typ) Arith.arith_tc_deps = {
        atcd_lift_core_typ_to_typ = lift_core_typ;
        atcd_lift_arith_typ_to_typ = lift_arith_typ;
        atcd_unlift_typ_to_arith_typ = unlift_arith_typ;
        atcd_lift_arith_term_to_term = lift_arith_term;
        atcd_unlift_term_to_arith_term = unlift_arith_term;
        atcd_typecheck = typecheck;
      } in
      Arith.arith_typecheck deps env t



let rec eval (env : value venv) (term: term) : value = match term with
   | CoreTerm t ->
      let deps : (term, typ, value) Core.core_eval_deps =
      {
        ced_lift_core_typ_to_typ = lift_core_typ;
        ced_unlift_typ_to_core_typ = unlift_core_typ;
        ced_lift_core_term_to_term = lift_core_term;
        ced_unlift_term_to_core_term = unlift_core_term;
        ced_lift_core_value_to_value = lift_core_value;
        ced_unlift_value_to_core_value = unlift_core_value;
        ced_eval = eval;
      } in
    Core.core_eval deps env t
   | LetTerm t ->
      let deps : (term, typ, value) Let.let_eval_deps = {
        led_lift_let_term_to_term = lift_let_term;
        led_unlift_term_to_let_term = unlift_let_term;
        led_eval = eval;
      } in
     Let.let_eval deps env t
    | ArithTerm t ->
      let deps : (term, typ, value ) Arith.arith_eval_deps = {
        aed_lift_arith_value_to_value = lift_arith_value;
        aed_unlift_value_to_arith_value = unlift_arith_value;
        aed_lift_arith_term_to_term = lift_arith_term;
        aed_unlift_term_to_arith_term = unlift_arith_term;
        aed_eval = eval;
        aed_lift_core_value_to_value = lift_core_value
      } in
      Arith.arith_eval deps env t


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
let intEq (a,b) = lift_arith_term (IntEq (a,b))
let varE x = lift_core_term (VarE x)
let intE x = lift_arith_term (IntE x)
let plus (a,b) = lift_arith_term (PlusE (a,b))
let app (a,b) = lift_core_term (AppE (a,b))

let letE (a,b,c) = lift_let_term (Let (a,b,c))

let intT = lift_arith_typ IntT




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
      intE 23
    )
  )

let t = typecheck [] program

(*let _ = print_endline (show_typ t)*)

let res = eval [] program

(*let _ = print_endline (show_value res)*)