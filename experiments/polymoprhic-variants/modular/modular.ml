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
  | `LamV of Core.var * 'term * ('value Core.venv)
  | `RecLamV  of Core.var * Core.var  * 'term * ('value Core.venv)
]


type ('term, 'typ) pterm = [
   `IntE of int
  | `BoolE of bool
  | `StringE of string
  | `LamE of Core.var * 'typ * 'term
  | `RecLamE of Core.var * 'typ * Core.var * 'typ  * 'term
  | `PlusE of 'term * 'term
  | `VarE of Core.var
  | `AppE of 'term * 'term
  | `IfE of 'term * 'term * 'term
  | `IntEq of 'term * 'term
  |  `Let of Core.var * 'term * 'term
]


type typ = typ ptyp
type term = (term, typ) pterm
type value = (value, term) pvalue




let rec typecheck (env : typ Core.tenv) (term : term) : typ = match term with
     # Core.core_term as term -> Core.core_typecheck typecheck env term
    |# Let.let_term as term -> Let.let_typecheck typecheck env term
    |#Arith.arith_term as term -> Arith.arith_typecheck typecheck env term



let rec eval (env : value Core.venv) (term: term) : value =
  match term with
    #Core.core_term  as t2 -> Core.core_eval eval env t2
    | #Let.let_term  as t2 -> Let.let_eval eval env t2
    |#Arith.arith_term as t2 -> Arith.arith_eval eval env t2



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