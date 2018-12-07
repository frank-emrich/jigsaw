open Shared

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