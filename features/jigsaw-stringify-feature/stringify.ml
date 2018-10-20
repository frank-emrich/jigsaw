module type STRINGIFY = sig
  val stringify_type : 'typ -> string

  val stringify_term : 'term -> string

  val stringify_value : 'value -> string

end [@@feature_decl ]

let stringify_core_typ (stringify_typ : 'typ -> string) (t : 'typ Core.Types.core_typ) =
  let st = stringify_typ  in
  match t with
  | Core.Types.BoolT -> "Bool"
  | StringT -> "String"
  | UnitT -> "Unit"
  | IntT -> "Int"
  | Arrow (t1, t2) -> st t1 ^ " -> " ^ st t2

and stringify_core_term (stringify_term : 'term -> string) (stringify_type : 'typ -> string)
    (term : ('term, 'typ) Core.Ir.core_term) =
  let st = stringify_term  in
  match term with
  | IntE i -> string_of_int i
  | BoolE b -> string_of_bool b
  | UnitE -> "()"
  | VarE v -> v
  | LamE (v, t, b) -> "\\" ^ v ^ " : " ^  stringify_type   t ^ " . " ^ st b
  | StringE s -> String.capitalize_ascii s
  | PlusE (t1, t2) -> st t1 ^ " + " ^ st t2
  | AppE (t1, t2) -> "(" ^ st t1 ^ " " ^  st t2 ^ ")"
  | IfE (c, t1, t2) -> Format.sprintf "if (%s) then (%s) else (%s)" (st c) (st t1)  (st t2)

and stringify_core_value stringify_type stringify_term  value =
  let sty = stringify_type  in
  let sterm = stringify_term  in
  match value with
  | Core.Ir.IntV i -> string_of_int i
  | UnitV -> "unit"
  | BoolV b -> string_of_bool b
  | StringV s -> "\"" ^ s ^ "\""
  | LamV (v, t, b) ->  "\\" ^ v ^ " : " ^  sty   t ^ " . " ^ sterm b
   (*[ @@feature_impl STRINGIFY.stringify_value core_value  ] *)
