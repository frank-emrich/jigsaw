module Stringify : sig
  val stringify_type :
       ('ext_type -> string)
    -> 'ext_type Core.Types.core_type
    -> string

  val stringify_term :
       ('ext_term -> string)
    -> ('ext_type -> string)
    -> ('ext_term, 'ext_type) Core.Ir.core_term
    -> string

  val stringify_value : 
    ('ext_term -> string)
    -> ('ext_type -> string)
    -> ('ext_value -> string)
    -> (('ext_value, 'ext_term, 'ext_type) Core.Ir.core_value)
    -> string

end = struct
  let rec stringify_type ext_type_stringifier t =
    let st = stringify_type ext_type_stringifier in
    match t with
    | Core.Types.BoolT -> "Bool"
    | StringT -> "String" 
    | UnitT -> "Unit"
    | IntT -> "Int"
    | Arrow (t1, t2) -> st t1 ^ " -> " ^ st t2
    | ExtTyp ext -> ext_type_stringifier ext

  and stringify_term ext_term_stringifier ext_type_stringifier
      (term : ('ext_term, 'ext_type) Core.Ir.core_term) =
    let nct = stringify_term ext_term_stringifier ext_type_stringifier in
    match term with
    | IntE i -> string_of_int i 
    | BoolE b -> string_of_bool b 
    | UnitE -> "()" 
    | VarE v -> v 
    | LamE (v, t, b) -> "\\" ^ v ^ " : " ^  stringify_type  ext_type_stringifier t ^ " . " ^ nct b
    | StringE s -> String.capitalize_ascii s
    | PlusE (t1, t2) -> nct t1 ^ " + " ^ nct t2
    | AppE (t1, t2) -> "(" ^ nct t1 ^ " " ^  nct t2 ^ ")"
    | IfE (c, t1, t2) -> Format.sprintf "if (%s) then (%s) else (%s)" (nct c) (nct t1)  (nct t2) 
    | ExtTerm e -> ext_term_stringifier e
    
  and stringify_value ext_term_stringifier ext_type_stringifier ext_value_stringifier value = 
    let sty = stringify_type ext_type_stringifier in
    let sterm = stringify_term ext_term_stringifier ext_type_stringifier in
    match value with
    | Core.Ir.IntV i -> string_of_int i
    | UnitV -> "unit"
    | BoolV b -> string_of_bool b
    | StringV s -> "\"" ^ s ^ "\""
    | LamV (v, t, b) ->  "\\" ^ v ^ " : " ^  sty   t ^ " . " ^ sterm b   
    | ExtValue e -> ext_value_stringifier e  
end