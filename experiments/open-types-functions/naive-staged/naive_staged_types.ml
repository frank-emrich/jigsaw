
type typ =
   | CoreTyp of typ Core_types.core_typ
   | ArithTyp of Arith_types.arith_typ


(* The union of all extensions of the core terms *)
type term =
   | CoreTerm of (term, typ) Core_types.core_term
   | LetTerm of term Let_types.let_term
   | ArithTerm of term Arith_types.arith_term



(* The union of all extensions of the core values *)
type value =
   | CoreValue of (value, term) Core_types.core_value
   | ArithValue of Arith_types.arith_value