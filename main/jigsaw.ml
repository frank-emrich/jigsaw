
(* The union of all extensions of the core types *)
type typ = [%synthesized_type typ]

(* The union of all extensions of the core terms *)
type term = [%synthesized_type term]

(* The union of all extensions of the core values *)
type value = [%synthesized_type value]



let rec typecheck env (term : term) = match term with
    | Ext_term_core_term t -> Core.Ir.core_typecheck typecheck typ_of_core_typ core_typ_of_typ env t
    | Ext_term_query_term t -> Query.query_typecheck typecheck typ_of_query_typ typ_of_core_typ query_typ_of_typ  env t
    | Ext_term_let_term t -> Let.let_typecheck typecheck env t

let rec stringify_typ t = match t with
   | Ext_typ_core_typ t -> Stringify.stringify_core_typ stringify_typ t
   | Ext_typ_query_typ t -> Stringify_query.Stringify_for_query.stringify_query_typ  t

let rec stringify_term t = match t with
   | Ext_term_core_term t -> Stringify.stringify_core_term stringify_term stringify_typ t
   | Ext_term_query_term t -> Stringify_query.Stringify_for_query.stringify_query_term stringify_term t
   | Ext_term_let_term t -> Stringify_let.Stringify_for_let.stringify_let_term stringify_term t

let stringify_value (v : value) : string = match v with
   | Ext_value_core_value v -> Stringify.stringify_core_value stringify_typ stringify_term v
   | Ext_value_query_value v -> Stringify_query.Stringify_for_query.stringify_query_value v


let rec eval (env : value Core.Ir.venv) (term: term) : value = match term with
   | Ext_term_core_term t -> Core.Ir.core_eval eval value_of_core_value core_value_of_value env t
   | Ext_term_query_term (t : term Query.query_term) -> Query.query_eval eval value_of_query_value value_of_core_value query_value_of_value stringify_term stringify_typ env t
   | Ext_term_let_term t -> Let.let_eval eval env t






(* Everything above this line should eventually be generated from the core + extensions *)

let sample_term1 : term =
    term_of_core_term (IfE
            (term_of_core_term (BoolE true),
            term_of_core_term (IntE 3),
            term_of_core_term (IntE 4)))

let sample_term2 : term =
    term_of_let_term
         (LetE
            ("x",
            term_of_query_term (CreateCell),

                term_of_let_term (LetE
                    ("y",
                    term_of_query_term (Update (term_of_core_term (VarE "x"), sample_term1)),
                    term_of_query_term (Query (term_of_core_term (VarE "x")) ) ))))


let result = eval [] sample_term2
let _ = print_endline (stringify_typ (typecheck [] sample_term1))
let _ = print_endline (stringify_value result)