
(* The union of all extensions of the core types *)
type ext_type =
   | QueryTypeExt of Query.query_type_ext

(* The union of all extensions of the core terms *)
type ext_term =
   | QueryTermExt of (ext_term, ext_type) Query.query_term_ext  
   | LetTermExt of  (ext_term, ext_type) Let.let_term_ext

(* The union of all extensions of the core values *)
type ext_value =
   | QueryValueExt of Query.query_value_ext  

let lift_query_type (qt : Query.query_type_ext) = Core.Types.ExtTyp (QueryTypeExt qt)
let unlift_query_type (t : ext_type Core.Types.core_type) = match t with
    | Core.Types.ExtTyp (QueryTypeExt qt) -> qt
    | _ -> failwith "illegal use of unlift_query_type" 
let lift_query_value (qv : Query.query_value_ext) = Core.Ir.ExtValue (QueryValueExt qv)
let unlift_query_value (v : (ext_value, ext_term, ext_type) Core.Ir.core_value) = match v with
   | Core.Ir.ExtValue (QueryValueExt qv) -> qv
   | _ -> failwith "Illegal use of  unlift_query_value" 
let lift_query_term (qt : (ext_term, ext_type) Query.query_term_ext) = Core.Ir.ExtTerm (QueryTermExt qt)

let lift_let_term (lt : (ext_term, ext_type) Let.let_term_ext) =  Core.Ir.ExtTerm (LetTermExt lt)

(* The overall type for types*)
type typ = ext_type Core.Types.core_type
(* The overall type for terms *)
type term = (ext_term, ext_type) Core.Ir.core_term

let rec ext_typecheck env (term : ext_term) = match term with
    | QueryTermExt e -> Query.query_typecheck ext_typecheck lift_query_type unlift_query_type env e
    | LetTermExt e -> Let.let_typecheck ext_typecheck env e

let rec stringify_ext_type t = match t with
   | QueryTypeExt e -> Stringify_query.Stringify_for_query.stringify_query_type stringify_ext_type e

let rec stringify_ext_term t = match t with
   | QueryTermExt e -> Stringify_query.Stringify_for_query.stringify_query_term stringify_ext_term stringify_ext_type e
   | LetTermExt e -> Stringify_let.Stringify_for_let.stringify_let_term stringify_ext_term stringify_ext_type e

let rec stringify_ext_value (v : ext_value) : string = match v with
   | QueryValueExt e -> Stringify_query.Stringify_for_query.stringify_query_value stringify_ext_term stringify_ext_type stringify_ext_value e


let rec ext_eval env (term: ext_term) = match term with
   | QueryTermExt e -> Query.query_eval ext_eval lift_query_value unlift_query_value stringify_ext_term stringify_ext_type env e
   | LetTermExt e -> Let.let_eval ext_eval env e 





let typecheck = Core.Ir.core_typecheck ext_typecheck  
let eval = Core.Ir.core_eval ext_eval
let stringify_term = Stringify.Stringify.stringify_term stringify_ext_term stringify_ext_type
let stringify_type = Stringify.Stringify.stringify_type  stringify_ext_type
let stringify_value = Stringify.Stringify.stringify_value stringify_ext_term stringify_ext_type stringify_ext_value

(* Everything above this line should eventually be generated from the core + extensions *)

let sample_term1 : term =
    IfE 
        (BoolE true,
         IntE 3,
         IntE 4)

let sample_term2 : term = 
    lift_let_term
        (LetE 
            ("x",
            lift_query_term (CreateCell),
            lift_let_term
                (LetE 
                    ("y",
                    lift_query_term (Update ((VarE "x"), sample_term1)),
                    lift_query_term (Query (VarE "x") ) ))))


let result = eval [] sample_term2 
let _ = print_endline (stringify_type (typecheck [] sample_term1))
let _ = print_endline (stringify_value result)