
(* The union of all extensions of the core types *)
type typ =
   | CoreTyp of typ Core.Types.core_typ 
   | QueryTyp of Query.query_typ

(* The union of all extensions of the core terms *)
type term =
   | CoreTerm of (term, typ) Core.Ir.core_term
   | QueryTerm of term Query.query_term  
   | LetTerm of term Let.let_term

(* The union of all extensions of the core values *)
type value =
   | CoreValue of (term, typ) Core.Ir.core_value
   | QueryValue of Query.query_value  

let lift_query_typ (qt : Query.query_typ) = QueryTyp qt

(* TODO: express unlift in terms of option type for nicer pattern matching *)
let unlift_query_typ (t : typ) = match t with
    | QueryTyp qt -> Some qt
    | _ -> None
let lift_query_value (qv : Query.query_value) =  QueryValue qv
let unlift_query_value (v : value) = match v with
   | QueryValue qv -> Some qv
   | _ -> None
let lift_query_term (qt : term Query.query_term) = QueryTerm qt

let lift_let_term (lt : term Let.let_term) =  LetTerm lt
let lift_core_typ (ct : typ Core.Types.core_typ) : typ = CoreTyp ct
let lift_core_term (ct : (term, typ) Core.Ir.core_term) : term = CoreTerm ct
let lift_core_value (v : (term, typ) Core.Ir.core_value) : value = CoreValue v
let unlift_core_value (v : value) = match v with
   | CoreValue v -> Some v
   | _ -> None 
let unlift_core_typ (t: typ) = match t with
   | CoreTyp t -> Some t
   | _ -> None  

let rec typecheck env (term : term) = match term with
    | CoreTerm t -> Core.Ir.core_typecheck typecheck lift_core_typ unlift_core_typ env t
    | QueryTerm t -> Query.query_typecheck typecheck lift_query_typ lift_core_typ unlift_query_typ  env t
    | LetTerm t -> Let.let_typecheck typecheck env t

let rec stringify_typ t = match t with
   | CoreTyp t -> Stringify.stringify_core_typ stringify_typ t
   | QueryTyp t -> Stringify_query.Stringify_for_query.stringify_query_typ  t

let rec stringify_term t = match t with
   | CoreTerm t -> Stringify.stringify_core_term stringify_term stringify_typ t
   | QueryTerm t -> Stringify_query.Stringify_for_query.stringify_query_term stringify_term t
   | LetTerm t -> Stringify_let.Stringify_for_let.stringify_let_term stringify_term t

let stringify_value (v : value) : string = match v with
   | CoreValue v -> Stringify.stringify_core_value stringify_typ stringify_term v
   | QueryValue v -> Stringify_query.Stringify_for_query.stringify_query_value v


let rec eval (env : value Core.Ir.venv) (term: term) : value = match term with
   | CoreTerm t -> Core.Ir.core_eval eval lift_core_value unlift_core_value env t 
   | QueryTerm (t : term Query.query_term) -> Query.query_eval eval lift_query_value lift_core_value unlift_query_value stringify_term stringify_typ env t
   | LetTerm t -> Let.let_eval eval env t 






(* Everything above this line should eventually be generated from the core + extensions *)

let sample_term1 : term =
    lift_core_term (IfE 
            (lift_core_term (BoolE true),
            lift_core_term (IntE 3),
            lift_core_term (IntE 4)))

let sample_term2 : term = 
    lift_let_term
         (LetE 
            ("x",
            lift_query_term (CreateCell),
            
                lift_let_term (LetE 
                    ("y",
                    lift_query_term (Update (lift_core_term (VarE "x"), sample_term1)),
                    lift_query_term (Query (lift_core_term (VarE "x")) ) ))))


let result = eval [] sample_term2 
let _ = print_endline (stringify_typ (typecheck [] sample_term1))
let _ = print_endline (stringify_value result)