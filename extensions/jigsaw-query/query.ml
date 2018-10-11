(*module type MOTHER = 
sig
    type overall_ext_type
    type overall_ext_term
    type overall_ext_value

    val stringify_overall_ext_type : overall_ext_type -> string
    val stringify_overall_ext_type : overall_ext_term -> string

end*)



let store : (int * string) list ref = ref []
let counter = ref 0



type query_value_ext = QueryHandle of int

type query_type_ext = QueryHandleT

type ('ext_term, 'ext_typ) query_term_ext =
  | CreateCell 
  | Update of
      ('ext_term, 'ext_typ) Core.Ir.core_term
      * ('ext_term, 'ext_typ) Core.Ir.core_term
  | Query of ('ext_term, 'ext_typ) Core.Ir.core_term

let query_typecheck
    (check_ext_term :
      'ext_typ Core.Ir.tenv -> 'ext_term -> 'ext_typ Core.Types.core_type)
    (lift_type: query_type_ext -> 'ext_typ Core.Types.core_type)
    (unlift_type : 'ext_typ Core.Types.core_type -> query_type_ext)
    (env : 'ext_typ Core.Ir.tenv) (term : ('ext_term, 'ext_typ) query_term_ext)
    =
  match term with
  | CreateCell -> lift_type QueryHandleT 
  | Update (id, value) -> (
    match
      ( unlift_type (Core.Ir.core_typecheck check_ext_term env id)
      , Core.Ir.core_typecheck check_ext_term env value )
    with
    | QueryHandleT, _ -> Core.Types.UnitT
    | _ -> raise (Core.Ir.TypeError "type problem in Update") )
  | Query id -> (
    match unlift_type (Core.Ir.core_typecheck check_ext_term env id) with
    | QueryHandleT -> Core.Types.StringT
    | _ -> raise (Core.Ir.TypeError "type problem in Query") )


let query_eval
  (eval_ext_term  : ('ext_value, 'ext_term, 'ext_type) Core.Ir.venv -> 'ext_term -> ('ext_value, 'ext_term, 'ext_type) Core.Ir.core_value)
  (lift_value : query_value_ext -> ('ext_value, 'ext_term, 'ext_type) Core.Ir.core_value)
  (unlift_value : ('ext_value, 'ext_term, 'ext_type) Core.Ir.core_value -> query_value_ext)
  (stringify_term_ext : 'ext_term -> string)
  (stringify_type_ext : 'ext_type -> string)
  (env : ('ext_value, 'ext_term, 'ext_type) Core.Ir.venv) (term : ('ext_term, 'ext_type) query_term_ext) =
  let ce = Core.Ir.core_eval eval_ext_term env in
  match term with
    | CreateCell -> 
      let c = !counter in
      counter := !counter + 1;
      store := (c, "") :: !store;
      lift_value (QueryHandle c)
    | Update (reft, ut) -> 
      begin match unlift_value (ce reft) with
       | QueryHandle c ->
          store := ((c, Stringify.Stringify.stringify_term stringify_term_ext stringify_type_ext ut) :: !store) ; Core.Ir.UnitV  
      | _ -> failwith "eval error" 
      end
   | Query id ->
    match unlift_value (ce id) with
      | QueryHandle i -> Core.Ir.StringV (List.assoc i !store)
      | _-> failwith "eval error" 



