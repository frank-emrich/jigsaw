(* variant of query.ml where we provide some info via a functor *)

(*
let store : (int * string) list ref = ref []
let counter = ref 0



type query_value = QueryHandle of int [@@extension_of value]

type query_typ = QueryHandleT [@@extension_of typ]

type 'term query_term =
| CreateCell
| Update of
    'term
    * 'term
| Query of 'term
[@@extension_of term]


module QueryImpls( Deps :
        sig
            type term
            type value
            type typ
            val lift_query_typ : query_typ -> typ
            val unlift_query_typ :  typ -> query_typ option
            val lift_query_value : query_value -> value
            val unlift_query_value : value -> query_value option
            val lift_core_typ : typ Core.Types.core_typ -> typ
            val lift_core_value : (term, typ) Core.Ir.core_value -> value
            val stringify_term : term -> string
            val stringify_typ : typ -> string
        end [@inject] ) =
struct
    open Deps


    let query_typecheck
        (typecheck : typ Core.Ir.tenv -> term -> typ)
        (env : typ Core.Ir.tenv) (term : term query_term) : typ
        =
    let utc t = unlift_query_typ (typecheck env t) in
    let tc t = typecheck env in
    match term with
    | CreateCell -> lift_query_typ QueryHandleT
    | Update (id, value) -> (
        match
        ( utc id
        , tc value)
        with
        | Some QueryHandleT, _ -> lift_core_typ (Core.Types.UnitT)
        | _ -> raise (Core.Ir.TypeError "type problem in Update") )
    | Query id -> (
        match utc id with
        | Some QueryHandleT -> lift_core_typ (Core.Types.StringT)
        | _ -> raise (Core.Ir.TypeError "type problem in Query") )


    let query_eval
    (eval  : value Core.Ir.venv -> term -> value)
    (env : value Core.Ir.venv) (term : term query_term) : value =
    let ue x = unlift_query_value (eval env x) in
    match term with
        | CreateCell ->
        let c = !counter in
        counter := !counter + 1;
        store := (c, "") :: !store;
        lift_query_value (QueryHandle c)
        | Update (reft, ut) ->
        begin match ue reft with
        | Some (QueryHandle c) ->
            store := ((c, stringify_term ut) :: !store) ; lift_core_value Core.Ir.UnitV
        | _ -> failwith "eval error"
        end
    | Query id ->
        match ue id with
        | Some (QueryHandle i) -> lift_core_value (Core.Ir.StringV (List.assoc i !store))
        | _-> failwith "eval error"

end

*)