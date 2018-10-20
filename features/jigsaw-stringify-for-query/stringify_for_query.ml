
let stringify_query_typ  query =
    match query with
       |  Query.QueryHandleT -> "QueryHandleT"

let stringify_query_term stringify_term query_term =
    match query_term with
        | Query.CreateCell -> "CreateCell"
        | Query.Update (refterm, vterm) -> "Update " ^ stringify_term refterm ^ " to " ^ stringify_term vterm
        | Query.Query refterm -> "Query " ^  stringify_term refterm

let stringify_query_value v =
    match v with
        Query.QueryHandle i -> "QueryHandle @ " ^ string_of_int i