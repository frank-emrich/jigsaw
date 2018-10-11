

let stringify_query_type _ext_type_stringifier query =
    match query with
       |  Query.QueryHandleT -> "QueryHandleT"

let stringify_query_term ext_term_stringifier ext_type_stringifier query_term = 
    let sfy = Stringify.Stringify.stringify_term ext_term_stringifier ext_type_stringifier in
    match query_term with 
        | Query.CreateCell -> "CreateCell"
        | Query.Update (refterm, vterm) -> "Update " ^ sfy refterm ^ " to " ^ sfy vterm
        | Query.Query refterm -> "Query " ^  sfy refterm

let stringify_query_value _ext_term_stringifier _ext_type_stringifier _ext_value_stringifier v =
    match v with
        Query.QueryHandle i -> "QueryHandle @ " ^ string_of_int i 