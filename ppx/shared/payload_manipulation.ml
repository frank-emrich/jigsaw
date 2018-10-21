open Migrate_parsetree
open Ast_407
open Parsetree


let extract_single_ident_payload loc payload =
    let bad_shape loc =
    Errors.raise_error loc
      ( "Expected a single identifier" )
  in
    match payload with
    | PStr [{pstr_desc= eval_expr; pstr_loc= loc}] -> (
      match eval_expr with
      | Pstr_eval ({pexp_desc= Pexp_ident extended_type_name; pexp_loc= _; pexp_attributes= []}, []) ->
          extended_type_name.txt
      | _ -> bad_shape loc  )
    | _ -> bad_shape loc
