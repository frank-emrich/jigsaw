(*open Migrate_parsetree
open Ast_versioning.Ast*)
open Ast_versioning.Parsetree

let make_ghost_location () = Location.in_file !Location.input_name

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


let make_single_ident_content_payload (content : Longident.t) : payload =
  let ghost_loc = make_ghost_location () in
  let content_loced = Location.mkloc content ghost_loc in
  let eval_structure_item = Pstr_eval ({pexp_desc= Pexp_ident content_loced; pexp_loc= ghost_loc; pexp_attributes= []}, []) in
  PStr [{pstr_desc= eval_structure_item; pstr_loc= ghost_loc}]

let make_single_ident_content_attribute name content : attribute =
  let ghost_loc = make_ghost_location () in
  let name_loced = Location.mkloc name ghost_loc in
  let payload = make_single_ident_content_payload content in
  (name_loced, payload)

let extract_single_ident_expr (expr : expression ) : Longident.t =
  Errors.debug ("Expr: " ^ Errors.string_of_expression expr);
  match expr.pexp_desc with
    | Pexp_ident lid -> lid.txt
    | Pexp_constant (Pconst_string (s, _)) -> Longident.parse s
    | _ -> Errors.raise_error expr.pexp_loc "Expected single (possibly qualified) identifier"

let extract_single_string_constant (expr : expression ) : string =
  match expr.pexp_desc with
    | Pexp_constant (Pconst_string (s, _)) -> s
    | _ -> Errors.raise_error expr.pexp_loc "Expected single string constant"


let attribute_has_name attribute_name (attribute : attribute) =
  (fst attribute).txt = attribute_name



let attribute_has_empty_payload attribute =
  match snd attribute with
    | PStr [] -> true
    |_ -> false





let get_library_name_from_first_stage_marker_extension item =
  match item.pstr_desc with
    | Pstr_extension ((extension_loced, _), [(attr_name_loced, attr_payload)]) when  extension_loced.txt = Names.Extensions.first_stage_marker ->
      Some  (Longident.last (extract_single_ident_payload attr_name_loced.loc attr_payload))
    | _ -> None


let is_first_stage_marker_extension item =
  match get_library_name_from_first_stage_marker_extension item with
    | None -> false
    | Some _ -> true