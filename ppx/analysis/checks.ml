
open Jigsaw_ppx_shared
(*open Ast_versioning.Ast*)
open Ast_versioning.Parsetree

let check_no_extensibility_related_attributes ?(except=None) (attrs : attributes) =
  let is_forbidden_attr (attr : attribute) =
    let name = (fst attr).txt in
    match except with
      | None ->
        List.mem name Names.Attributes.all
      | Some allowed ->
        name <> allowed && List.mem name Names.Attributes.all in
  match List.find_opt is_forbidden_attr attrs with
    | None -> ()
    | Some forbidden_attr ->
      Errors.raise_error
        (fst forbidden_attr).loc
        ("Found extensibility-related attribute \"" ^ (fst forbidden_attr).txt ^ "\" in an unsupported location")

let check_type_extension_not_inside_of_injection_functor  ctx loc =
  if Context.current_module_path_is_simple ctx then
    ()
  else
    Errors.raise_error loc "This type extension definition is inside of an injection functor, which is not allowed.
                        Refer to extensible types by using polymorphic variables with the same name as the desired extensible type"

let check_attr_payload_empty (attr : attribute) =
  if Ast_manipulation.attribute_has_empty_payload attr then
    ()
  else
    let attr_name = (fst attr).txt in
    Errors.raise_error (fst attr).loc ("Attribute " ^ attr_name ^ " should not have a payload")

let check_no_type_parameters (td_record : type_declaration) =
  match td_record.ptype_params with
    | [] -> ()
    | _ -> Errors.raise_error td_record.ptype_loc "Type parameters not supported here"


