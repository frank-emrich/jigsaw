
open Jigsaw_ppx_shared
open Ast_versioning.Ast
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



let check_feature_exists ctx loc (feature_name : Analysis_data.feature_id) =
  if Context.has_feature ctx feature_name then
    ()
  else
    Errors.raise_error loc ("Unknown feature : " ^ feature_name ^ ". Maybe you forgot adding the library defining it as a dependency?")


let check_feature_function_exists ctx loc (feature_name : Analysis_data.feature_id) (feature_function_name : Analysis_data.feature_function_id) =
  match Context.get_feature_function_of_feature ctx feature_name feature_function_name with
    | None -> Errors.raise_error loc ("Feature function unknown: " ^ feature_function_name)
    | Some _ -> ()


let check_no_label_in_feature_function_impl loc (label : Asttypes.arg_label) =
  match label with
    | Nolabel -> ()
    | _ -> Errors.raise_error loc "No labelled or optional parameters supported for feature functions"