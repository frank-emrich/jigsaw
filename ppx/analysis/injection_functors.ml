open Jigsaw_ppx_shared.Ast_versioning.Ast
open Jigsaw_ppx_shared.Ast_versioning.Parsetree


let unloc (loc : 'a Asttypes.loc) = loc.txt
(*let raise_error = Jigsaw_ppx_shared.Errors.raise_error
let info_message _loc_opt = Jigsaw_ppx_shared.Errors.info
let debug_message _loc_opt = Jigsaw_ppx_shared.Errors.debug*)


module AD = Jigsaw_ppx_shared.Analysis_data
module E = Jigsaw_ppx_shared.Errors
module U = Jigsaw_ppx_shared.Utils



let validate_type_decl overall_loc type_decl =
    let name = unloc type_decl.ptype_name in
    let kind = type_decl.ptype_kind in
    let manifest = type_decl.ptype_manifest in
    match kind, manifest with
      | Ptype_abstract, None ->
        (* TODO: check that the name matches a known extensible type *)
        AD.InjectionType name
      | _ -> E.raise_error overall_loc "Only abstract types without manifest supported in injection functor argument"

let check_extensible_type_name extensible_type_core_type expected_extensible_type_name =
  let desc = extensible_type_core_type.ptyp_desc in
  let loc = extensible_type_core_type.ptyp_loc in
  let error () = E.raise_error loc (Printf.sprintf "Expected type %s here" expected_extensible_type_name) in
  match desc with
    | Ptyp_constr (ident_loced, []) ->
      let name = Longident.flatten ident_loced.txt in
      begin match name with
        | [type_name] when type_name = expected_extensible_type_name -> ()
        | _ -> E.raise_error loc (Printf.sprintf "Expected unqualified name of extensible type here, namely type %s" expected_extensible_type_name)
      end
    | _ -> error ()

let check_type_extension extension_type_core_type (expected_extension_type_name : AD.type_extension_id) =
  let desc = extension_type_core_type.ptyp_desc in
  let loc = extension_type_core_type.ptyp_loc in
  let extension_name_string = AD.show_tei expected_extension_type_name in
  match desc with
    | Ptyp_constr (ident_loced, _args) ->
      (* TODO: We could check here that the right arguments are given to the type extension *)
      if ident_loced.txt = expected_extension_type_name then
        ()
      else
        E.raise_error loc ("Expected type used here to refer to type extension named " ^ extension_name_string)
    | _ -> E.raise_error loc (Printf.sprintf "Expected type refering to the type extension named %s" extension_name_string )

let check_optionized_extension_type opt_extension_type_core_type expected_extension_type_name =
    let desc = opt_extension_type_core_type.ptyp_desc in
    let loc = opt_extension_type_core_type.ptyp_loc in
    match desc with
      | Ptyp_constr (lid, [arg]) when String.concat "." (Longident.flatten lid.txt ) = "option"->
        check_type_extension arg expected_extension_type_name
      | _ ->
        (* TODO: Eventually, determine the fully applied extension type here to print it*)
        E.raise_error loc "Right-hand side type of unlift function should be option of a type extension"

let validate_lift_function_decl ctx previous loc function_name function_type extended_type (type_extension : string) =
  (* TODO Validate existence of _type_extension *)
  if List.exists ((=) (AD.InjectionType extended_type)) previous then
    let function_type_desc = function_type.ptyp_desc in
    let function_type_loc = function_type.ptyp_loc in
    let extension_identifier = match Context.get_extensions_by_unqualified_name ctx extended_type type_extension with
      | [] -> E.raise_error loc (Printf.sprintf "No extension %s of type %s known" type_extension extended_type)
      | [ext] -> ext.te_extension_id
      | _ -> E.raise_error loc (Printf.sprintf "There is more than one extension of %s with (unqualified) name %s" extended_type type_extension) in
    match function_type_desc with
      | Ptyp_arrow (Nolabel, from_type, to_type) ->
        check_extensible_type_name to_type extended_type;
        check_type_extension from_type extension_identifier;
        AD.InjectionLift (function_name, extended_type, extension_identifier)
      | _ -> E.raise_error function_type_loc "A lift function should have a type of the shape some_type_extension  -> extensible_type"
  else
    E.raise_error
        loc
        (Printf.sprintf
            "Function %s refers to the extensible type %s, but it has not been declared inside of the injection functor argument"
            function_name
            extended_type)

(* TODO de-duplicate w.r.t  validate_lift_function_decl*)
let validate_unlift_function_decl ctx previous loc function_name function_type extended_type (type_extension : string) =
  if List.exists ((=) (AD.InjectionType extended_type)) previous then
    let function_type_desc = function_type.ptyp_desc in
    let function_type_loc = function_type.ptyp_loc in
    let extension_identifier = match Context.get_extensions_by_unqualified_name ctx extended_type type_extension with
      | [] -> E.raise_error loc (Printf.sprintf "No extension %s of type %s known" type_extension extended_type)
      | [ext] -> ext.te_extension_id
      | _ -> E.raise_error loc (Printf.sprintf "There is more than one extension of %s with (unqualified) name %s" extended_type type_extension) in
    match function_type_desc with
      | Ptyp_arrow (Nolabel, from_type, to_type) ->
        check_extensible_type_name from_type extended_type;
        check_optionized_extension_type to_type extension_identifier;
        AD.InjectionUnlift (function_name, extended_type, extension_identifier)

      | _ -> E.raise_error function_type_loc "An unlift function should have a type of the shape  -> some_type_extension option"
  else
    E.raise_error
        loc
        (Printf.sprintf
            "Function %s refers to the extensible type %s, but it has not been declared inside of the injection functor argument"
            function_name
            extended_type)

let validate_feature_function _previous function_name =
  (* TODO implement properly *)
  AD.InjectionFeatureFunction (function_name, "implement", "me")


let handle_injection_functor_argument_signature ctx argument_signature =
  let handle_type _previous loc (type_decl : type_declaration) =
     validate_type_decl loc type_decl in
  let handle_value previous (value_desc : value_description) =
    let name = unloc value_desc.pval_name in
    let loc = value_desc.pval_name.loc in
    let value_type = value_desc.pval_type in
    let parse_lift = Jigsaw_ppx_shared.Names.parse_lift_function_name in
    let parse_unlift = Jigsaw_ppx_shared.Names.parse_unlift_function_name in
    match (parse_lift name, parse_unlift name) with
      | Some (extended_type, type_extension), None ->
        E.debug
            ~loc:(Some loc)
            (Printf.sprintf "Based on its name, function %s has been determined to be a lifting function from %s to %s"
                name type_extension extended_type);
        validate_lift_function_decl ctx previous loc name value_type extended_type type_extension
      | None, Some (extended_type, type_extension) ->
        E.debug
            ~loc:(Some loc)
            (Printf.sprintf "Based on its name, function %s has been determined to be a un-lifting function from %s to %s"
                name  extended_type type_extension);
        validate_unlift_function_decl ctx previous loc name value_type extended_type type_extension
      | Some _, Some _ -> failwith "Function name must not indicate lift and unlift at the same time"
      | None, None ->
        E.debug (Printf.sprintf "Assuming that function %s corresponds to a feature function" name);
        validate_feature_function previous name
         in

  let handle_item previous (argument_signature_item : signature_item) =
    match argument_signature_item.psig_desc with
      | Psig_type (_rec_flag, type_decls) ->
        let loc = argument_signature_item.psig_loc in
        List.fold_left (fun prev_inner type_decl ->  (handle_type prev_inner loc  type_decl) :: prev_inner ) previous type_decls
      | Psig_value (value_desc : value_description) ->
        [handle_value previous value_desc]
      | _ -> E.raise_error argument_signature_item.psig_loc "Unsupported item in injection functor" in
  List.rev (List.fold_left handle_item [] argument_signature)

let handle_injection_functor ctx module_type =
  let desc = module_type.pmty_desc in
  let loc = module_type.pmty_loc in
  match desc with
    | Pmty_signature signature ->
      handle_injection_functor_argument_signature ctx signature
    | _ -> E.raise_error loc "The type of an injection functor argument should be stated as an in-line signature (i.e., sig ... end)"