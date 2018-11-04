open Jigsaw_ppx_shared.Ast_versioning.Ast
open Jigsaw_ppx_shared.Ast_versioning.Parsetree
open Jigsaw_ppx_shared


let unloc (loc : 'a Asttypes.loc) = loc.txt

let handle_attribute _ (attr_loc, attr_payload) =
  let name = unloc attr_loc in
  let loc = attr_loc.loc in
  if name = Jigsaw_ppx_shared.Names.Attributes.extension_of then
    let long_id = (Jigsaw_ppx_shared.Ast_manipulation.extract_single_ident_payload loc attr_payload) in
    let id = String.concat "." (Longident.flatten long_id) in
    Some id
  else None




(* Does the declared type have a constructor to be used as an extension point *)
(* Not used at the time, we do not require extensible types to be explicitly declared *)
let handle_extensible_type ctx (td_record : type_declaration) =
  match td_record.ptype_kind with
  | Ptype_abstract ->
    let ext_attr_name = Jigsaw_ppx_shared.Names.Attributes.extensible_type in
    let extensible_type_attrs = List.filter (Ast_manipulation.attribute_has_name ext_attr_name) td_record.ptype_attributes in
    begin match extensible_type_attrs with
      | [] -> None
      | [attr] ->
        Checks.check_attr_payload_empty attr;
        Checks.check_no_type_parameters td_record;
        let declared_type_name = unloc td_record.ptype_name in
        if Context.has_extensible_type ctx declared_type_name then
          Errors.raise_error (fst attr).loc ("There already exists an extensible type named " ^ declared_type_name )
        else
          (Context.register_extensible_type ctx declared_type_name;
          Some td_record)
      | _ ->
        Errors.raise_error td_record.ptype_loc ("Found multiple attributes named " ^  ext_attr_name)
    end
  | _ -> None




(* Does the type declaration have an attribute saying that the whole type represents an extension of another type? *)
let handle_type_extension ctx type_decl =
  let extension_of = List.fold_left handle_attribute None type_decl.ptype_attributes in
  match extension_of with
  | None -> None
  | Some extended_type ->
      Checks.check_type_extension_not_inside_of_injection_functor ctx type_decl.ptype_loc;
      let extension_name = unloc type_decl.ptype_name in
      let type_parameters = List.map fst type_decl.ptype_params in
      let match_type_parameters core_t =
        let loc = core_t.ptyp_loc in
        let type_descr = core_t.ptyp_desc in
        (* FIXME: should we check here that each type parameter corresponds to a registered extensible type? *)
        match type_descr with
        | Ptyp_var param_name ->
            param_name
        | _ ->
            Errors.raise_error loc "Type parameter does not fit excpected shape"
      in
      let type_param_assoc_list = List.map match_type_parameters type_parameters in
      let module_path =
        if Context.current_module_path_is_simple ctx then
          Context.current_simple_module_path_to_list ctx
        else
          Errors.raise_error
            type_decl.ptype_loc
            (Printf.sprintf "The type extension %s is defined inside of injection functor,
              which is unsupported" extension_name) in
      let extension_id = match Longident.unflatten (module_path @ [extension_name]) with
        | Some lid -> lid
        | None -> failwith "Creating longident failed" in
      Context.register_type_extension
        ctx
        extension_id
        extended_type
        type_param_assoc_list ;
      Some type_decl