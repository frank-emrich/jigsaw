open Migrate_parsetree
open Jigsaw_ppx_shared.Ast_versioning.Ast
open Jigsaw_ppx_shared.Ast_versioning.Parsetree

module AD = Jigsaw_ppx_shared.Analysis_data
module AM = Jigsaw_ppx_shared.Ast_manipulation
module E = Jigsaw_ppx_shared.Errors
module U = Jigsaw_ppx_shared.Utils
module P = Jigsaw_ppx_shared.Persisting

let (->-) f g = fun x -> g (f x)



let extract_type_var core_type =
  match core_type.ptyp_desc with
  | Ptyp_var type_var_name -> type_var_name
  | _ -> failwith "Attempting to extract type var from something that isn't one"

let unloc (loc : 'a Asttypes.loc) = loc.txt



let handle_constructor ext_points constr =
  let attrs = constr.pcd_attributes in
  let is_extension_attr attr = unloc (fst attr) = Jigsaw_ppx_shared.Names.Attributes.extension_point in
  match List.find_opt is_extension_attr attrs with
  | Some _attr ->
      let constr_name = unloc constr.pcd_name in
      let constr_res_type_opt = constr.pcd_res in
      (* used in GADTs *)
      let constr_args = constr.pcd_args in
      let constr_loc = constr.pcd_loc in
      (constr_name, constr_loc, constr_args, constr_res_type_opt) :: ext_points
  | None -> ext_points

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
(*let handle_extensible_type td_record =
  match td_record.ptype_kind with
  | Ptype_variant constrs -> (
      let type_param_names = List.map (fun (q, _) -> extract_type_var q) td_record.ptype_params in
      let declared_type_name = unloc td_record.ptype_name in
      let name_loc = td_record.ptype_name.loc in
      let extension_points = List.fold_left handle_constructor [] constrs in
      match extension_points with
      | [] -> None
      | [(ext_constr_name, ext_constr_loc, ext_constr_args, ext_constr_res_type)] -> (
        match (ext_constr_res_type, ext_constr_args) with
        | Some ct, _ ->
            raise_error ct.ptyp_loc "Extension point must not have GADT type"
        | _, Pcstr_record _ ->
            raise_error ext_constr_loc "Extension point must not have record type" ;
        | _, Pcstr_tuple [] | _, Pcstr_tuple (_ :: _ :: _) ->
            raise_error ext_constr_loc "Extension point constructor must have single argument"
        | _, Pcstr_tuple [{ptyp_desc= Ptyp_var ext_constr_type_var; ptyp_loc= _; ptyp_attributes= _}]
          when List.mem ext_constr_type_var type_param_names ->
            register_extensible_type declared_type_name ext_constr_name ext_constr_type_var ;
            Some td_record
        | _ ->
            raise_error ext_constr_loc
              "Extension point constructor must accept single type argument, which is a type variable that the \
               declared type is polymorpic over"  )
      | _ ->
          raise_error name_loc "More than one constructor is annotated to be an extension point"  )
  | _ -> None*)



(* Does the type declaration have an attribute saying that the whole type represents an extension of another type? *)
let handle_type_extension ctx type_decl =
  let extension_of = List.fold_left handle_attribute None type_decl.ptype_attributes in
  match extension_of with
  | None -> None
  | Some extended_type ->
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
            E.raise_error loc "Type parameter does not fit excpected shape"
      in
      let type_param_assoc_list = List.map match_type_parameters type_parameters in
      let module_path =
        if Context.current_module_path_is_simple ctx then
          Context.current_simple_module_path_to_list ctx
        else
          E.raise_error
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


let remove_injection_attrs_from_functor module_expr =
  let desc = module_expr.pmod_desc in
  match desc with
    | Pmod_functor (name_loced, (Some module_type), result_m_expr) ->
      let functor_arg_attributes = module_type.pmty_attributes in
      let injection_attr_name = Jigsaw_ppx_shared.Names.Attributes.inject in
      let is_injection_attr = Jigsaw_ppx_shared.Ast_manipulation.attribute_has_name injection_attr_name in
      let non_inject_attributes = List.filter (is_injection_attr ->- not) functor_arg_attributes in
      let module_type_without_injection_attrs = {module_type with pmty_attributes = non_inject_attributes} in
      let functor_without_injection_attrs = Pmod_functor (name_loced, (Some module_type_without_injection_attrs), result_m_expr) in
      {module_expr with pmod_desc = functor_without_injection_attrs}
    | _ -> failwith "Expected funtor with module type"


let add_attributes_to_functor module_expr attributes =
  let desc = module_expr.pmod_desc in
  match desc with
    | Pmod_functor (name_loced, (Some module_type), result_m_expr) ->
      let existing_attrs = module_type.pmty_attributes in
      let extended_attrs = existing_attrs @ attributes in
      let module_type_update = {module_type with pmty_attributes = extended_attrs} in
      let functor_updated = Pmod_functor (name_loced, (Some module_type_update), result_m_expr) in
      {module_expr with pmod_desc = functor_updated}
    | _ -> failwith "Expected funtor with module type"

(* Mapper functions *)

(*let module_expr m m_expr =
  let handle_non_injection_functor () =
    Context.push_functor mod_name;
    let result = Ast_mapper.default_mapper.module_expr m m_expr in
    Context.pop_module;

  let descr = m_expr.pmod_desc in
  match descr with

    | _ -> Ast_mapper.default_mapper.module_expr m m_expr*)

let type_declaration (ctx : Context.t) m type_decl =
  match (None, handle_type_extension ctx type_decl) with
  | None, None -> Ast_mapper.default_mapper.type_declaration m type_decl
  | Some _, Some _ ->
      let loc = type_decl.ptype_name.loc in
      E.raise_error loc "Type cannot be extensible and an extension at the same time"
  | Some res, _ | _, Some res -> res

let attribute (_ctx : Context.t) m attr =
  let name = unloc (fst attr) in
  if List.mem name Jigsaw_ppx_shared.Names.Attributes.all then
    E.raise_error (fst attr).loc ("Found extensibility-related attribute \"" ^ name ^ "\" in an unsupported location")
  else () ;
  Ast_mapper.default_mapper.attribute m attr

let module_binding (ctx : Context.t) mapper mod_binding =
  let mod_name = unloc mod_binding.pmb_name in
  let mod_expr = mod_binding.pmb_expr in
  let mod_expr_desc = mod_expr.pmod_desc in
  match mod_expr_desc with
    | Pmod_structure _ ->
      Context.push_plain_module ctx mod_name;
      let result = Ast_mapper.default_mapper.module_binding mapper mod_binding in
      Context.pop_module ctx;
      result
    | Pmod_functor (name_loced, (Some module_type), _) ->
      (* Look for functor annotated with [@inject] *)
      let functor_arg_attributes = module_type.pmty_attributes in
      let injection_attr_name = Jigsaw_ppx_shared.Names.Attributes.inject in
      let is_injection_attr = Jigsaw_ppx_shared.Ast_manipulation.attribute_has_name injection_attr_name in
      let inject_attributes = List.filter is_injection_attr functor_arg_attributes in
      begin match inject_attributes with
        | [] ->
          Context.push_functor ctx mod_name;
          let result = Ast_mapper.default_mapper.module_binding mapper mod_binding in
          Context.pop_module ctx;
          result
        | [inject_attr] ->
          if Jigsaw_ppx_shared.Ast_manipulation.attribute_has_empty_payload inject_attr then
            (let argument_elements = Injection_functors.handle_injection_functor ctx module_type in
            (* We remove the injection attributes so the default mapper does not call our fail-on-extensibility-attributes function on them *)
            let mod_binding_no_inject_attrs = {mod_binding with pmb_expr = remove_injection_attrs_from_functor mod_expr } in
            Context.push_injection_functor ctx mod_name argument_elements;
            let result = Ast_mapper.default_mapper.module_binding mapper mod_binding_no_inject_attrs in
            let result_with_inj_attrs = {result with pmb_expr = add_attributes_to_functor result.pmb_expr [inject_attr] } in
            Context.pop_module ctx;
            result_with_inj_attrs)
          else
            E.raise_error (fst inject_attr).loc ("Attribute " ^ injection_attr_name ^ " must not have playload ")
        | _ -> E.raise_error name_loced.loc ("Encountered multiple attributes named " ^ injection_attr_name)
      end
    | _ ->
      (* We may die here on certain kinds of module bindings that we don't want *)
      Ast_mapper.default_mapper.module_binding mapper mod_binding


let structure (_ctx : Context.t) = Ast_mapper.default_mapper.structure

(* End mapper functions  *)


let get_library_name cookies =
  match Driver.get_cookie cookies "library-name" Jigsaw_ppx_shared.Ast_versioning.ocaml_version with
    | None -> E.raise_error_noloc "No cookie named library-name was passed"
    | Some lib_expr ->
      let lid = Jigsaw_ppx_shared.Ast_manipulation.extract_single_ident_expr lib_expr in
      String.concat "." (Longident.flatten lid)

let build_context _config cookies =
  let cwd = Sys.getcwd () in
  let current_library = get_library_name cookies in
  let current_file = !Location.input_name in
  let include_dirs = !Clflags.include_dirs in

  let analysis_data_blob = P.load_analysis_data current_library include_dirs  in
  let analysis_data = AD.create analysis_data_blob in
  let (work_in_progress_data, work_in_progress_files) =
    P.load_work_in_progress_analysis_data_if_existing cwd current_library current_file in

  Context.create current_library analysis_data work_in_progress_data work_in_progress_files


let save_context _config cookies ctx =
  let (analysis_data_of_context, files_in_current_context) = Context.export_data ctx in
  let cwd = Sys.getcwd () in
  let current_library = get_library_name cookies in
  P.persist_data cwd current_library analysis_data_of_context files_in_current_context



let actual_mapper ctx : Ast_mapper.mapper =
  {
    Ast_mapper.default_mapper with
    type_declaration = type_declaration ctx;
    attribute = attribute ctx;
    module_binding = module_binding ctx;
    structure = structure ctx;
  }

let toplevel_structure config cookies _ strct =
  E.debug "toplevel structure begin";
  let ctx = build_context config cookies in

  let strct_without_first_stage_marker = [] (*List.filter (AM.is_first_stage_marker_extension ->- not) strct*)  in
  let m = actual_mapper ctx in

  let result = structure ctx m strct_without_first_stage_marker in
  E.debug "toplevel structure end";
  save_context config cookies ctx;
  result





let mapper config cookies =
  {Ast_mapper.default_mapper with
    structure = toplevel_structure config cookies }

