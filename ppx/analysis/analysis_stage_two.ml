(*open Migrate_parsetree*)
open Jigsaw_ppx_shared.Ast_versioning.Ast
open Jigsaw_ppx_shared.Ast_versioning.Parsetree

open Jigsaw_ppx_shared
module AD = Jigsaw_ppx_shared.Analysis_data
module AM = Jigsaw_ppx_shared.Ast_manipulation
module E = Jigsaw_ppx_shared.Errors
module U = Jigsaw_ppx_shared.Utils
module P = Jigsaw_ppx_shared.Persisting

let (->-) f g = fun x -> g (f x)




let unloc (loc : 'a Asttypes.loc) = loc.txt





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
  match (Types.handle_extensible_type ctx type_decl, Types.handle_type_extension ctx type_decl) with
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

let extension (_ctx : Context.t) m extension =
  let name = unloc (fst extension) in
  if List.mem name Jigsaw_ppx_shared.Names.Extensions.all then
    E.raise_error (fst extension).loc ("Found extensibility-related extension \"" ^ name ^ "\" in an unsupported location")
  else () ;
  Ast_mapper.default_mapper.extension m extension


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

let structure_item (ctx : Context.t) rec_mapper strct_item =
  let desc = strct_item.pstr_desc in
  let default () = Ast_mapper.default_mapper.structure_item rec_mapper strct_item in
  match desc with
    | Pstr_modtype mt_decl ->
      begin match Feature_declarations.handle_feature_decl ctx mt_decl with
        | Some mt_decl' ->
          {strct_item with pstr_desc = Pstr_modtype mt_decl'}
        | None -> default ()
      end
    | Pstr_value (recflag, value_bindings) ->
      let value_bindings' = List.map
        (fun vb ->
          match Feature_implementations.handle_feature_function_implementation ctx vb with
            | Some vb' -> vb'
            | None -> vb)
        value_bindings in
        {strct_item with pstr_desc = Pstr_value (recflag, value_bindings')}
    | Pstr_extension (ext, attr) ->
      begin match Injection_functors.handle_auto_inject_module ctx ext attr with
        | Some m_binding ->
           {strct_item with pstr_desc = Pstr_module (rec_mapper.module_binding rec_mapper m_binding)}
        | None -> default ()
      end
    | _ -> default ()
(* End mapper functions  *)


let build_context _config _cookies current_library =
  let cwd = Sys.getcwd () in
  let current_file = !Location.input_name in
  let include_dirs = !Clflags.include_dirs in

  let analysis_data_blob = P.load_analysis_data current_library include_dirs  in
  let analysis_data = AD.create analysis_data_blob in
  let (work_in_progress_data, work_in_progress_files) =
    P.load_work_in_progress_analysis_data_if_existing cwd current_library current_file in

  Context.create current_library current_file analysis_data work_in_progress_data work_in_progress_files


let save_context _config _cookies ctx =
  let (analysis_data_of_context, files_in_current_context) = Context.export_data ctx in
  let cwd = Sys.getcwd () in
  let current_library = Context.get_current_library ctx in
  P.persist_data cwd current_library analysis_data_of_context files_in_current_context

let dump_to_file ctx toplevel_result =
  match Sys.getenv_opt Names.Env.dump_dir with
    | Some dump_path ->
      if Sys.file_exists dump_path && Sys.is_directory dump_path then
        (let cur_file_name = Context.get_current_file ctx in
        let file_path = Filename.concat dump_path cur_file_name in
        let access = 0o660 (* octal *) in
        let flags : open_flag list = [Open_creat ; Open_wronly] in
        let out_ch = open_out_gen flags access file_path in
        let fmt = Format.formatter_of_out_channel out_ch in
        Errors.print_structure fmt toplevel_result;
        close_out out_ch;
        Errors.debug ("Wrote preprocessing result to file " ^ file_path))
      else
        Errors.raise_error_noloc ("Not a folder: " ^ dump_path)
    | None -> ()

(* The ast mappers catch and re-raise some  exceptions by default, meaning that we don't get usefull stack traces.
   We avoid this by wrapping every mapper function in a try-with *)
let exception_wrapper (mapper_fun : Ast_mapper.mapper -> 'a -> 'a) m v =
  try
    mapper_fun m v
  with Failure msg ->
    begin
      prerr_endline ("Error: " ^ msg);
      if Errors.is_print_stacktrace_enabled then
        prerr_endline  (Printexc.get_backtrace ());
        exit 1
    end


let actual_mapper ctx : Ast_mapper.mapper =
  {
    Ast_mapper.default_mapper with
      type_declaration = exception_wrapper (type_declaration ctx);
      attribute = exception_wrapper (attribute ctx);
      extension = exception_wrapper (extension ctx);
      module_binding = exception_wrapper (module_binding ctx);
      structure = exception_wrapper (structure ctx);
      structure_item = exception_wrapper (structure_item ctx);
  }

let toplevel_structure config cookies _ strct =

  E.debug "toplevel structure begin";

  let (marker_items, strct_without_marker) = List.partition AM.is_first_stage_marker_extension strct in
  let library_name = match marker_items with
    | [first_stage_marker_item] ->
      begin match AM.get_library_name_from_first_stage_marker_extension first_stage_marker_item with
        | Some name -> String.capitalize_ascii name
        | None -> E.raise_error_noloc "The first stage marker is ill-formed, it does not contain the library name"
      end
    | _ -> E.raise_error_noloc "There should be exactly one item in the top-level structure which has been marked by
          the first stage. This looks like a misconfiguration." in

  let ctx = build_context config cookies library_name in

  let m = actual_mapper ctx in

  let result = structure ctx m strct_without_marker in
  E.debug "toplevel structure end";
  save_context config cookies ctx;
  dump_to_file ctx result;
  result





let mapper config cookies =
  {Ast_mapper.default_mapper with
    structure = toplevel_structure config cookies }

