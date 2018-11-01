open Jigsaw_ppx_shared.Ast_versioning.Parsetree


module AD = Jigsaw_ppx_shared.Analysis_data
module E = Jigsaw_ppx_shared.Errors
module U = Jigsaw_ppx_shared.Utils

type t = {
  analysis_data : AD.t;
  current_module : AD.module_path_element list ref; (* used as stack, most recent module at head *)
  current_file : string;
  current_library : string;
  files_in_current_library : string list;
  extensible_types_table : (AD.extensible_type_id, AD.extensible_type_info) Hashtbl.t;
  type_extension_table : (AD.extensible_type_id, AD.type_extension_info list)  Hashtbl.t;
  feature_decl_table : (AD.feature_id, AD.feature_decl_info list)  Hashtbl.t;
}



let create
  (library_name : string)
  (current_file : string)
  (analysis_data : AD.t)
  (wip_data : AD.data)
  (wip_files : string list) : t =
  {
    analysis_data;
    current_module =  ref [(AD.ModulePathPlain library_name)];
    current_file;
    current_library = library_name;
    files_in_current_library = wip_files;
    extensible_types_table = Jigsaw_ppx_shared.Utils.hashtbl_of_seq wip_data.extensible_types;
    type_extension_table = Jigsaw_ppx_shared.Utils.hashtbl_of_seq wip_data.extensions;
    feature_decl_table = Jigsaw_ppx_shared.Utils.hashtbl_of_seq wip_data.feature_decls;
  }

(* Tracking currently processed module *)

let get_current_file (ctx : t) = ctx.current_file

let get_current_library (ctx : t) = ctx.current_library

let push_plain_module (ctx : t) name =
  ctx.current_module := (AD.ModulePathPlain name) :: !(ctx.current_module)

let push_functor (ctx : t) name =
  ctx.current_module := (AD.ModulePathFunctor name) :: !(ctx.current_module)

let push_injection_functor (ctx : t) name elements =
  ctx.current_module := ((AD.ModulePathInjectionFunctor (name, elements)) :: !(ctx.current_module))

let pop_module (ctx : t) =
  ctx.current_module := List.tl !(ctx.current_module)

let current_module_path_is_simple ctx =
  List.for_all (function
    | AD.ModulePathPlain _ -> true
    | _ -> false) !(ctx.current_module)

let current_simple_module_path_to_list (ctx : t) =
  List.rev_map (function
    | AD.ModulePathPlain module_name -> module_name
    | _ -> E.raise_error_noloc "Using current_plain_module_path_to_list on non-plain module path"
  ) !(ctx.current_module)



(* registration functions (current libraries). Validation of added data must be done beforehand *)


let register_extensible_type (ctx : t) (name : AD.extensible_type_id) =
  E.info ("registered extensible type " ^ name);
  Hashtbl.add ctx.extensible_types_table name (get_current_file ctx)

let register_type_extension
  (ctx : t)
  (extension : AD.type_extension_id)
  (extended_type : AD.extensible_type_id)
  (type_parameters : string list) =
  E.info
    ( "adding extension " ^ AD.show_tei extension ^ " for type "
    ^ AD.show_tei extension
    ^ ", type_parameters: " ^  (String.concat "," type_parameters)) ;
  let extension_info : AD.type_extension_info = {
      te_extension_id = extension;
      te_defining_file = get_current_file ctx;
      te_type_parameters = type_parameters;
    } in
  match Hashtbl.find_opt ctx.type_extension_table extended_type  with
    | None ->
      Hashtbl.add ctx.type_extension_table extended_type
        [extension_info]
    | Some existing_extensions ->
        Hashtbl.add ctx.type_extension_table extended_type
          (extension_info :: existing_extensions)


let register_feature_function
    ctx
    (feature_name : AD.feature_id)
    (feature_function : AD.feature_function_id)
    (typ : core_type) =
  E.info (Printf.sprintf "Registering function %s of feature %s of type %s"
       feature_function feature_name (E.string_of_core_type typ) );
  let feature_function_info : AD.feature_function_info = {
    ff_function_type = typ;
    ff_defining_file = get_current_file ctx
  } in
  match Hashtbl.find_opt ctx.feature_decl_table feature_name  with
    | None ->
      Hashtbl.add ctx.feature_decl_table feature_name
        [(feature_function, feature_function_info)]
    | Some existing_functions ->
        Hashtbl.add ctx.feature_decl_table feature_name
          ((feature_function, feature_function_info) :: existing_functions)



(* query functions current and previous libraries *)

let has_extensible_type (ctx : t) (extensible_type_name : string) =
  Hashtbl.mem ctx.extensible_types_table extensible_type_name ||
  AD.has_extensible_type ctx.analysis_data extensible_type_name

let has_feature (ctx : t) (feature_name : AD.feature_id) : bool =
  Hashtbl.mem ctx.feature_decl_table feature_name ||
  AD.has_feature ctx.analysis_data feature_name


let get_extensions_of_type (ctx : t) (extensible_type_name : string) : AD.type_extension_info list =
  E.check (has_extensible_type ctx extensible_type_name);
  let ad_extensions =
    if AD.has_extensible_type ctx.analysis_data extensible_type_name then
      AD.get_extensions_of_type ctx.analysis_data extensible_type_name
    else
      [] in
  match Hashtbl.find_opt ctx.type_extension_table extensible_type_name with
    | Some exts -> exts @ ad_extensions
    | None -> ad_extensions


let get_extensions_by_unqualified_name
    (ctx : t)
    (extensible_type_name : AD.extensible_type_id)
    (extension_unqualified_name : string) : AD.type_extension_info list =
  E.check (has_extensible_type ctx extensible_type_name);
  let has_desired_name ext_info = (Longident.last ext_info.AD.te_extension_id = extension_unqualified_name) in
  let extensions = get_extensions_of_type ctx extensible_type_name in
  List.filter has_desired_name extensions



let export_data (ctx : t) : (AD.data * string list) =
  {
    extensible_types = U.seq_of_hashtbl ctx.extensible_types_table;
    extensions = U.seq_of_hashtbl ctx.type_extension_table;
    feature_decls = U.seq_of_hashtbl ctx.feature_decl_table;
  }, ctx.files_in_current_library
