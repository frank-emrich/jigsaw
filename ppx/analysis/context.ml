
module AD = Jigsaw_ppx_shared.Analysis_data
module E = Jigsaw_ppx_shared.Errors
module U = Jigsaw_ppx_shared.Utils

type t = {
  analysis_data : AD.t;
  current_module : AD.module_path_element list ref; (* used as stack, most recent module at head *)
  files_in_current_library : string list;
  extensible_types_table : (AD.extensible_type_id, AD.extensible_type_info) Hashtbl.t;
  type_extension_table : (AD.extensible_type_id, AD.type_extension_info list)  Hashtbl.t;
}



let create
  (library_name : string)
  (analysis_data : AD.t)
  (wip_data : AD.data)
  (wip_files : string list) : t =
  {
    analysis_data;
    current_module =  ref [(AD.ModulePathPlain library_name)];
    files_in_current_library = wip_files;
    extensible_types_table = Jigsaw_ppx_shared.Utils.hashtbl_of_seq wip_data.extensible_types;
    type_extension_table = Jigsaw_ppx_shared.Utils.hashtbl_of_seq wip_data.extensions;

  }

(* Tracking currently processed module *)

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
  Hashtbl.add ctx.extensible_types_table name ()

let register_type_extension
  (ctx : t)
  (extension : AD.type_extension_id)
  (extended_type : AD.extensible_type_id)
  (type_parameters : string list) =
  E.info
    ( "adding extension " ^ AD.show_tei extension ^ " for type "
    ^ AD.show_tei extension
    ^ ", type_parameters: " ^  (String.concat "," type_parameters)) ;
  match Hashtbl.find_opt ctx.type_extension_table extended_type  with
    | None ->
      Hashtbl.add ctx.type_extension_table extended_type
        [(extension, type_parameters)]
    | Some existing_extensions ->
        Hashtbl.add ctx.type_extension_table extended_type
          ((extension, type_parameters) :: existing_extensions)



(* query functions current and previous libraries *)

let has_extensible_type (ctx : t) (extensible_type_name : string) =
  Hashtbl.mem ctx.extensible_types_table extensible_type_name ||
  AD.has_extensible_type ctx.analysis_data extensible_type_name



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
  let has_desired_name ext_info = Longident.last (fst ext_info) = extension_unqualified_name in
  let extensions = get_extensions_of_type ctx extensible_type_name in
  List.filter has_desired_name extensions



let export_data (ctx : t) : (AD.data * string list) =
  {
    extensible_types = U.seq_of_hashtbl ctx.extensible_types_table;
    extensions = U.seq_of_hashtbl ctx.type_extension_table;
  }, ctx.files_in_current_library
