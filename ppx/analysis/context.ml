open Jigsaw_ppx_shared.Ast_versioning.Parsetree


open Jigsaw_ppx_shared
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
  feature_impl_table : (AD.feature_id, (AD.feature_function_id * AD.feature_function_impl_info ) list)  Hashtbl.t;
}



let create
  (library_name : string)
  (current_file : string)
  (analysis_data : AD.t)
  (wip_data : AD.data)
  (wip_files : string list) : t =
  let current_file_module = Names.filename_to_module_name current_file in
  {
    analysis_data;
    current_module =  ref [ (AD.ModulePathPlain current_file_module) ; (AD.ModulePathLibrary library_name)];
    current_file;
    current_library = library_name;
    files_in_current_library = wip_files;
    extensible_types_table = Jigsaw_ppx_shared.Utils.hashtbl_of_seq wip_data.extensible_types;
    type_extension_table = Jigsaw_ppx_shared.Utils.hashtbl_of_seq wip_data.extensions;
    feature_decl_table = Jigsaw_ppx_shared.Utils.hashtbl_of_seq wip_data.feature_decls;
    feature_impl_table = Jigsaw_ppx_shared.Utils.hashtbl_of_seq wip_data.feature_impls;
  }

(* Tracking currently processed module *)

let get_current_file (ctx : t) = ctx.current_file

let get_current_library (ctx : t) = ctx.current_library

let get_current_module_path (ctx : t) =
  List.rev !(ctx.current_module)

let push_plain_module (ctx : t) name =
  ctx.current_module := (AD.ModulePathPlain name) :: !(ctx.current_module)

let push_functor (ctx : t) name =
  ctx.current_module := (AD.ModulePathFunctor name) :: !(ctx.current_module)

let push_injection_functor (ctx : t) name elements =
  let elements_stringified = List.map Analysis_data.show_injection_arg_element elements in
  Errors.debug ("Entering Injection Functor named " ^ name ^ ", elements: " ^ String.concat "," elements_stringified);
  ctx.current_module := ((AD.ModulePathInjectionFunctor (name, elements)) :: !(ctx.current_module))

let pop_module (ctx : t) =
  let modname = match List.hd !(ctx.current_module) with
    | Analysis_data.ModulePathPlain name
    | ModulePathFunctor name
    | ModulePathLibrary name
    | ModulePathInjectionFunctor (name, _) -> name   in
  Errors.debug ("Leaving module/injection functor " ^ modname);
  ctx.current_module := List.tl !(ctx.current_module)

let current_module_path_is_simple ctx =
  List.for_all (function
    | AD.ModulePathPlain _
    | ModulePathLibrary _-> true
    | _ -> false) !(ctx.current_module)

let current_simple_module_path_to_list (ctx : t) =
  List.rev_map (function
    | AD.ModulePathPlain name
    | ModulePathLibrary name -> name
    | _ -> E.raise_error_noloc "Using current_plain_module_path_to_list on non-plain module path"
  ) !(ctx.current_module)

(* like current_simple_module_path_to_list, but only works until it sees a functor *)
let current_simple_module_path_prefix_list (ctx : t) =
  List.fold_left
    (fun prefix m ->
      match m with
        | AD.ModulePathPlain name
        | ModulePathLibrary name -> name :: prefix
        | _ -> prefix)
    []
    !(ctx.current_module)


let inside_of_injection_functor ctx =
  not (current_module_path_is_simple ctx)

let get_active_injection_functor_data ctx =
  E.check (inside_of_injection_functor ctx);
  let injection_functor = List.find
    (function
      | AD.ModulePathInjectionFunctor _ -> true
      | _ -> false)
    !(ctx.current_module) in
  match injection_functor with
    | AD.ModulePathInjectionFunctor (name, info) -> (name, info)
    | _ -> assert false


let get_active_injection_functor_types ctx =
  let _name, data = get_active_injection_functor_data ctx in
  List.fold_left
    (fun acc element ->
      match element with
        | AD.InjectionType typ -> typ :: acc
        | _ -> acc)
    []
    data

(* Remove the prefix from a Longident that corresponds to the module we are currently in *)
let contextualize_lid ctx lid =
  let rec contextualize current_module_parts lid_parts =
    match current_module_parts, lid_parts with
      | _, []
      | [], _ -> lid_parts
      | (name :: cmps), (id :: ids) ->
        if name = id then
          contextualize cmps ids
        else
          lid_parts in
  match Longident.unflatten (contextualize (current_simple_module_path_prefix_list ctx) (Longident.flatten lid)) with
    | Some res_lid -> res_lid
    | None -> failwith "Internal Errors: contextualize_lid failed to produce valid Longident.t"

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
    (typ : core_type)
    (typ_split : AD.feature_function_type_part list) =
  E.info (Printf.sprintf "Registering function %s of feature %s of type %s"
       feature_function feature_name (E.string_of_core_type typ) );
  let feature_function_info : AD.feature_function_info = {
    ff_function_type = typ;
    ff_defining_file = get_current_file ctx;
    ff_function_type_split = typ_split
  } in
  match Hashtbl.find_opt ctx.feature_decl_table feature_name  with
    | None ->
      Hashtbl.add ctx.feature_decl_table feature_name
        [(feature_function, feature_function_info)]
    | Some existing_functions ->
        Hashtbl.add ctx.feature_decl_table feature_name
          ((feature_function, feature_function_info) :: existing_functions)


let register_feature_function_implementation
    ctx
    (feature_name : AD.feature_id)
    (feature_function : AD.feature_function_id)
    (injections: AD.feature_function_id list )
    (type_correspondences : AD.feature_function_impl_type_part list ) =
  let info : Analysis_data.feature_function_impl_info = {
    ffi_location = get_current_module_path ctx;
    ffi_defining_file = get_current_file ctx;
    ffi_injections = injections;
    ffi_type = type_correspondences;
  } in
  E.info
    (Printf.sprintf "Registered implementation of function %s of feature %s"
     feature_function feature_name );
  let entry = (feature_function, info) in
  let entries = match Hashtbl.find_opt ctx.feature_impl_table feature_name with
    | Some impls -> entry :: impls
    | None -> [entry] in
  Hashtbl.add ctx.feature_impl_table feature_name entries

(* query functions current and previous libraries *)

let has_extensible_type (ctx : t) (extensible_type_name : string) =
  Hashtbl.mem ctx.extensible_types_table extensible_type_name ||
  AD.has_extensible_type ctx.analysis_data extensible_type_name

let has_feature (ctx : t) (feature_name : AD.feature_id) : bool =
  Hashtbl.mem ctx.feature_decl_table feature_name ||
  AD.has_feature ctx.analysis_data feature_name

let get_extensible_types (ctx : t) : Analysis_data.feature_id list =
  let ad_types = Analysis_data.get_extensible_types ctx.analysis_data in
  Hashtbl.fold
    (fun t _ types ->
      if Analysis_data.has_extensible_type ctx.analysis_data t then
        types
      else
        t :: types)
      ctx.extensible_types_table
      ad_types



let get_feature_functions_of_feature (ctx : t) (feature_name : Analysis_data.feature_id) =
  if Analysis_data.has_feature ctx.analysis_data feature_name then
    Analysis_data.get_feature_functions_of_feature ctx.analysis_data feature_name
  else
    (Errors.check (has_feature ctx feature_name);
    Hashtbl.find ctx.feature_decl_table feature_name)



let get_feature_function_of_feature (ctx : t) (feature_name : Analysis_data.feature_id) (feature_function_name : Analysis_data.feature_function_id) =
  let fns = get_feature_functions_of_feature ctx feature_name in
  List.assoc_opt feature_function_name fns

let has_feature_function (ctx : t) (feature_name : Analysis_data.feature_id) (feature_function_name : Analysis_data.feature_function_id) =
  if Analysis_data.has_feature ctx.analysis_data feature_name then
    Analysis_data.has_feature_function ctx.analysis_data feature_name feature_function_name
  else
    match get_feature_function_of_feature ctx feature_name feature_function_name with
      | Some _ -> true
      | None -> false

let get_feature_function_implementations
    (ctx : t)
    (feature_name : Analysis_data.feature_id)
    (feature_function_name : Analysis_data.feature_function_id) =
  let ad_impls =
    if AD.has_feature ctx.analysis_data feature_name then
      AD.get_feature_function_implementations ctx.analysis_data feature_name feature_function_name
    else
      [] in
  match Hashtbl.find_opt ctx.feature_impl_table feature_name with
    | Some impls ->
      let impls_for_function = List.filter (fun (ffname, _) -> ffname = feature_function_name ) impls in
      impls_for_function @ ad_impls
    | None -> ad_impls


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
    feature_impls = U.seq_of_hashtbl ctx.feature_impl_table;
  }, ctx.files_in_current_library
