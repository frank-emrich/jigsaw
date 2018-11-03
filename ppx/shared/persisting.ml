(*open Ast_versioning.Parsetree*)


type persisted_data = {
   per_library: string;
   per_files : string list;
   per_extensible_types_map : Analysis_data.extensible_type_seq;
   per_type_extensions_map : Analysis_data.type_extension_seq;
   per_feature_decls : Analysis_data.feature_decl_seq;
   per_feature_impls : Analysis_data.feature_function_impl_seq
} [@@deriving show,yojson]



let load_from_library_file path =
  let json = Yojson.Safe.from_file path in
  let data_result  = persisted_data_of_yojson json in
  Errors.debug ("reading file" ^ path);
  match data_result with
    | Error msg ->
      Errors.raise_error_noloc ("Error while loading analysis data from " ^ path ^ " : " ^ msg)
    | Ok (data : persisted_data) ->
      Errors.debug ("Successfully loaded file " ^ path);
      Errors.debug (show_persisted_data data);
      data

let get_file_name_for_library top_library =
  let uc_top_library = String.uncapitalize_ascii top_library in
  uc_top_library ^ "-jigext.json"

let is_extension_json_file file_name =
  let regexp = Str.regexp "^.+-jigext\\.json$" in
  Str.string_match regexp file_name 0

let load_from_include_folder is_pwd current_library folder_path : (persisted_data * string) list =
  (*Errors.debug ("processing folder " ^ folder_path);*)
  if ( Sys.file_exists folder_path && Sys.is_directory folder_path) then
    let folder_contents = Sys.readdir folder_path in
    Array.fold_left (fun acc name ->
      let cur_path = Filename.concat  folder_path name in
      if (    not (Sys.is_directory cur_path)
           && is_extension_json_file name
              (* When reading from the current workdir, skip the file for the currently processed library *)
           && not (is_pwd && name = get_file_name_for_library current_library )) then
        (load_from_library_file cur_path, cur_path) :: acc
      else
        acc
    ) [] folder_contents
  else
    []

let empty_analysis_data =
  {
    Analysis_data.extensible_types = [];
    Analysis_data.extensions = [];
    Analysis_data.feature_decls = [];
    Analysis_data.feature_impls = [];
  }

(* public *)

let load_analysis_data current_top_library folders : Analysis_data.data =
  let result_nopwd = List.concat (List.map (load_from_include_folder false current_top_library) folders ) in
  let pwd_path = Sys.getcwd () in
  let result = load_from_include_folder true current_top_library pwd_path @ result_nopwd in

  List.fold_left (fun acc_data (cur_persisted_data, cur_persited_data_file) ->
    if cur_persisted_data.per_library = current_top_library then
      Errors.raise_error_noloc (Printf.sprintf "The extensbility information currently being loaded from file %s is for library %s,
         which is the same library we are currently pre-processing for" cur_persited_data_file current_top_library)
    else
      {
        Analysis_data.extensible_types =
          acc_data.Analysis_data.extensible_types @ cur_persisted_data.per_extensible_types_map;
        Analysis_data.extensions =
          acc_data.Analysis_data.extensions @ cur_persisted_data.per_type_extensions_map;
        Analysis_data.feature_decls =
          acc_data.Analysis_data.feature_decls  @ cur_persisted_data.per_feature_decls;
        Analysis_data.feature_impls =
          acc_data.Analysis_data.feature_impls  @ cur_persisted_data.per_feature_impls;
      }
  ) empty_analysis_data result

let extensible_type_not_from file ( (_, ext_type_info) : string * Analysis_data.extensible_type_info) =
  not (ext_type_info = file)

let type_extension_not_from file  (extension : Analysis_data.type_extension_info) : bool =
  not (extension.Analysis_data.te_defining_file = file)

let feature_function_not_from file (_, (extension : Analysis_data.feature_function_info)) : bool =
  not (extension.Analysis_data.ff_defining_file = file)


let feature_function_impl_not_from file (_, (extension : Analysis_data.feature_function_impl_info)) : bool =
  not (extension.Analysis_data.ffi_defining_file = file)


let remove_nested_info (filter_inner_seq : 'b -> bool)  (seq : ('a * 'b list) list as 'seq) : 'seq =
  List.fold_right
    (fun outer_item (acc : 'seq) ->
      let key = fst outer_item in
      let inner_seq = snd outer_item in
      let filtered_inner_seq = List.filter filter_inner_seq inner_seq in
      match filtered_inner_seq with
        | [] -> acc
        | _ -> (key, filtered_inner_seq) :: acc )
    seq []


(*let remove_features_defined_in file (seq : Analysis_data.feature_decl_seq) =
  let seq' = List.map (type_extension_not_from file) seq in
  List.filter ( fun (_, extensions) -> not (extensions = [])) seq'*)

let load_work_in_progress_analysis_data_if_existing base_folder top_library_name current_file_name : (Analysis_data.data * string list) =
  (*Errors.debug ("base_folder: " ^ base_folder);*)
  let file_to_read = get_file_name_for_library top_library_name in
  let load_path = Filename.concat base_folder file_to_read in
  let wip_data =
    if Sys.file_exists load_path then
      begin
      let pdata = load_from_library_file load_path in
      let previous_files = pdata.per_files in
      let updated_files = current_file_name :: pdata.per_files in
      let existing_files_contain_current = List.mem  current_file_name previous_files in

      let first_existing_file_is_current =
        match previous_files with
          | f :: _ when f = current_file_name -> true
          | _ -> false in
      Errors.debug ("previous files: " ^ String.concat ", " previous_files);
      match existing_files_contain_current, first_existing_file_is_current with
        | (true, false) ->
          Errors.raise_error_noloc "It seems like the dependency graph between modules changed. Please perform a clean rebuild"
        | (true, true) ->
          (* We filter out data collected in a previous run over the file we are currently pre-processing *)
          (({
            extensible_types =
              List.filter (extensible_type_not_from current_file_name) pdata.per_extensible_types_map;
            extensions =
              remove_nested_info (type_extension_not_from current_file_name) pdata.per_type_extensions_map ;
            feature_decls =
              remove_nested_info (feature_function_not_from current_file_name) pdata.per_feature_decls;
            feature_impls =
              remove_nested_info (feature_function_impl_not_from current_file_name) pdata.per_feature_impls;
          }, previous_files) : Analysis_data.data * string list )
        | (false, _) ->
          (({
            extensible_types = pdata.per_extensible_types_map;
            extensions = pdata.per_type_extensions_map;
            feature_decls = pdata.per_feature_decls;
            feature_impls = pdata.per_feature_impls;
          }, updated_files) : Analysis_data.data * string list )
        end
      else
        ((empty_analysis_data, [current_file_name]) : Analysis_data.data * string list ) in
  Errors.debug ("Loaded wip data: " ^ Analysis_data.show_data (fst wip_data));
  wip_data


let persist_data
    (base_folder : string)
    (top_library_name : string)
    (context_analysis_data : Analysis_data.data)
    (context_files : string list) =
  let file_to_write = get_file_name_for_library top_library_name in
  let path_to_write = Filename.concat base_folder file_to_write in
  let pdata : persisted_data = {
      per_library = top_library_name;
      per_files = context_files;
      per_extensible_types_map = context_analysis_data.extensible_types;
      per_type_extensions_map = context_analysis_data.extensions;
      per_feature_decls = context_analysis_data.feature_decls;
      per_feature_impls = context_analysis_data.feature_impls;
    } in
  let json = persisted_data_to_yojson pdata in
  Errors.debug ("Persisting analysis data to " ^ path_to_write);
  Yojson.Safe.to_file path_to_write json

