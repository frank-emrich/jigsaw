open Ast_versioning.Parsetree



type persistable_core_type = core_type


type persisted_data = {
   per_library: string;
   per_files : string list;
   per_extensible_types_map : Analysis_data.extensible_type_seq;
   per_type_extensions_map : Analysis_data.type_extension_seq
} [@@deriving yojson]



let load_from_library_file path =
  let json = Yojson.Safe.from_file path in
  let data_result  = persisted_data_of_yojson json in
  match data_result with
    | Error msg ->
      Errors.raise_error_noloc ("Error while loading analysis data from " ^ path ^ " : " ^ msg)
    | Ok (data : persisted_data) ->
      Errors.debug ("Successfully loaded file " ^ path);
      data

let get_file_name_for_library top_library =
  top_library ^ ".jigext"

let is_extension_json_file file_name =
  let regexp = Str.regexp "^.+\\.jigext$" in
  Str.string_match regexp file_name 0

let load_from_include_folder folder_path : (persisted_data * string) list =
  if ( Sys.file_exists folder_path && Sys.is_directory folder_path) then
    let folder_contents = Sys.readdir folder_path in
    Array.fold_left (fun acc name ->
      let cur_path = Filename.concat  folder_path name in
      if not (Sys.is_directory cur_path) && is_extension_json_file name then
        (load_from_library_file cur_path, cur_path) :: acc
      else
        acc
    ) [] folder_contents
  else
    failwith ( folder_path ^ " is not a folder")

(* public *)

let load_analysis_data current_top_library folders : Analysis_data.data =
  let resuls= List.concat (List.map load_from_include_folder folders) in
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
      }
  ) {Analysis_data.extensible_types = []; Analysis_data.extensions = []} resuls

let load_work_in_progress_analysis_data_if_existing base_folder top_library_name current_file_name =
  let file_to_read = get_file_name_for_library top_library_name in
  let load_path = Filename.concat base_folder file_to_read in
  if Sys.file_exists load_path then
    begin
    let pdata = load_from_library_file load_path in
    let previous_files = pdata.per_files in
    let updates_files = current_file_name :: pdata.per_files in
    if List.mem  current_file_name previous_files then
      Errors.raise_error_noloc (Printf.sprintf "The file %s already contains analysis data for the  file that we are
        currently processing (%s). This indicates not cleaning up after the last compilation" load_path current_file_name)
    else
      (({
        extensible_types = pdata.per_extensible_types_map;
        extensions = pdata.per_type_extensions_map;
      }, updates_files) : Analysis_data.data * string list )
    end
  else
    (({
      extensible_types = [];
      extensions = [];
    }, [current_file_name]) : Analysis_data.data * string list )


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
      per_type_extensions_map = context_analysis_data.extensions
    } in
  let json = persisted_data_to_yojson pdata in
  Yojson.Safe.to_file path_to_write json

