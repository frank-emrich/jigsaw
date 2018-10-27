  let raise_info = Jigsaw_ppx_shared.Errors.raise_info
  type type_param_assoc_entry = string * string option [@@deriving show]

  (*let type_table = Hashtbl.create 10*)

  let extension_table = Hashtbl.create 10

  (*let register_extensible_type type_name ext_constructor ext_type_var =
    raise_info ("adding extensible type " ^ type_name) ;
    Hashtbl.add type_table type_name (ext_constructor, ext_type_var)*)

  let register_type_extension (extension_name : string)  (extended_type_name : string) (extension_path : Context.module_path_element list)  (type_parameters : string list) =
    raise_info
      ( "adding extension " ^ extension_name ^ " for type "
      ^ extended_type_name
      ^ ", current module path is " ^ String.concat "." (List.map Context.show_module_path_element extension_path)
      ^ ", type_parameters: " ^  (String.concat "," type_parameters)) ;
    match Hashtbl.find_opt extension_table extended_type_name with
    | None -> Hashtbl.add extension_table extended_type_name [(extension_name, extension_path, type_parameters)]
    | Some existing_extensions ->
        Hashtbl.add extension_table extended_type_name
          ((extension_name, extension_path, type_parameters) :: existing_extensions)

  let has_extensible_type (extensible_type_name : string) = Hashtbl.mem extension_table extensible_type_name

  (*let get_extensible_type type_name = Hashtbl.find_opt type_table type_name*)

  let get_type_extensions (extended_type_name : string)  = Hashtbl.find_opt extension_table extended_type_name