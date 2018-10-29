(*open Utils*)

type extensible_type_id = string [@@deriving show, yojson]
type type_extension_id = Longident.t

let pp_type_extension_id fmt tei = Format.pp_print_string fmt (String.concat "." (Longident.flatten tei))
let type_extension_id_to_yojson tei : Yojson.Safe.json =
   `Variant ("Longident", Some (`String (String.concat "." (Longident.flatten tei)) ))

let type_extension_id_of_yojson : Yojson.Safe.json -> (type_extension_id, string) Result.result = function
  | `Variant ("Longident", Some (`String tei_string )) ->
    Result.Ok (Longident.parse tei_string)
  | _ -> Result.Error "Error unmarshalling Longident"



let show_tei lid = String.concat "." (Longident.flatten lid)

type injection_arg_element =
  | InjectionType of string
  | InjectionLift of string * extensible_type_id * type_extension_id (* function name, overall, extension *)
  | InjectionUnlift of string * extensible_type_id * type_extension_id (* function name, overall, extension *)
  | InjectionFeatureFunction of string * string * string (* function name, feature name, feature function name *)
    [@@deriving show, yojson]


type module_path_element =
  | ModulePathPlain of string
  | ModulePathFunctor of string
  | ModulePathInjectionFunctor of string * injection_arg_element list
    [@@deriving show, yojson]

type extensible_type_info = string (* defining file *)
  [@@deriving show, yojson]

type extensible_type_seq = (string * extensible_type_info) list (* name -> extensible_type_info *)
  [@@deriving show, yojson]


type type_extension_info = {
  te_extension_id : type_extension_id;
  te_defining_file : string;
  te_type_parameters : string list
}(* extension id, defining file,  type variables *)
  [@@deriving show, yojson]

type type_extension_seq = (extensible_type_id * type_extension_info list) list (* extended name -> [type_extension_info] *)
  [@@deriving show, yojson]




type type_param_assoc_entry = string * string option [@@deriving show]

type t = {
  types : extensible_type_info Utils.StringMap.t;
  extensions : (type_extension_info list) Utils.StringMap.t

}

type data = {
  extensible_types : extensible_type_seq;
  extensions : type_extension_seq
} [@@deriving show]

let create (id : data) : t = {
  types = Utils.string_map_of_seq  id.extensible_types;
  extensions = Utils.string_map_of_seq  id.extensions
}


let has_extensible_type (ad : t) (extensible_type_name : extensible_type_id) : bool =
  Utils.StringMap.mem extensible_type_name ad.types

(*let get_extensible_type type_name = Hashtbl.find_opt type_table type_name*)

let get_extensions_of_type (ad : t) (extensible_type_name : extensible_type_id) : type_extension_info list =
  Errors.check (has_extensible_type ad extensible_type_name);
  match Utils.StringMap.find_opt extensible_type_name ad.extensions with
    | None -> []
    | Some exts -> exts


let get_extensions_by_unqualified_name
    (ad : t)
    (extensible_type_name : extensible_type_id)
    (extension_unqualified_name : string) : type_extension_info list =
  Errors.check (has_extensible_type ad extensible_type_name);
  let has_desired_name ext_info = Longident.last ext_info.te_extension_id = extension_unqualified_name in
  let extensions = get_extensions_of_type ad extensible_type_name in
  List.filter has_desired_name extensions