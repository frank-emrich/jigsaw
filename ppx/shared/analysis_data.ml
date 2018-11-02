(*open Utils*)
open Ast_versioning.Parsetree

type extensible_type_id = string [@@deriving show, yojson]
type type_extension_id = Longident.t

type feature_id = string [@@deriving show, yojson]
type feature_function_id = string [@@deriving show, yojson]

let pp_type_extension_id fmt tei = Format.pp_print_string fmt (String.concat "." (Longident.flatten tei))
let type_extension_id_to_yojson tei : Yojson.Safe.json =
   `Variant ("Longident", Some (`String (String.concat "." (Longident.flatten tei)) ))

let type_extension_id_of_yojson : Yojson.Safe.json -> (type_extension_id, string) Result.result = function
  | `Variant ("Longident", Some (`String tei_string )) ->
    Result.Ok (Longident.parse tei_string)
  | _ -> Result.Error "Error unmarshalling Longident"

let pp_core_type = Errors.print_core_type
let core_type_to_yojson = Yojson_implementations.core_type_to_yojson
let core_type_of_yojson = Yojson_implementations.core_type_of_yojson

let show_tei lid = String.concat "." (Longident.flatten lid)

type injection_arg_element =
  | InjectionType of extensible_type_id
  | InjectionLift of string * extensible_type_id * type_extension_id (* function name, overall, extension *)
  | InjectionUnlift of string * extensible_type_id * type_extension_id (* function name, overall, extension *)
  | InjectionFeatureFunction of string * feature_id * feature_function_id (* function name, feature name, feature function name *)
    [@@deriving show, yojson]


type module_path_element =
  | ModulePathPlain of string
  | ModulePathFunctor of string
  | ModulePathInjectionFunctor of string * injection_arg_element list
    [@@deriving show, yojson]


type feature_function_type_element =
  | FFT_Nonextensible of core_type
  | FFT_Extensible of extensible_type_id


type extensible_type_info = string (* defining file *)
  [@@deriving show, yojson]

type extensible_type_seq = (string * extensible_type_info) list (* name -> extensible_type_info *)
  [@@deriving show, yojson]


type type_extension_info = {
  te_extension_id : type_extension_id;
  te_defining_file : string;
  te_type_parameters : string list
}
  [@@deriving show, yojson]

type type_extension_seq = (extensible_type_id * type_extension_info list) list (* extended name -> [type_extension_info] *)
  [@@deriving show, yojson]


type type_param_assoc_entry = string * string option [@@deriving show]

type feature_function_type_part =
  | TypePartNormalType of core_type (* must not use an extensible type somewhere inside *)
  | TypePartExtensibleType of extensible_type_id [@@deriving show, yojson]




type feature_function_info = {
  ff_function_type : core_type;
  (* An original type t_1 -> t_2 -> ... -> t_n is represented by a list if n elements *)
  ff_function_type_split : feature_function_type_part list;
  (* This is actually a property of the whole feature, not of individual files: *)
  ff_defining_file : string
} [@@deriving yojson, show]

type feature_decl_info = (feature_function_id * feature_function_info) [@@deriving yojson, show]

type feature_decl_seq = (feature_id * feature_decl_info list) list [@@deriving yojson, show]


type t = {
  types : extensible_type_info Utils.StringMap.t;
  extensions : (type_extension_info list) Utils.StringMap.t;
  features : (feature_decl_info list) Utils.StringMap.t
}

type data = {
  extensible_types : extensible_type_seq;
  extensions : type_extension_seq;
  feature_decls : feature_decl_seq
} [@@deriving show]

let create (id : data) : t = {
  types = Utils.string_map_of_seq  id.extensible_types;
  extensions = Utils.string_map_of_seq  id.extensions;
  features = Utils.string_map_of_seq id.feature_decls
}


let has_extensible_type (ad : t) (extensible_type_name : extensible_type_id) : bool =
  Utils.StringMap.mem extensible_type_name ad.types

let has_feature (ad : t) (feature_name : feature_id) : bool =
  Utils.StringMap.mem feature_name ad.features

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