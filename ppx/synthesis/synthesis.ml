open Migrate_parsetree
open Jigsaw_ppx_shared.Ast_versioning.Ast
open Jigsaw_ppx_shared.Ast_versioning.Parsetree

let unloc (loc : 'a Asttypes.loc) = loc.txt
let raise_error = Jigsaw_ppx_shared.Errors.raise_error
let info = Jigsaw_ppx_shared.Errors.raise_info
let verify_condition cond loc msg = if cond then () else raise_error loc msg




let active_file = !Location.input_name

(* Maps *)
let synthesized_extension_types = Hashtbl.create 10
let get_type_name_for_synthesized_extension extended_type_name =
  Hashtbl.find_opt synthesized_extension_types extended_type_name

let register_synthesized_name type_name =
  Hashtbl.add synthesized_extension_types type_name type_name

(* Such a list of files would ultimately be obtained from something linke ocamlfind *)
let files_to_check = [ 
    ("Core", "../../../core/types.ml");
    ("Core", "../../../core/ir.ml");
    ("Let", "../../../extensions/jigsaw-let/let.ml");
    ("Query", "../../../extensions/jigsaw-query/query.ml");
    ("Stringify", "../../../features/jigsaw-stringify-feature/stringify.ml");
    ("Stringify_let", "../../../features/jigsaw-stringify-for-let/stringify_for_let.ml");
    ("Stringify_query", "../../../features/jigsaw-stringify-for-query/stringify_for_query.ml");
     ]


let make_ghost_location () = Location.in_file !Location.input_name
let locify x loc = Location.mkloc x loc


let () = Run_analysis.init ()

let _ = List.iter Run_analysis.process_file files_to_check

let constructor_name_of_extension_path extension_name extension_path =
  (String.concat ""  (List.map String.capitalize_ascii extension_path)) ^ (String.capitalize_ascii extension_name)

let map_type_parameter_name_to_extensible_type param : string option =
  if Jigsaw_ppx_analysis.Analysis_results.has_extensible_type param then
    Some param
  else
    None


let ident_for_extension_path extension_name extension_path =
  match Longident.unflatten  (List.append extension_path [extension_name]) with
    | Some i -> i
    | None -> failwith "ident_for_extension_path failed"

let constructor_name_for_extension extended_type_name extension_name = "Ext_" ^ extended_type_name ^ "_" ^ extension_name

let extension_type_parameter_to_type_argument original_loc ghost_loc extended_type_name type_param =
      match map_type_parameter_name_to_extensible_type type_param with
       |  None -> raise_error original_loc ("Cannot type parameter " ^ type_param ^ " to the name of an extensible type")
       |  Some other_type -> match get_type_name_for_synthesized_extension other_type with
        | None -> raise_error original_loc ("Trying to synthesize type "
          ^  extended_type_name
          ^ ", which depends on "
          ^ other_type
          ^ ", but we havent synthesized the latter one yet ");
        | Some type_name ->
          let type_loc = Location.mkloc (Longident.parse type_name) ghost_loc in
          Ast_helper.Typ.constr ~loc:ghost_loc type_loc []

(* TODO: This only works if we use the resulting type in the same module where it was defined *)
let extended_type_name_to_core_type extended_type_name : core_type =
  let ghost_location = make_ghost_location () in
  Ast_helper.Typ.constr ?loc:(Some ghost_location) (locify (Longident.parse extended_type_name) ghost_location) []

(* Points to where the extension was defined*)
let extension_name_to_longident  extension_path extension_name =
  match Longident.unflatten (extension_path @[extension_name] ) with
   | Some lid -> lid
   | None -> failwith "unexpected error"

(* Points to where the extensible type was synthesized *)
let extension_constructor_to_longident extended_type_name _extension_path extension_name =
  (* TODO: We need to save the location where something was synthesized or at least provide a mechanism to specify so with an attribute *)
  Longident.parse (constructor_name_for_extension extended_type_name extension_name)


let extension_name_to_core_type original_loc ghost_loc extension_path extension_name type_parameters : core_type =
  let extension_longident = extension_name_to_longident extension_path extension_name in
  let extension_name_loced = Location.mkloc extension_longident ghost_loc in
  let type_args : core_type list = List.map (extension_type_parameter_to_type_argument original_loc ghost_loc extension_name) type_parameters in
  Ast_helper.Typ.constr ~loc:ghost_loc extension_name_loced type_args


let construtctor_for_extension synth_loc extended_type_name (extension_name, _extension_path, type_var_mapping) : constructor_declaration =
  let ghost_location = Location.in_file !Location.input_name in
  let constr_name = constructor_name_for_extension extended_type_name extension_name in
  let constr_name_loced = Location.mkloc constr_name ghost_location in

  let constr_type = extension_name_to_core_type synth_loc ghost_location _extension_path extension_name type_var_mapping in
  let constr_args : constructor_arguments = Pcstr_tuple [constr_type] in
  info ("Adding constructor " ^ constr_name ^ " to type " ^ extended_type_name ^ " to represent extension " ^ extension_name );
  Ast_helper.Type.constructor ~loc:ghost_location ~args:constr_args constr_name_loced

let lift_function original_loc ghost_loc extended_type_name extension_path extension_name type_parameters : structure_item =
  let loc = ghost_loc in
  let fun_name = extended_type_name ^ "_of_" ^ extension_name in
  let fun_name_pattern = Metaquot_versioning.pattern_to_metaquot (Ast_helper.Pat.var ?loc:(Some loc) (locify fun_name loc)) in
  let overall_type = extended_type_name_to_core_type extended_type_name in
  let extension_constructor_lid = extension_constructor_to_longident extended_type_name extension_path extension_name in
  let constructor_application = Ast_helper.Exp.construct ?loc:(Some loc) (locify extension_constructor_lid ghost_loc) (Some [%expr x] ) in
  let extension_type = Metaquot_versioning.type_to_metaquot (extension_name_to_core_type original_loc ghost_loc extension_path extension_name type_parameters) in
  let meta_quot_function  =  [%stri  let [%p fun_name_pattern] : ([%t extension_type] -> [%t overall_type])  =  fun x -> [%e constructor_application] ] in
  Metaquot_versioning.structure_item_from_metaquot meta_quot_function


let unlift_function original_loc ghost_loc extended_type_name extension_path extension_name type_parameters =
  let loc = ghost_loc in
  let fun_name = extension_name  ^ "_of_" ^ extended_type_name in
  let fun_name_pattern = Metaquot_versioning.pattern_to_metaquot (Ast_helper.Pat.var ?loc:(Some loc) (locify fun_name loc)) in
  let overall_type = extended_type_name_to_core_type extended_type_name in
  let extension_type = Metaquot_versioning.type_to_metaquot (extension_name_to_core_type original_loc ghost_loc extension_path extension_name type_parameters) in
  let extension_constructor_lid = extension_constructor_to_longident extended_type_name extension_path extension_name in
  let constructor_pattern = Metaquot_versioning.pattern_to_metaquot (Ast_helper.Pat.construct ?loc:(Some ghost_loc) (locify extension_constructor_lid ghost_loc) (Some [%pat? x])) in
  let meta_quot_function =
    [%stri
      let [%p fun_name_pattern] : ([%t overall_type] -> ([%t extension_type] option)) = function
      | [%p constructor_pattern]  -> Some x
      | _ -> None
        [@@warning "-11"] (* Disable warning about superfluos case, which may occur if only one constructor exists, making the _ case unreachable *)
    ] in
  Metaquot_versioning.structure_item_from_metaquot meta_quot_function

let synthesized_type synth_loc extended_type_name original_type_decl  =
  let ghost_location = Location.in_file !Location.input_name in
  match Jigsaw_ppx_analysis.Analysis_results.get_type_extensions extended_type_name with
    | None -> raise_error synth_loc (extended_type_name ^ " is not registered as an extensible type")
    | Some [] ->
      (* There are no extensions for the given type. The extension type is defined to be unit *)
      info ("Synthesized type " ^ extended_type_name ^ "to be unit due to lack of extensions" );
      let unit_loc = Location.mkloc (Longident.parse "unit")  ghost_location in
      let unit_type = Ast_helper.Typ.constr ~loc:ghost_location unit_loc [] in
      {original_type_decl with ptype_manifest = Some unit_type}
    | Some exts ->
      let constructors : constructor_declaration list =
        List.map (fun ext -> construtctor_for_extension synth_loc extended_type_name ext) exts in
      let kind : type_kind = Ptype_variant constructors in
      {original_type_decl with
        ptype_manifest = None ;
        ptype_kind = kind}


let synthesize_lift_functions synth_loc extended_type_name =
  let ghost_loc = make_ghost_location () in
  match Jigsaw_ppx_analysis.Analysis_results.get_type_extensions extended_type_name with
   | None  -> raise_error synth_loc ("Trying to synthesize lift functions for unknown extensible type: " ^ extended_type_name)
   | Some extensions ->
      let lift_functions = List.map (fun (a,b,c) -> lift_function synth_loc ghost_loc extended_type_name b a c) extensions in
      let unlift_functions = List.map (fun (a,b,c) -> unlift_function synth_loc ghost_loc extended_type_name b a c) extensions in
      lift_functions @ unlift_functions


let type_declaration_struct m td_record : (type_declaration * structure_item list) =
    print_endline ("Hello there, " ^ !Location.input_name);
    match td_record.ptype_manifest with
       | Some  {ptyp_desc = Ptyp_extension (extension_loc, extension_payload) ; ptyp_loc = _loc ; ptyp_attributes = []} ->
         let ocaml_syntac_extension_name = unloc extension_loc in
         if  ocaml_syntac_extension_name = Jigsaw_ppx_shared.Names.Extensions.synthesized_type then
          let extended_type_lid = Jigsaw_ppx_shared.Payload_manipulation.extract_single_ident_payload extension_loc.loc extension_payload in
          let extended_type = String.concat "." (Longident.flatten extended_type_lid) in
          let declared_type_name = unloc td_record.ptype_name in
          verify_condition (declared_type_name = extended_type) td_record.ptype_name.loc ("Name of synthesized type (namely: " ^ extended_type ^ ") must match name of type to be synthesized (namely: " ^ declared_type_name ^")") ;
          register_synthesized_name extended_type;
          let lift_functions = synthesize_lift_functions _loc extended_type in
          (synthesized_type _loc extended_type td_record, lift_functions)
         else
            (Ast_mapper.default_mapper.type_declaration m  td_record, [])
       | _ -> (Ast_mapper.default_mapper.type_declaration m  td_record, [])


let structure_item m item : structure_item list =
  let original_loc = item.pstr_loc in
  match item.pstr_desc with
   | Pstr_type (rec_flag, type_decls) ->
    let accumulate_extra_structitems (acc_decls, acc_struct_items) decl =
       let (decl', extra_items) = type_declaration_struct m decl in
       (decl' :: acc_decls, extra_items :: acc_struct_items) in
    let (decls_reved, extra_struct_items_reved_nested) = List.fold_left accumulate_extra_structitems ([], []) type_decls in
    let decls = List.rev decls_reved in
    (* We accept that the outer list is reved, but the inner lists stay in order *)
    let extra_struct_items = List.concat extra_struct_items_reved_nested in
    List.cons  (Ast_helper.Str.type_ ?loc:(Some original_loc) rec_flag decls) extra_struct_items
   | _ -> [Ast_mapper.default_mapper.structure_item m item]

let structure m items = items |> List.map (structure_item m) |> List.concat

let attribute m attr =
  let name = unloc (fst attr) in
  if List.mem name Jigsaw_ppx_shared.Names.Attributes.all then
    raise_error (fst attr).loc ("Found attribute " ^ name ^ "  in an unsupported location")
  else
    ();
  Ast_mapper.default_mapper.attribute m attr


let synthesis_mapper _config _cookies =
  { Ast_mapper.default_mapper with
    structure = structure;
    (*attribute = attribute*)}


(* register with the driver *)
let () = Driver.register ~name:"synthesis_mapper" ocaml_version synthesis_mapper