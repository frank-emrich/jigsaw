open Migrate_parsetree


open Ast_407
open Parsetree
let ocaml_version = Versions.ocaml_407

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
let files_to_check = ["tests/test_analysis.ml"]




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

let constructor_name_for_extension extended_type_name extension_name = "Ext" ^ extended_type_name ^ extension_name

let construtctor_for_extension synth_loc extended_type_name (extension_name, _extension_path, type_var_mapping) : constructor_declaration =
  let ghost_location = Location.in_file !Location.input_name in
  let param_to_arg type_param =
      match map_type_parameter_name_to_extensible_type type_param with
       |  None -> raise_error synth_loc ("Cannot type parameter " ^ type_param ^ " to the name of an extensible type")
       |  Some other_type -> match get_type_name_for_synthesized_extension other_type with
        | None -> raise_error synth_loc ("Trying to synthesize type "
          ^  extended_type_name
          ^ ", which depends on "
          ^ other_type
          ^ ", but we havent synthesized the latter one yet ");
        | Some type_name ->
          let type_loc = Location.mkloc (Longident.parse type_name) ghost_location in
          Ast_helper.Typ.constr ~loc:ghost_location type_loc []  in


  let constr_name = constructor_name_for_extension extended_type_name extension_name in
  let constr_name_loced = Location.mkloc constr_name ghost_location in
  let extension_longident = match Longident.unflatten (_extension_path @[extension_name] ) with
   | Some lid -> lid
   | None -> failwith "unexpected error"  in
  let extension_name_loced = Location.mkloc extension_longident ghost_location in
  let type_args : core_type list = List.map param_to_arg type_var_mapping in
  let constr_type : core_type = Ast_helper.Typ.constr ~loc:ghost_location extension_name_loced type_args in
  let constr_args : constructor_arguments = Pcstr_tuple [constr_type] in
  info ("Adding constructor " ^ constr_name ^ " to type " ^ extended_type_name ^ " to represent extension " ^ extension_name );
  Ast_helper.Type.constructor ~loc:ghost_location ~args:constr_args constr_name_loced



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




let type_declaration m td_record =
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
          synthesized_type _loc extended_type td_record
         else
            Ast_mapper.default_mapper.type_declaration m  td_record
       | _ -> Ast_mapper.default_mapper.type_declaration m  td_record



let attribute m attr =
  let name = unloc (fst attr) in
  if List.mem name Jigsaw_ppx_shared.Names.Attributes.all then
    raise_error (fst attr).loc ("Found attribute " ^ name ^ "  in an unsupported location")
  else
    ();
  Ast_mapper.default_mapper.attribute m attr


let synthesis_mapper _config _cookies =
  { Ast_mapper.default_mapper with
    type_declaration = type_declaration;
    (*attribute = attribute*)}


(* register with the driver *)
let () = Driver.register ~name:"synthesis_mapper" ocaml_version synthesis_mapper