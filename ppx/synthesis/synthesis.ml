open Migrate_parsetree


open Ast_407
open Parsetree
let ocaml_version = Versions.ocaml_407

let unloc (loc : 'a Asttypes.loc) = loc.txt  
let raise_error = Jigsaw_ppx_shared.Errors.raise_error

let active_file = !Location.input_name

(* Maps *)
let synthesized_extension_types = Hashtbl.create 10
let get_type_name_for_synthesized_extension extended_type_name = 
  Hashtbl.find_opt synthesized_extension_types extended_type_name 

(* Such a list of files would ultimately be obtained from something linke ocamlfind *)
let files_to_check = ["tests/test_analysis.ml"] 


let _ = print_endline "toplevel in synthesis"
let () = Run_analysis.init ()

let _ = List.iter Run_analysis.process_file files_to_check

let constructor_name_of_extension_path extension_name extension_path =
  (String.concat ""  (List.map String.capitalize_ascii extension_path)) ^ (String.capitalize_ascii extension_name)

let ident_for_extension_path extension_name extension_path =
  match Longident.unflatten  (List.append extension_path [extension_name]) with
    | Some i -> i
    | None -> failwith "ident_for_extension_path failed" 

let constructor_name_for_extension extended_type_name extension_name = "Ext" ^ extended_type_name ^ extension_name

let construtctor_for_extension synth_loc extended_type_name (extension_name, _extension_path, type_var_mapping) : constructor_declaration =
  let ghost_location = Location.in_file !Location.input_name in
  let param_to_arg (_, type_opt) = match type_opt with
    | None -> failwith "This should not have happened"
    | Some ext_type_name -> 
      match get_type_name_for_synthesized_extension ext_type_name with
        | None -> raise_error synth_loc ("Trying to synthesize extension type for " 
          ^  extended_type_name 
          ^ ", which depends on " 
          ^ ext_type_name 
          ^ ", but we havent synthesized that one yet"); 
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
  Ast_helper.Type.constructor ~loc:ghost_location ~args:constr_args constr_name_loced

  

let synthesized_extension_type synth_loc extended_type_name original_type_decl  =
  let extended_type_name_string = String.concat "." (Longident.flatten extended_type_name) in
  let ghost_location = Location.in_file !Location.input_name in
  match Jigsaw_ppx_analysis.Analysis.AnalysisResults.get_type_extensions extended_type_name with
    | None -> raise_error synth_loc (extended_type_name_string ^ " is not registered as an extensible type")
    | Some [] ->
      (* There are no extensions for the given type. The extension type is defined to be unit *)
      let unit_loc = Location.mkloc (Longident.parse "unit")  ghost_location in
      let unit_type = Ast_helper.Typ.constr ~loc:ghost_location unit_loc [] in 
      {original_type_decl with ptype_manifest = Some unit_type} 
    | Some exts ->  
      print_endline "in an extended type now";
      let extended_type_name_str = String.concat "." (Longident.flatten extended_type_name) in
      let constructors : constructor_declaration list = 
        List.map (fun ext -> construtctor_for_extension synth_loc extended_type_name_str ext) exts in 
      let kind : type_kind = Ptype_variant constructors in
      {original_type_decl with 
        ptype_manifest = None ;
        ptype_kind = kind} 




let type_declaration m td_record = 
    print_endline ("Hello there, " ^ !Location.input_name);
    match td_record.ptype_manifest with
       | Some  {ptyp_desc = (Ptyp_extension ext) ; ptyp_loc = _loc ; ptyp_attributes = _attrs} ->
         let ocaml_syntac_extension_name = unloc (fst ext) in
         print_endline "General Kenobi!";
         print_endline ocaml_syntac_extension_name;
         if  ocaml_syntac_extension_name = Jigsaw_ppx_shared.Names.Extensions.synthesized_extension_type then
          (*synthesized_extension_type loc *)
            begin
                print_endline "matched";
                (* TODO *)
               { td_record with 
                ptype_manifest = None ;
                ptype_kind = Ptype_abstract } 
            end
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