open Migrate_parsetree


open Ast_407
open Parsetree
let ocaml_version = Versions.ocaml_407

let unloc (loc : 'a Asttypes.loc) = loc.txt  
let raise_error = Jigsaw_ppx_shared.Errors.raise_error


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


let type_declaration m td_record = 
    print_endline "Hello there";
    match td_record.ptype_manifest with
       | Some  {ptyp_desc = (Ptyp_extension ext) ; ptyp_loc = _loc ; ptyp_attributes = _attrs} ->
         let ocaml_syntac_extension_name = unloc (fst ext) in
         print_endline "General Kenobi!";
         print_endline ocaml_syntac_extension_name;
         if  ocaml_syntac_extension_name = Jigsaw_ppx_shared.Names.Extensions.synthesized_extension_type then
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