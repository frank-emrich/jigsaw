open Migrate_parsetree



(* We express all transformations in terms of OCaml 4.07 ASTs.
   Migrate_parsetree makes sure that the ASTs are then converted to the
   actual OCaml version being executed *)
open Ast_407
open Parsetree
let ocaml_version = Versions.ocaml_407


module AnalysisResults = 
struct
  let raise_info = Jigsaw_ppx_shared.Errors.raise_info
  let type_table = Hashtbl.create 10 
  let extension_table = Hashtbl.create 10

  let register_extensible_type type_name ext_constructor ext_type_var =
    raise_info ("adding extensible type " ^ type_name);
    Hashtbl.add type_table type_name (ext_constructor, ext_type_var)


  let register_type_extension extension_name extended_type_name extension_path = 
    raise_info ("adding extension " ^ extension_name ^ " for type " ^ String.concat "." (Longident.flatten extended_type_name) ^ ", current module path is " ^ String.concat "." extension_path);
    match Hashtbl.find_opt extension_table extended_type_name with
      | None -> 
        Hashtbl.add extension_table extended_type_name [(extension_name, extension_path)] 
      | Some existing_extensions -> 
         Hashtbl.add extension_table extended_type_name ((extension_name, extension_path) :: existing_extensions)


  let get_extensible_type type_name =
    Hashtbl.find_opt type_table type_name

  let get_type_extensions extended_type_name = 
    Hashtbl.find_opt extension_table extended_type_name
end


let current_module : string list ref = ref [] 

let extract_type_var core_type =
  match core_type.ptyp_desc with
    | Ptyp_var type_var_name ->  type_var_name
    | _ -> failwith "Attempting to extract type var from something that isn't one"  

let unloc (loc : 'a Asttypes.loc) = loc.txt  

let raise_error = Jigsaw_ppx_shared.Errors.raise_error


let register_extensible_type  = AnalysisResults.register_extensible_type

let register_type_extension  =  AnalysisResults.register_type_extension

let handle_constructor ext_points constr =
  let attrs = constr.pcd_attributes in
  let is_extension_attr attr = unloc (fst attr) = Jigsaw_ppx_shared.Names.Attributes.extension_point in
  match List.find_opt is_extension_attr attrs with
    | Some _attr -> 
      let constr_name = unloc constr.pcd_name in
      let constr_res_type_opt = constr.pcd_res in (* used in GADTs *) 
      let constr_args = constr.pcd_args in
      let constr_loc = constr.pcd_loc in
      (constr_name, constr_loc, constr_args, constr_res_type_opt) :: ext_points
    | None -> ext_points



let handle_attribute _ attr = 
  let name = unloc (fst attr) in
  let payload = snd attr in
  let bad_shape loc = raise_error loc  ("The " ^ Jigsaw_ppx_shared.Names.Attributes.extension_of ^ " attribute should be followed by a single identifier: the name of the type to extend."  ) in
  if name = Jigsaw_ppx_shared.Names.Attributes.extension_of then
    match payload with
      | PStr [{ pstr_desc = eval_expr ; pstr_loc = loc }] ->
        begin match eval_expr with
          | Pstr_eval ( { pexp_desc = (Pexp_ident extended_type_name) ; pexp_loc = _ ; pexp_attributes  = [] }   , []) ->
          Some extended_type_name.txt
          |  _ -> bad_shape loc; None
        end
      | _ ->  bad_shape (fst attr).loc; None
  else
    None


(* Does the declared type have a constructor to be used as an extension point *)
let handle_extensible_type td_record =
  match td_record.ptype_kind with
    | Ptype_variant constrs ->
      let type_param_names = List.map (fun (q,_) -> extract_type_var q) td_record.ptype_params in
      let declared_type_name = unloc (td_record.ptype_name) in
      let name_loc = td_record.ptype_name.loc in
      let extension_points = List.fold_left handle_constructor [] constrs in
      begin match extension_points with
       | [] -> None 
       | [(ext_constr_name, ext_constr_loc, ext_constr_args, ext_constr_res_type)] ->
         begin match ext_constr_res_type, ext_constr_args with
          | Some ct, _ ->  
            raise_error ct.ptyp_loc "Extension point must not have GADT type"; None
          | _, Pcstr_record _ ->
            raise_error ext_constr_loc"Extension point must not have record type"; None 
          | _, Pcstr_tuple []
          | _, Pcstr_tuple (_::_::_) ->
            raise_error ext_constr_loc "Extension point constructor must have single argument"; None   
          | _, Pcstr_tuple [{ptyp_desc = Ptyp_var ext_constr_type_var ; ptyp_loc = _ ; ptyp_attributes = _} ] when List.mem ext_constr_type_var  type_param_names ->
            register_extensible_type declared_type_name ext_constr_name ext_constr_type_var;
            Some td_record
          | _ ->
            raise_error ext_constr_loc "Extension point constructor must accept single type argument, which is a type variable that the declared type is polymorpic over"; None

        end      
       | _ ->  raise_error name_loc ("More than one constructor is annotated to be an extension point"); None
      end

    | _ -> None


(* Does the type declaration have an attribute saying that the whole type represents an extension of another type? *)
let handle_type_extension td_record =
  let extension_of = List.fold_left handle_attribute None td_record.ptype_attributes in
  match extension_of with
    | None ->  None
    | Some extended_type ->
      let extension_name = unloc td_record.ptype_name in
      register_type_extension extension_name extended_type (List.rev !current_module);
      Some td_record





(* Mapper functions *)

let type_declaration m td_record = 
  match handle_extensible_type td_record, handle_type_extension td_record with
    | None, None ->
       Ast_mapper.default_mapper.type_declaration m td_record
    | Some _, Some _ ->
      let loc = td_record.ptype_name.loc in
      raise_error loc "Type cannot be extensible and an extension at the same time"; td_record
    | Some res, _
    |   _, Some res ->
      res  


  
  
let attribute m attr =
  let name = unloc (fst attr) in
  if List.mem name Jigsaw_ppx_shared.Names.Attributes.all then
    raise_error (fst attr).loc ("Found extensibility-related attribute \"" ^ name ^ "\" in an unsupported location")
  else
    ();
  Ast_mapper.default_mapper.attribute m attr

let module_binding mapper mod_binding =
  let mod_name = unloc mod_binding.pmb_name in
  current_module := mod_name :: !current_module;
  let result = Ast_mapper.default_mapper.module_binding mapper mod_binding in
  current_module := List.tl !current_module;
  result

(* End mapper functions  *)





   

let analysis_mapper _config _cookies =
  print_endline "analysis has been invoked";
  { Ast_mapper.default_mapper with
    type_declaration = type_declaration;
    attribute = attribute;
    module_binding = module_binding}



(*let _ = Compiler_libs.Ast_mapper.register "my_mapper" (fun _ -> To_current.copy_mapper my_mapper)*)

