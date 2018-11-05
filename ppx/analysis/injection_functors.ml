open Jigsaw_ppx_shared.Ast_versioning.Ast
open Jigsaw_ppx_shared.Ast_versioning.Parsetree


let unloc (loc : 'a Asttypes.loc) = loc.txt
(*let raise_error = Jigsaw_ppx_shared.Errors.raise_error
let info_message _loc_opt = Jigsaw_ppx_shared.Errors.info
let debug_message _loc_opt = Jigsaw_ppx_shared.Errors.debug*)


module AD = Jigsaw_ppx_shared.Analysis_data
module E = Jigsaw_ppx_shared.Errors
module U = Jigsaw_ppx_shared.Utils
open Jigsaw_ppx_shared



let get_lift_function_name extensible_type (extension : Analysis_data.type_extension_id) =
  "lift_" ^ Longident.last extension ^ "_to_" ^ extensible_type

let get_unlift_function_name extensible_type (extension : Analysis_data.type_extension_id) =
  "unlift_" ^ extensible_type ^ "_to_" ^ Longident.last extension

let extensible_type_to_core_type extensible_type : core_type =
  let loced_name = Ast_manipulation.make_ghost_loced (Longident.parse extensible_type) in
  Ast_helper.Typ.constr loced_name []


let extension_type_to_core_type ctx (extension : Analysis_data.type_extension_info) : core_type =
  let args = List.map extensible_type_to_core_type extension.te_type_parameters in
  let name_contexualized = Context.contextualize_lid ctx extension.te_extension_id in
  let name_loced = Ast_manipulation.make_ghost_loced name_contexualized in
  Ast_helper.Typ.constr name_loced args



let lift_function_type
    ctx
    (extensible_type : Analysis_data.extensible_type_id)
    (extension : Analysis_data.type_extension_info) =
  let from_type = extension_type_to_core_type ctx  extension in
  let to_type = extensible_type_to_core_type extensible_type in
  Ast_helper.Typ.arrow Nolabel from_type to_type

let unlift_function_type
    ctx
    (extensible_type : Analysis_data.extensible_type_id)
    (extension : Analysis_data.type_extension_info) =
  let from_type = extensible_type_to_core_type extensible_type  in
  let to_type =
    Ast_manipulation.optionalize_core_type
      (extension_type_to_core_type ctx  extension) in
  Ast_helper.Typ.arrow Nolabel from_type to_type


(* TODO: So far, all matching of functions in injection functor is based on their names.
   Instead, we should have attributes for this and only fall back to using names if there is no attribute *)

let validate_type_decl ctx overall_loc type_decl =
    let name = unloc type_decl.ptype_name in
    Errors.debug ("Seen type " ^ name);
    let kind = type_decl.ptype_kind in
    let manifest = type_decl.ptype_manifest in
    match kind, manifest with
      | Ptype_abstract, None ->
        (* TODO: check that the name matches a known extensible type *)
        if Context.has_extensible_type ctx name then
          AD.InjectionType name
        else
          E.raise_error type_decl.ptype_loc ("Injection functor contains type " ^ name ^ ", which is not a previously declared extensible type")
      | _ -> E.raise_error overall_loc "Only abstract types without manifest supported in injection functor argument"

(* TODO: Use the functions here for creating the necessary types*)
let check_extensible_type_name extensible_type_core_type expected_extensible_type_name =
  let desc = extensible_type_core_type.ptyp_desc in
  let loc = extensible_type_core_type.ptyp_loc in
  let error () = E.raise_error loc (Printf.sprintf "Expected type %s here" expected_extensible_type_name) in
  match desc with
    | Ptyp_constr (ident_loced, []) ->
      let name = Longident.flatten ident_loced.txt in
      begin match name with
        | [type_name] when type_name = expected_extensible_type_name -> ()
        | _ -> E.raise_error loc (Printf.sprintf "Expected unqualified name of extensible type here, namely type %s" expected_extensible_type_name)
      end
    | _ -> error ()

let check_type_extension ctx extension_type_core_type (expected_extension_type_name : AD.type_extension_id) =
  let expected_extension_type_name_contextualized = Context.contextualize_lid ctx expected_extension_type_name in
  let desc = extension_type_core_type.ptyp_desc in
  let loc = extension_type_core_type.ptyp_loc in
  let extension_name_string = AD.show_tei expected_extension_type_name_contextualized in
  match desc with
    | Ptyp_constr (ident_loced, _args) ->
      (* TODO: We could check here that the right arguments are given to the type extension *)
      if ident_loced.txt = expected_extension_type_name_contextualized then
        ()
      else
        E.raise_error loc ("Expected type used here to refer to type extension named " ^ extension_name_string)
    | _ -> E.raise_error loc (Printf.sprintf "Expected type refering to the type extension named %s" extension_name_string )

let check_optionized_extension_type ctx opt_extension_type_core_type expected_extension_type_name =
    let desc = opt_extension_type_core_type.ptyp_desc in
    let loc = opt_extension_type_core_type.ptyp_loc in
    match desc with
      | Ptyp_constr (lid, [arg]) when String.concat "." (Longident.flatten lid.txt ) = "option"->
        check_type_extension ctx arg expected_extension_type_name
      | _ ->
        (* TODO: Eventually, determine the fully applied extension type here to print it*)
        E.raise_error loc "Right-hand side type of unlift function should be option of a type extension"

let validate_lift_function_decl ctx previous loc function_name function_type extended_type (type_extension : string) =
  (* TODO Validate existence of _type_extension *)
  if List.exists ((=) (AD.InjectionType extended_type)) previous then
    let function_type_desc = function_type.ptyp_desc in
    let function_type_loc = function_type.ptyp_loc in
    let extension_identifier = match Context.get_extensions_by_unqualified_name ctx extended_type type_extension with
      | [] -> E.raise_error loc (Printf.sprintf "No extension %s of type %s known" type_extension extended_type)
      | [ext] -> ext.te_extension_id
      | _ -> E.raise_error loc (Printf.sprintf "There is more than one extension of %s with (unqualified) name %s" extended_type type_extension) in
    match function_type_desc with
      | Ptyp_arrow (Nolabel, from_type, to_type) ->
        check_extensible_type_name to_type extended_type;
        check_type_extension ctx from_type extension_identifier;
        AD.InjectionLift (function_name, extended_type, extension_identifier)
      | _ -> E.raise_error function_type_loc "A lift function should have a type of the shape some_type_extension  -> extensible_type"
  else
    E.raise_error
        loc
        (Printf.sprintf
            "Function %s refers to the extensible type %s, but it has not been declared inside of the injection functor argument"
            function_name
            extended_type)

(* TODO de-duplicate w.r.t  validate_lift_function_decl*)
let validate_unlift_function_decl ctx previous loc function_name function_type extended_type (type_extension : string) =
  if List.exists ((=) (AD.InjectionType extended_type)) previous then
    let function_type_desc = function_type.ptyp_desc in
    let function_type_loc = function_type.ptyp_loc in
    let extension_identifier = match Context.get_extensions_by_unqualified_name ctx extended_type type_extension with
      | [] -> E.raise_error loc (Printf.sprintf "No extension %s of type %s known" type_extension extended_type)
      | [ext] -> ext.te_extension_id
      | _ -> E.raise_error loc (Printf.sprintf "There is more than one extension of %s with (unqualified) name %s" extended_type type_extension) in
    match function_type_desc with
      | Ptyp_arrow (Nolabel, from_type, to_type) ->
        check_extensible_type_name from_type extended_type;
        check_optionized_extension_type ctx to_type extension_identifier;
        AD.InjectionUnlift (function_name, extended_type, extension_identifier)

      | _ -> E.raise_error function_type_loc "An unlift function should have a type of the shape  -> some_type_extension option"
  else
    E.raise_error
        loc
        (Printf.sprintf
            "Function %s refers to the extensible type %s, but it has not been declared inside of the injection functor argument"
            function_name
            extended_type)

let validate_feature_function _previous function_name =
  (* TODO implement properly *)
  AD.InjectionFeatureFunction (function_name, "implement", "me")


let handle_injection_functor_argument_signature ctx argument_signature =
  let handle_type _previous loc (type_decl : type_declaration) =
     validate_type_decl ctx loc type_decl in
  let handle_value previous (value_desc : value_description) =
    let name = unloc value_desc.pval_name in
    let loc = value_desc.pval_name.loc in
    let value_type = value_desc.pval_type in
    let parse_lift = Jigsaw_ppx_shared.Names.parse_lift_function_name in
    let parse_unlift = Jigsaw_ppx_shared.Names.parse_unlift_function_name in
    match (parse_lift name, parse_unlift name) with
      | Some (extended_type, type_extension), None ->
        E.debug
            ~loc:(Some loc)
            (Printf.sprintf "Based on its name, function %s has been determined to be a lifting function from %s to %s"
                name type_extension extended_type);
        validate_lift_function_decl ctx previous loc name value_type extended_type type_extension
      | None, Some (extended_type, type_extension) ->
        E.debug
            ~loc:(Some loc)
            (Printf.sprintf "Based on its name, function %s has been determined to be a un-lifting function from %s to %s"
                name  extended_type type_extension);
        validate_unlift_function_decl ctx previous loc name value_type extended_type type_extension
      | Some _, Some _ -> failwith "Function name must not indicate lift and unlift at the same time"
      | None, None ->
        E.debug (Printf.sprintf "Assuming that function %s corresponds to a feature function" name);
        validate_feature_function previous name
         in

  let handle_item previous (argument_signature_item : signature_item) =
    match argument_signature_item.psig_desc with
      | Psig_type (_rec_flag, type_decls) ->
        let loc = argument_signature_item.psig_loc in
        List.fold_left (fun prev_inner type_decl ->  (handle_type prev_inner loc  type_decl) :: prev_inner ) previous type_decls
      | Psig_value (value_desc : value_description) ->
        (handle_value previous value_desc) :: previous
      | _ -> E.raise_error argument_signature_item.psig_loc "Unsupported item in injection functor" in
  List.rev (List.fold_left handle_item [] argument_signature)

(* At this point we already know that we have an injection functor *)
let handle_injection_functor ctx module_type =
  let desc = module_type.pmty_desc in
  let loc = module_type.pmty_loc in
  match desc with
    | Pmty_signature signature ->
      handle_injection_functor_argument_signature ctx signature
    | _ -> E.raise_error loc "The type of an injection functor argument should be stated as an in-line signature (i.e., sig ... end)"






(* auto injection stuff below *)


let create_type_items ctx : signature_item list =
  let ghost_loc = Ast_manipulation.make_ghost_location () in
  let extensible_types = Context.get_extensible_types ctx in
  let make_abstract_type name : signature_item =
    let loced_name = Ast_manipulation.make_loced ghost_loc name in
    let type_decl =
      Ast_helper.Type.mk
        ~loc:ghost_loc
        ~kind:Ptype_abstract
        loced_name in
    Ast_helper.Sig.type_
      ~loc:ghost_loc
      Recursive
      [type_decl] in
  List.map make_abstract_type extensible_types

let check_no_name_clash loc (names : Utils.StringSet.t) name =
  if Utils.StringSet.mem name names then
    Errors.raise_error loc ("Name clash: Would have two functions of name " ^ name ^ " in argument of injection functor")

let create_lift_unlift_functions ctx loc : signature_item list * Utils.StringSet.t =
  let ghost_loc = Ast_manipulation.make_ghost_location () in
  let extensible_types = Context.get_extensible_types ctx in
  let for_extension names extensible_type (type_extension : Analysis_data.type_extension_info) =
    let lift_name = get_lift_function_name extensible_type type_extension.te_extension_id in
    check_no_name_clash loc names lift_name;
    let lift_descr : value_description =
       {
        pval_name = Ast_manipulation.make_loced ghost_loc lift_name;
        pval_type = lift_function_type ctx extensible_type type_extension;
        pval_prim = [];
        pval_attributes = []; (* TODO: add lift function marker*)
        pval_loc = ghost_loc;
      } in
    let lift_value = Ast_helper.Sig.value ~loc:ghost_loc lift_descr in

    let unlift_name = get_unlift_function_name extensible_type type_extension.te_extension_id in
    check_no_name_clash loc names unlift_name;
    let unlift_descr : value_description =
       {
        pval_name = Ast_manipulation.make_loced ghost_loc unlift_name;
        pval_type = unlift_function_type ctx extensible_type type_extension;
        pval_prim = [];
        pval_attributes = []; (* TODO: add unlift function marker*)
        pval_loc = ghost_loc;
      } in
    let unlift_value = Ast_helper.Sig.value ~loc:ghost_loc unlift_descr in
    (lift_value, lift_name, unlift_value, unlift_name) in


  let for_extensible_type prev extensible_type =
    let extensions : Analysis_data.type_extension_info list =
      Context.get_extensions_of_type ctx extensible_type in
    List.fold_left
      (fun (items, names) extension ->
        let (lift, lift_name, unlift, unlift_name) = for_extension names extensible_type  extension in
        let new_names_set = Utils.StringSet.add lift_name (Utils.StringSet.add unlift_name names) in
        (lift :: unlift :: items), new_names_set)
      prev
      extensions in
  List.fold_left for_extensible_type ([], Utils.StringSet.empty) extensible_types


let create_feature_functions
    ctx
    loc
    (taken_fnames  : Utils.StringSet.t)
    (requested_features : Analysis_data.feature_id list) : signature_item list =
  let ghost_loc = Ast_manipulation.make_ghost_location () in
  let for_feature_function (feature_function : Analysis_data.feature_decl_info) =
    let function_name = fst feature_function in
    check_no_name_clash loc taken_fnames function_name;
    let function_type = (snd feature_function).ff_function_type in
    let feature_func_descr : value_description=
       {
        pval_name = Ast_manipulation.make_loced ghost_loc function_name;
        pval_type = function_type;
        pval_prim = [];
        pval_attributes = []; (* TODO: add feature function marker *)
        pval_loc = ghost_loc;
      } in
    let function_value = Ast_helper.Sig.value ~loc:ghost_loc feature_func_descr in
    (function_value, function_name) in

  let for_feature prev feature_name =
    let feature_functions = Context.get_feature_functions_of_feature ctx feature_name in
    List.fold_left
      (fun (items, names) feature_function ->
         let (fvalue, fname) = for_feature_function feature_function in
         (fvalue :: items), (Utils.StringSet.add fname names))
      prev
      feature_functions in
  let (items, _taken_names) = List.fold_left for_feature ([], taken_fnames) requested_features in
  items





let collect_features_to_inject ctx  attrs =
  let feature_attr = Names.Attributes.with_feature in
  let errorh loc =
    Errors.raise_error loc "Expected comma-separated list of one or more feature names" in
  let feature_lid_to_feature loc lid =
    match Longident.flatten lid with
      | [fn] ->
        Checks.check_feature_exists ctx loc fn;
        fn
      | _ -> Errors.raise_error loc "Expected feature name, in particular something unqualified" in
  let handle_attr prev (attr : attribute) =
    let name = (fst attr).txt in
    let loc = (fst attr).loc in
    let payload = (snd attr) in
    print_endline ("name: " ^ name);
    if name = Names.Attributes.with_feature then
      let feature_lids =
        Ast_manipulation.extract_tuple_payload
          errorh
          Checks.check_no_extensibility_related_attributes
          loc
          payload in
      (List.map (feature_lid_to_feature loc) feature_lids @ prev)
    else
      prev in
  Checks.check_no_extensibility_related_attributes ~except:[feature_attr] attrs;
  List.fold_left handle_attr [] attrs


let create_argument_signature ctx loc requested_features : module_type =
  let ghost_loc = Ast_manipulation.make_ghost_location () in

  let type_decls : signature_item list = create_type_items ctx in

  (* TODO: Check for function name clashes *)
  let lift_unlift_functions, taken_names  = create_lift_unlift_functions ctx loc in
  let feature_functions : signature_item list = create_feature_functions ctx loc taken_names requested_features in

  let signature = type_decls @ lift_unlift_functions @ feature_functions in
  let inject_attr = Ast_manipulation.make_empty_attribute ghost_loc Names.Attributes.inject in
  Ast_helper.Mty.signature ~loc:ghost_loc ~attrs:[inject_attr] signature


let add_open_of_dependencies_module (mod_expr : module_expr) : module_expr =
  let ghost_loc = Ast_manipulation.make_ghost_location () in

  let desc = mod_expr.pmod_desc in
  let loc = mod_expr.pmod_loc in

  let open_lid =
    Ast_manipulation.make_loced
      ghost_loc
      (Longident.parse Names.Mods.injection_functor_argument) in
  let open_descr : open_description =
    Ast_helper.Opn.mk ~loc:ghost_loc open_lid in

  match desc with
    | Pmod_structure str ->
      let open_item = Ast_helper.Str.open_ ~loc:ghost_loc open_descr in
      let extended_str = Pmod_structure ( open_item :: str) in
        {
          mod_expr with
          pmod_desc = extended_str
        }
    | _ -> Errors.raise_error loc "Expected a structure literal here (i.e., struct ... end)"

let create_module_binding ctx (original_mb : module_binding) requested_features : module_binding =
  let ghost_loc = Ast_manipulation.make_ghost_location () in

  let name = original_mb.pmb_name in
  let functor_arg_name = Names.Mods.injection_functor_argument in
  let functor_arg_name_loced = Ast_manipulation.make_loced ghost_loc functor_arg_name in
  let module_expression = add_open_of_dependencies_module original_mb.pmb_expr in
  let loc = original_mb.pmb_loc in
  let attrs = original_mb.pmb_attributes in
  Checks.check_no_extensibility_related_attributes attrs;

  let argument_sig : module_type = create_argument_signature ctx loc requested_features in
  let functor_module_expr : module_expr =
    Ast_helper.Mod.functor_ ~loc:ghost_loc ~attrs:[] functor_arg_name_loced (Some argument_sig) module_expression in
  Ast_helper.Mb.mk ~loc:ghost_loc ~attrs:attrs name functor_module_expr


let process_auto_inject_module ctx (ext : extension) : module_binding =
  let extension_name = (fst ext).txt in
  let loc = (fst ext).loc in
  let payload = snd ext in
  match payload with
    | PStr [st_item] ->
      let st_loc = st_item.pstr_loc in
      let st_desc = st_item.pstr_desc in
      begin match st_desc with
        | Pstr_module original_mod_binding ->
          let attrs = original_mod_binding.pmb_attributes in
          let requested_features = collect_features_to_inject ctx attrs in
          create_module_binding ctx original_mod_binding requested_features
        | _ -> Errors.raise_error st_loc "Expected module here"
      end
    | _ -> Errors.raise_error loc ("Payload of extension " ^ extension_name ^ " must be structure containing single item")


let handle_auto_inject_module ctx (ext : extension) attrs =
  Checks.check_no_extensibility_related_attributes attrs;
  let extension_name = (fst ext).txt in
  if extension_name = Names.Extensions.inject_types_module then
    Some (process_auto_inject_module ctx ext))
  else
    None