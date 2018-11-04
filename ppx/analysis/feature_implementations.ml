open Jigsaw_ppx_shared
open Ast_versioning.Ast
open Ast_versioning.Parsetree


let extract_type_from_pattern (pat : pattern) =
  let desc = pat.ppat_desc in
  match desc with
    | Ppat_constraint (_, t) -> Some t
    | _ -> None

let rec extract_type_from_expression expr : core_type =
  let desc = expr.pexp_desc in
  let loc = expr.pexp_loc in
  match desc with
    | Pexp_constraint (_, t) -> t
    | Pexp_fun (label, _, p, e) ->
      Checks.check_no_label_in_feature_function_impl loc label;
      let t2 =  extract_type_from_expression e in
      begin match extract_type_from_pattern p with
        | Some t1 ->
          let ghost_loc = Ast_manipulation.make_ghost_location () in
          {
            ptyp_loc = ghost_loc;
            ptyp_attributes = [];
            ptyp_desc = Ptyp_arrow (Nolabel, t1, t2)
          }
        | None ->
          let pattern_loc = p.ppat_loc in
          Errors.raise_error pattern_loc "This (sub-)pattern does not have a type annotation. Failed to reconstruct type annotation for whole feature function"
      end
    | _ ->
      Errors.raise_error loc "This (sub-)expression does not have a type annotation. Failed to reconstruct type annotation for whole feature function"


(* extracts function parameter patterns from an expression as long as we see nested function definitions *)
let rec extract_function_parameters_from_expression expr : pattern list =
  let desc = expr.pexp_desc in
  match desc with
    | Pexp_constraint (e, _) -> extract_function_parameters_from_expression e
    | Pexp_fun (Nolabel, _, p, e) ->
      p :: extract_function_parameters_from_expression e
    | _ -> []


let extract_feature_attribute_data _ctx (feature_attr : attribute) =
  let payload = snd feature_attr in
  let loc = (fst feature_attr).loc in
  let feature_lid = Ast_manipulation.extract_single_ident_payload loc payload in
  let feature_name, feature_function_name =
    match Longident.flatten feature_lid with
    | [x ; y ] -> x, y
    | _ ->
      Errors.raise_error loc
        "Expected identifier of the shape X.y, where X is the name of a feature
         and y is the name of a feature function of that feature" in

  feature_name, feature_function_name


let match_feature_function_parameter_types_against_declaration
    ctx
    feature_name
    feature_function_name
    index
    (impl_type : core_type)
    (declared_type : Analysis_data.feature_function_type_part)   =
  let get_extension_ids_contextualized extensible_type =
    let extensions = Context.get_extensions_of_type ctx extensible_type in
    List.fold_left
      (fun acc extension ->
        let next_pair = Context.contextualize_lid ctx extension.Analysis_data.te_extension_id, extension.te_extension_id in
        next_pair :: acc)
      []
      extensions in


  let loc = impl_type.ptyp_loc in
  match declared_type with
    | TypePartNormalType expected_type ->
      if Type_equality.eq impl_type expected_type then
        (* TODO: What if no syntactic match due to different module paths? *)
        Analysis_data.ImplTypePartMatch
      else
        let expected_type_string = Errors.string_of_core_type expected_type in
        let impl_type_string = Errors.string_of_core_type impl_type in
        Errors.raise_error loc
          (Printf.sprintf "Type missmatch in component %d of an implementation of function %s of feature %s.
            According to the declaration of the feature, this componenet should have type %s.
            We determined this not to be an extensible type, which means that the implementation of the feature should
            use the exact same type here. However, the implementation uses type %s here, which is not (syntactially) identical"
            (index+1) feature_function_name feature_name expected_type_string impl_type_string)
    | TypePartExtensibleType extensible_type_name ->
      match impl_type.ptyp_desc with
        | Ptyp_constr (lid, _args) ->
          (* TODO: check that  the arguments match what we expected  *)
          (* TODO: Handle the case that we do not want to handle only a specific extension, but the whole type *)
          let contextualized_extensions = get_extension_ids_contextualized extensible_type_name in
          begin match List.find_opt (fun (contextualized_lid, _plain_id) -> contextualized_lid = lid.txt) contextualized_extensions with
            | Some (_, extension_plain_lid) ->
              ImplTypePartExtension (extensible_type_name, extension_plain_lid)
            | None ->
              let flatten_lid = fun x -> String.concat "." (Longident.flatten (fst x)) in
              let supported_extensions = String.concat " , " (List.map flatten_lid  contextualized_extensions) in
              let impl_type_string = Errors.string_of_core_type impl_type in
              Errors.raise_error
                loc
                ((Printf.sprintf "According to the declaration of the function %s of feature %s,
                    we expected one of the extensions of the extensible type %s here.
                    However, the given feature function implementation provides the type %s here, which doesn't match any of the known extensions.
                    Supported type extensions are the following here: "
                    feature_function_name feature_name extensible_type_name impl_type_string) ^ supported_extensions)
          end
        | _ ->
          Errors.raise_error loc
          (Printf.sprintf "According to the declaration of the function %s of feature %s,
            we expected one of the extensions of the extensible type %s here. However, what we found is not a type constructor"
              feature_function_name feature_name extensible_type_name)


let match_feature_function_return_type_against_declaration
    loc
    (impl_ret_type : core_type)
    (declared_return_type : core_type) =
  if Type_equality.eq impl_ret_type declared_return_type then
    ()
  else
    Errors.raise_error
      loc
      (Printf.sprintf "The return type of a feature function implementation must (syntactially) be equal to the declared type.
        However, the declared type is %s, and the type stated here is %s"
        (Errors.string_of_core_type declared_return_type)
        (Errors.string_of_core_type impl_ret_type))

(* If a feature implementaion is prefixed with additional parameters, those are supposed to be the
   overall versions of feature functions of the same feature. Here, we try to map the function parameters
   to feature functions based on the variable names they contain *)
let determine_injections ctx feature_name feature_impl_parameters _types count =
  let rec match_parameter (pat : pattern) =
    let desc =  pat.ppat_desc in
    let loc = pat.ppat_loc in
    match desc with
      | Ppat_var var  ->
        let potential_feature_func = var.txt in
        begin match Context.get_feature_function_of_feature ctx feature_name potential_feature_func with
          | Some _ ->
            potential_feature_func
          | None ->
            Errors.raise_error loc ("We expected the name of this variable to match one of the functions of feature " ^ feature_name)
        end
      | Ppat_constraint (subpat, _) ->
        match_parameter subpat
      | _ ->
        Errors.raise_error loc "This parameter should be a single (possibly type-annotated variable) in order to denote an overall implementation of a feature function" in
  let relevant_patterns = Utils.take count feature_impl_parameters in
  List.map match_parameter relevant_patterns


let check_unique_instance
    ctx
    loc
    feature_name
    feature_function_name
    (type_correspondences : Analysis_data.feature_function_impl_type_part list) =
  let implementations = Context.get_feature_function_implementations ctx feature_name feature_function_name in
  let all_instances_identical (index, prev) my_part their_part =
    match my_part, their_part with
      | Analysis_data.ImplTypePartMatch, Analysis_data.ImplTypePartExtension (extensible_type, other_extension) ->
        let extension_str = String.concat "." (Longident.flatten other_extension) in
        Errors.raise_error
          loc
          (Printf.sprintf "This implementation can handle the whole of type %s as  parameter no. %d,
             but there exists a different implementation that deals with only the extension %s"
             extensible_type (index+1) extension_str)
      | ImplTypePartExtension (extensible_type, extension), ImplTypePartMatch ->
        let extension_str = String.concat "." (Longident.flatten extension) in
        Errors.raise_error
          loc
          (Printf.sprintf "This implementation can deal with only the extension %s of extensible type %s as parameter no. %d,
            but there exists another implementation that can deal with the whole of the type in this position."
             extension_str extensible_type (index+1))
      | ImplTypePartExtension (_, my_extension), ImplTypePartExtension (_, their_extension) ->
        (index + 1, prev && (my_extension = their_extension))
      | ImplTypePartMatch, ImplTypePartMatch ->
        (index + 1, prev) in
    let check_not_all_instances_equal existing_impl =
      let existing_impl_type_correspondences = existing_impl.Analysis_data.ffi_type in
      let (_, identical) =
        List.fold_left2
          all_instances_identical
          (0, true)
          type_correspondences
          existing_impl_type_correspondences in
      if identical then
        Errors.raise_error loc "There exists an implementation of this feature function already for the exact same selection of type extensions"
      else
        () in
    List.iter (fun (_, impl) -> check_not_all_instances_equal impl) implementations


let process_feature_function_implementation ctx (binding : value_binding) (feature_attr : attribute)   =
  let pat = binding.pvb_pat in
  let expr = binding.pvb_expr in
  let loc = binding.pvb_loc in
  let attr_loc = (fst feature_attr).loc in

  let feature_name, feature_function_name = extract_feature_attribute_data ctx feature_attr in
  Checks.check_feature_exists ctx attr_loc feature_name;
  Checks.check_feature_function_exists ctx attr_loc feature_name feature_function_name;

  let feature_function_info : Analysis_data.feature_function_info =
    match Context.get_feature_function_of_feature ctx feature_name feature_function_name with
      | Some i -> i
      | None -> assert false (* caught earlier *) in

  let feature_function_declared_type = feature_function_info.ff_function_type in
  let feature_function_declared_type_split = feature_function_info.ff_function_type_split in
  let feature_function_declared_type_split_without_ret =
    Utils.take
      (List.length feature_function_declared_type_split -1)
      feature_function_declared_type_split in
  let feature_function_type_part_count = List.length feature_function_declared_type_split in
  let feature_function_declared_return_type =
    match Utils.last feature_function_declared_type_split with
      | Analysis_data.TypePartNormalType t -> t
      | _ -> failwith "Analysis of feature function declared type must always yield TypePartNormalType for return type" in

  let typ =
    match extract_type_from_pattern pat with
      | Some t -> t
      | None -> extract_type_from_expression expr in
  let typ_all_listified = Feature_declarations.listify_type typ in
  let typ_all_part_count = List.length typ_all_listified in
  if typ_all_part_count < feature_function_type_part_count then
    Errors.raise_error
      loc
      (Printf.sprintf
        "The function %s of feature %s has type %s, consisting of %d parts.
         However, you annotated this feature function implementation with a type that only has %d"
         feature_function_name
         feature_name
         (Errors.string_of_core_type feature_function_declared_type)
         feature_function_type_part_count
         typ_all_part_count)
  else
    let injection_function_count = typ_all_part_count - feature_function_type_part_count in
    let injection_function_types_split, function_type_split = Utils.split_list_after injection_function_count typ_all_listified in
    let function_type_split_without_ret = Utils.take (List.length function_type_split - 1) function_type_split in
    let function_parameters = extract_function_parameters_from_expression expr in
    let feature_functions_to_inject = determine_injections ctx feature_name function_parameters injection_function_types_split injection_function_count in
    let type_correspondences =
      Utils.mapi2
        (match_feature_function_parameter_types_against_declaration ctx feature_name feature_function_name)
        function_type_split_without_ret
        feature_function_declared_type_split_without_ret in
    match_feature_function_return_type_against_declaration
      loc
      (Utils.last function_type_split)
      feature_function_declared_return_type;
    check_unique_instance
      ctx
      loc
      feature_name
      feature_function_name
      type_correspondences;
    Context.register_feature_function_implementation
      ctx
      feature_name
      feature_function_name
      feature_functions_to_inject
      type_correspondences



(* Test whether function binding is a feature function implementation *)

let handle_feature_function_implementation ctx (b : value_binding) : value_binding option =
  let attrs = b.pvb_attributes in
  let impl_attr_name = Names.Attributes.feature_implementation in
  let is_impl_attr = Ast_manipulation.attribute_has_name impl_attr_name in
  let (impl_attrs, non_impl_attrs) = List.partition is_impl_attr attrs in
  match impl_attrs with
    | [] -> None
    | [attr] ->
      Checks.check_no_extensibility_related_attributes non_impl_attrs;
      process_feature_function_implementation ctx b attr;
      Some b
    | _ ->
      Errors.raise_error b.pvb_loc ("More than one attribute of name " ^ impl_attr_name)