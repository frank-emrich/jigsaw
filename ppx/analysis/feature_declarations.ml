

module AD = Jigsaw_ppx_shared.Analysis_data
module AM = Jigsaw_ppx_shared.Ast_manipulation
module E = Jigsaw_ppx_shared.Errors
module U = Jigsaw_ppx_shared.Utils

open Jigsaw_ppx_shared
open Ast_versioning.Ast
open Ast_versioning.Parsetree

let unloc (x : _ Asttypes.loc) = x.txt


(* turns a type t1 -> t2 -> ... -> tn into a list [t1; t2;...; tn] *)
let listify_type (ct : core_type) =
  let rec lt t =
    let loc = t.ptyp_loc in
    let desc = t.ptyp_desc in
    match desc with
      | Ptyp_arrow (Nolabel, t1, t2) ->
        t1 :: (lt t2)
      | Ptyp_arrow (_, _, _) ->
        Errors.raise_error loc "We do not support feature functions with labelled or optional parameters"
      | _ -> [t] in
    lt ct

(* Collects all type constructors occuring in a type. Duplicates are possible *)
let collect_type_constructors ct : Longident.t list=
  let constructors : Longident.t list ref  = ref [] in
  let typ (rec_mapper : Ast_mapper.mapper) t =
    match t.ptyp_desc with
      | Ptyp_constr (lid, args) ->
        constructors := (lid.txt :: !constructors);
        let _ = List.map (rec_mapper.typ rec_mapper) args in
        t
      | _ -> Ast_mapper.default_mapper.typ rec_mapper t  in
  let mapper = {Ast_mapper.default_mapper with typ = typ} in
  let _ = mapper.typ mapper ct in
  !constructors

let handle_feature_function_type (ctx : Context.t) (t : core_type) : Analysis_data.feature_function_type_part list =
  let active_types = Context.get_active_injection_functor_types ctx in
  Errors.debug ("Active types: " ^ String.concat "," active_types);
  let type_listified = listify_type t in
  let type_part_num = List.length type_listified in
  let type_constructor_matches_extensible_type lid =
    match Longident.flatten lid with
      | [name] when List.mem name active_types -> true
      | _ -> false in
  let handle_type_part index (tp : core_type) =
    let is_return_type = (index = type_part_num - 1) in
    let desc = tp.ptyp_desc in
    (*let loc = tp.ptyp_loc in*)
    match is_return_type, desc with
      | true, _ -> Analysis_data.TypePartNormalType tp
      | _, Ptyp_constr (lid, _args) when type_constructor_matches_extensible_type lid.txt ->
        Analysis_data.TypePartExtensibleType (Longident.last lid.txt)
      | _ ->
        (*let type_constructors = collect_type_constructors tp in
        if List.exists type_constructor_matches_extensible_type type_constructors then
          Errors.raise_error
          loc
          "Extensible type used as an argument to another type constructor.
           Given a feature function of type t_1 -> t_2 -> .. -> t_n, extensible types are only allowed
           if they are identical to one of the t_i, but not nested inside of one of the t_i."
        else*)
        Analysis_data.TypePartNormalType tp in

  List.mapi handle_type_part type_listified





let check_feature_not_declared_already ctx decl_loc feature_name =
  if Context.has_feature ctx feature_name then
    E.raise_error decl_loc ("The feature " ^ feature_name ^ " has already been declared elsewhere")
  else
    ()

let check_feature_decls_not_empty parent_loc (s : signature) =
  if s = [] then
    E.raise_error parent_loc "This feature declaration is empty. This is probably not what you want"
  else
    ()

let handle_feature_decl_signature_item ctx feature_name (item : signature_item) =
  let loc = item.psig_loc in
  let desc = item.psig_desc in
  match desc with
    | Psig_value value_desc ->
      let typ = value_desc.pval_type in
      let type_split = handle_feature_function_type ctx typ in
      let function_name = unloc value_desc.pval_name in
      Context.register_feature_function
        ctx
        feature_name
        function_name
        typ
        type_split;
    | _ -> Errors.raise_error loc "Only val items supported in feature decl"

let handle_feature_decl_module_type ctx feature_name (mt : module_type) =
  let loc = mt.pmty_loc in
  let desc = mt.pmty_desc in
  match desc with
    | Pmty_signature s ->
      check_feature_decls_not_empty loc s;
      List.iter (handle_feature_decl_signature_item ctx feature_name) s
    | _ -> Errors.raise_error loc "You must provide a signature literal (i.e., sig ... end) when declaring a feature"

let handle_feature_decl ctx (module_type_decl : module_type_declaration) =
  let attributes = module_type_decl.pmtd_attributes in
  let loc = module_type_decl.pmtd_loc in
  let feature_decl_attr_name = Names.Attributes.feature_decl in
  let (feature_decl_attrs, non_feature_decl_attrs) =
    List.partition  (AM.attribute_has_name feature_decl_attr_name) attributes in
  Checks.check_no_extensibility_related_attributes non_feature_decl_attrs;
  let module_type_opt = module_type_decl.pmtd_type in
  match feature_decl_attrs, module_type_opt with
    | [], _ -> None
    | [attr], Some mt ->
      Checks.check_attr_payload_empty attr;
      let feature_name = unloc (module_type_decl.pmtd_name) in
      check_feature_not_declared_already ctx loc feature_name;
      handle_feature_decl_module_type ctx feature_name mt;
      Some module_type_decl
    | [_], None ->
      Errors.raise_error loc ("Module type declaration was annotated to be for a feature declaration, but no body was provided for the type")
    | _ -> Errors.raise_error loc ("Found multiple attributes of name " ^ feature_decl_attr_name)



