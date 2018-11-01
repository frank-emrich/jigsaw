

module AD = Jigsaw_ppx_shared.Analysis_data
module AM = Jigsaw_ppx_shared.Ast_manipulation
module E = Jigsaw_ppx_shared.Errors
module U = Jigsaw_ppx_shared.Utils

open Jigsaw_ppx_shared
open Ast_versioning.Ast
open Ast_versioning.Parsetree

let unloc (x : _ Asttypes.loc) = x.txt

let check_no_payload (attr : attribute) =
  if Ast_manipulation.attribute_has_empty_payload attr then
    ()
  else
    Errors.raise_error (fst attr).loc ("Attribute " ^ (fst attr).txt ^ " should not have payload")

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
      let function_name = unloc value_desc.pval_name in
      Context.register_feature_function ctx feature_name function_name typ;
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
      check_no_payload attr;
      let feature_name = unloc (module_type_decl.pmtd_name) in
      check_feature_not_declared_already ctx loc feature_name;
      handle_feature_decl_module_type ctx feature_name mt;
      Some module_type_decl
    | [_], None ->
      Errors.raise_error loc ("Module type declaration was annotated to be for a feature declaration, but no body was provided for the type")
    | _ -> Errors.raise_error loc ("Found multiple attributes of name " ^ feature_decl_attr_name)