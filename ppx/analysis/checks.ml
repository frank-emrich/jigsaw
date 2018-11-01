
open Jigsaw_ppx_shared
(*open Ast_versioning.Ast*)
open Ast_versioning.Parsetree

let check_no_extensibility_related_attributes ?(except=None) (attrs : attributes) =
  let is_forbidden_attr (attr : attribute) =
    let name = (fst attr).txt in
    match except with
      | None ->
        List.mem name Names.Attributes.all
      | Some allowed ->
        name <> allowed && List.mem name Names.Attributes.all in
  match List.find_opt is_forbidden_attr attrs with
    | None -> ()
    | Some forbidden_attr ->
      Errors.raise_error
        (fst forbidden_attr).loc
        ("Found extensibility-related attribute \"" ^ (fst forbidden_attr).txt ^ "\" in an unsupported location")
