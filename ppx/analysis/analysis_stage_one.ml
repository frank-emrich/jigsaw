open Migrate_parsetree
open Jigsaw_ppx_shared.Ast_versioning.Ast
open Jigsaw_ppx_shared.Ast_versioning.Parsetree


module E = Jigsaw_ppx_shared.Errors

(* This implements the identity-mapper (without copying).
   The analysis rewriter is only usable in the "second stage", when it is invoked by ocamlc or ocamlopt directly *)

let id2 _ x = x

(* TODO: add an extension to the toplevel module so that things break unless
   the second stage is actually executed (and removes the dummy extension) *)


let get_library_name cookies =
  let cookie_name = Jigsaw_ppx_shared.Names.Cookies.library_name in
  match Driver.get_cookie cookies cookie_name Jigsaw_ppx_shared.Ast_versioning.version with
    | None -> E.raise_error_noloc ("No cookie named " ^ cookie_name ^ " was passed.")
    | Some lib_expr ->
      Jigsaw_ppx_shared.Ast_manipulation.extract_single_string_constant lib_expr



let first_stage_marker cookies : structure_item =
  let ghost_loc = Jigsaw_ppx_shared.Ast_manipulation.make_ghost_location () in
  let extension_name = Jigsaw_ppx_shared.Names.Extensions.first_stage_marker in
  let extension_name_loc = Location.mkloc extension_name ghost_loc in
  let library_name = get_library_name cookies in
  let library_name_attr =
    Jigsaw_ppx_shared.Ast_manipulation.make_single_ident_content_attribute
      Jigsaw_ppx_shared.Names.Attributes.library_name
      (Longident.parse  library_name) in

  let empty_payload = PStr [] in

  let extension : structure_item_desc = Pstr_extension ((extension_name_loc, empty_payload), [library_name_attr]) in
  {
      pstr_desc = extension;
      pstr_loc = ghost_loc;
  }

let top_level_structure cookies _ strct =
  (first_stage_marker cookies ) :: strct

let mapper _ cookies : Ast_mapper.mapper =
{
  Ast_mapper.default_mapper with
  structure = top_level_structure cookies
}

