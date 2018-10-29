open Jigsaw_ppx_shared.Ast_versioning.Ast
open Jigsaw_ppx_shared.Ast_versioning.Parsetree

(* This implements the identity-mapper (without copying).
   The analysis rewriter is only usable in the "second stage", when it is invoked by ocamlc or ocamlopt directly *)

let id2 _ x = x

(* TODO: add an extension to the toplevel module so that things break unless
   the second stage is actually executed (and removes the dummy extension) *)


let first_stage_marker () : structure_item =
  let ghost_loc = Jigsaw_ppx_shared.Ast_manipulation.make_ghost_location () in
  let extension_name = Jigsaw_ppx_shared.Names.Extensions.first_stage_marker in
  let extension_name_loc = Location.mkloc extension_name ghost_loc in
  let empty_payload = PStr [] in

  let extension : structure_item_desc = Pstr_extension ((extension_name_loc, empty_payload), []) in
  {
      pstr_desc = extension;
      pstr_loc = ghost_loc;
  }

let top_level_structure _ strct =
  (first_stage_marker () ) :: strct

let mapper _ _ : Ast_mapper.mapper =
{
  Ast_mapper.default_mapper with
  structure = top_level_structure
}

(*  {
    Ast_mapper.attribute = id2;
    attributes = id2;
    case = id2;
    cases = id2;
    class_declaration = id2;
    class_description = id2;
    class_expr = id2;
    class_field = id2;
    class_signature = id2;
    class_structure = id2;
    class_type = id2;
    class_type_declaration = id2;
    class_type_field = id2;
    constructor_declaration = id2;
    expr = id2;
    extension = id2;
    extension_constructor = id2;
    include_declaration = id2;
    include_description = id2;
    label_declaration = id2;
    location = id2;
    module_binding = id2;
    module_declaration= id2;
    module_expr = id2;
    module_type = id2;
    module_type_declaration = id2;
    open_description = id2;
    pat = id2;
    signature = id2;
    signature_item = id2;
    structure = id2;
    structure_item = id2;
    typ = id2;
    type_declaration = id2;
    type_extension = id2;
    type_kind = id2;
    value_binding = id2;
    value_description = id2;
    with_constraint = id2;
    payload = id2;
  }*)


