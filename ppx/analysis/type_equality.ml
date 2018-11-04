(* We can't just use structural type equality on core types, because they contain location information,
   which is irrelevant for type equality tests. *)

open Jigsaw_ppx_shared
open Ast_versioning.Ast


(* This mapper replaces all locations with a fixed dummy location *)
let location _ _ = Location.none
let loc_elimination_mapper = {Ast_mapper.default_mapper with location = location }



let eq t1 t2 =
  let t1_noloc = loc_elimination_mapper.typ loc_elimination_mapper t1 in
  let t2_noloc = loc_elimination_mapper.typ loc_elimination_mapper t2 in
  t1_noloc = t2_noloc