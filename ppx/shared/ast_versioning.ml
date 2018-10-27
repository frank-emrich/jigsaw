(* We express all transformations in terms of OCaml 4.07 ASTs.
   Migrate_parsetree makes sure that the ASTs are then converted to the
   actual OCaml version being executed *)
module Ast = Migrate_parsetree.Ast_407
module Parsetree = Migrate_parsetree.Ast_407.Parsetree

let ocaml_version = Migrate_parsetree.Versions.ocaml_407