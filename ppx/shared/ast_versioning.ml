(* We express all transformations in terms of a fixed version of the OCaml AST, namely version 4.07
   Migrate_parsetree makes sure that the ASTs we use internally are  converted to the
   actual OCaml version whenever we actually interact with the compiler *)


(* Our fixed Version of the OCaml AST as a module and as a value *)
module Version (*: Migrate_parsetree.Versions.OCaml_version *) = Migrate_parsetree.Versions.OCaml_407
let    version  = Migrate_parsetree.Versions.ocaml_407

module Ast (*: Migrate_parsetree.Versions.Ast*) = Migrate_parsetree.Versions.OCaml_407.Ast
module Parsetree = Migrate_parsetree.Versions.OCaml_407.Ast.Parsetree






(* The ppxlib library has its own fixed AST version, which may differ from our fixed version.
   Thus, we need to translate AST versions whenever we interact with it.Longident

   Ideally, we would just refer to Ppxlib_ast.Selected_ast here, without hard-coding their version.
   However, this causes OCaml to miss some equivalences between versions, breaking things *)


(*module Ppxlib_ast_version : Migrate_parsetree.Versions.OCaml_version = Ppxlib_ast.Selected_ast*)
module Ppxlib_ast_version = Migrate_parsetree.Versions.OCaml_407

module To_ppxlib = Migrate_parsetree.Versions.Convert(Version)(Ppxlib_ast_version)
module From_ppxlib = Migrate_parsetree.Versions.Convert(Ppxlib_ast_version)(Version)


let structure_to_ppxlib = To_ppxlib.copy_structure
let structure_from_ppxlib = From_ppxlib.copy_structure

(*
let structure_item_from_ppxlib (si : Ppxlib_ast_version.Ast.Parsetree.structure_item )  =
  List.hd (From_ppxlib.copy_structure [si] )
*)

let pattern_to_ppxlib = To_ppxlib.copy_pattern
let type_to_ppxlib = To_ppxlib.copy_core_type

let expression_to_ppxlib (e : Parsetree.expression) = To_ppxlib.copy_expression e

let core_type_to_ppxlib (t : Parsetree.core_type) = To_ppxlib.copy_core_type t

