(* The ASTs we get from metaquot use a fixed version of the OCaml AST.
   Our own code transformations are expressed in another (possibly different) fixed version of the OCaml AST.
   This module provides functions to translate between the two. *)

module Metaquot_version = Ppxlib_ast.Selected_ast 
module Our_version = Migrate_parsetree.OCaml_407
module To_Metaquot = Migrate_parsetree.Versions.Convert(Our_version)(Metaquot_version)
module From_Metaquot = Migrate_parsetree.Versions.Convert(Metaquot_version)(Our_version)


let structure_to_metaquot = To_Metaquot.copy_structure
let structure_from_metaquot = From_Metaquot.copy_structure

let structure_item_from_metaquot si = List.hd (From_Metaquot.copy_structure [si] )

let pattern_to_metaquot = To_Metaquot.copy_pattern
let type_to_metaquot = To_Metaquot.copy_core_type