(* We put the registration of the mapper here so that we can link with the analysis mapper without registering it (i.e., without loading this module) *)

let () = Migrate_parsetree.Driver.register ~name:"analysis_mapper" Jigsaw_ppx_shared.Ast_versioning.ocaml_version Jigsaw_ppx_analysis.Analysis.analysis_mapper
