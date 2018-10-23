(* We need to process the files containing the definitions of extensions, feature declarations, feature implementations.
   In order to do so, we register the corresponding analysis mapper and then call it on the files in question.
   The necessary information are stored in mutable, global state because the PPX infrastructure is designed to
   rewrite ASTs, not to return analysis information from them. *)


(* The current approach has downsides:
   The synthesis is done by invoking the analysis on every single source file of the main package.
   Thus, for each of them the analysis of all of the core and the extensions is repeated.

   This means that in the long term, we should go for a a design where the analysis PPX passes store there results together
   with the compilation results each time the core or an extension/feature is compiled *)



let init () =
  print_endline "registered mapper";
  Custom_driver.register
      ~name:"analysis_mapper_external"
      Jigsaw_ppx_analysis.Analysis.ocaml_version
      Jigsaw_ppx_analysis.Analysis.analysis_mapper

let filename_to_module_name  fname =
       fname |> Filename.basename |> Filename.remove_extension |> String.capitalize_ascii

let process_file (library, file) =
      let config : Custom_driver.config =
      (* TODO: we could add -I, -L and -g options to populate these fields. *)
      { tool_name    = "migrate_driver"
      ; include_dirs = []
      ; load_path    = []
      ; debug        = false
      ; for_package  = None
      ; extras       = []
      } in
      let boxed_file = Custom_driver.guess_file_kind file in
      (* This is hacky, we may want to do this via cookies *)
      let module_name = filename_to_module_name file in
      let initial_module_path_rev =
            if library = module_name then 
                  [module_name]
            else 
                  [module_name ; library] in
      Jigsaw_ppx_analysis.Analysis.current_module := initial_module_path_rev;
      Custom_driver.process_file ~config:config ~output:None  ~output_mode:Null ~embed_errors:false boxed_file