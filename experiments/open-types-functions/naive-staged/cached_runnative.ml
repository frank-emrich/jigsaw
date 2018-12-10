(* Copy of runnative.ml from BER MetaOcaml, but allows passing flags to the staged compiler
   (e.g., for optimization), and caches results, if enabled*)


(* Given a closed code expression, compile it with the *native*
   compiler, link it in, and run returning
   its result or propagating raised exceptions.
*)

open Codelib
open Format



let debug_msg msg =
  if Shared.debug then
    prerr_endline msg
  else
    ()

module Cache =
struct

  (* Maps Hashed (AST  + compilation flags) to compiled files. We need to do the hashing ourselves to fake being polymorphic over 't code *)
  let the_table : (int, string) Hashtbl.t ref = ref (Hashtbl.create 5)
  let changed : bool ref = ref false


  let print_table tbl =
    if Shared.debug then
      Hashtbl.iter (fun k v -> prerr_endline ("Key: " ^ string_of_int k ^ ", value: " ^ v )) tbl
    else
      ()

  let attempt_load_from_disk path =
    if Sys.file_exists path then
      (debug_msg ("loading file " ^ path);
      let c = open_in  path in
      let table = Marshal.from_channel c in
      the_table := table;
      close_in c;
      print_table table)

  let save_if_changed path =
    if !changed then
      (
      debug_msg ("saving file " ^ path);
      print_table !the_table;
      let c = open_out path in
      Marshal.to_channel c !the_table [];
      close_out c)
    else
      ()


  let find_compiled_file_opt ((code, compilation_flags) as key) =
    let key_hashed = Hashtbl.hash key in
    debug_msg ("hashed key find_compiled_file_opt " ^ string_of_int key_hashed  );
    match Hashtbl.find_opt !the_table key_hashed with
      | None -> None
      | Some compiled_file ->
        if Sys.file_exists compiled_file then
          Some compiled_file
        else
          (* remove outdated entry from hash table *)
          (debug_msg ("removing file from cache " ^ string_of_int key_hashed );
          changed := true;
          Hashtbl.remove !the_table key_hashed;
          None)

  let add_compiled_file ((code, compilation_flags) as key) path =
    let key_hashed = Hashtbl.hash key in
    debug_msg ("hashed key add_compiled_file " ^ string_of_int key_hashed  );
    changed := true;
    Hashtbl.add !the_table key_hashed path;
    print_table !the_table


end




let load_path : string list ref = ref []

(* Add a directory to search for .cmo/.cmi files, needed
   for the sake of running the generated code .
   The specified directory is prepended to the load_path.
*)
let add_search_path : string -> unit = fun dir ->
  load_path := dir :: !load_path

let ocamlopt_path =
  let open Filename in
  concat (dirname (Config.standard_runtime)) "ocamlopt"

(* Compile the source file and make the .cmxs, returning its name *)
let compile_source : string -> string -> string = fun compilation_flags src_fname ->
  let basename = Filename.remove_extension src_fname in
  let plugin_fname =  basename ^ ".cmxs" in
  let other_files  =  [basename ^ ".cmi"; basename ^ ".cmx";
                       basename ^ ".o"] in
  let cmdline = ocamlopt_path ^
                " " ^ compilation_flags ^ " "^
                " -shared" ^
                " -o " ^ plugin_fname ^
                (String.concat "" @@
                 List.map (fun p -> " -I " ^ p) !load_path) ^
                " " ^ src_fname in
  debug_msg ("Runnative cmdline: "  ^ cmdline);
  let rc = Sys.command cmdline in
  List.iter Sys.remove other_files;
  if rc = 0 then plugin_fname else
    let () = Sys.remove plugin_fname in
    failwith "runnative: .cmxs compilation failure"

(*
 Dynlink library can only load the unit and evaluate its top-level
 expressions. There is no provision for accessing the names defined
 in the loaded unit. Therefore, the only way to get the result is
 to assign it to a reference cell defined in the main program.
 This file defines "result__" exactly for this purpose.

 Given the code cde, we generate a file

 Runnative.result__ := Some (Obj.repr (cde))

which we then compile and link in.
*)

(* The reference cell below contains something other than None for a brief
   period, before the value is taken and returned to the caller of
   runnative. This policy prevents memory leaks
*)
let result__ : Obj.t option ref = ref None

let code_file_prefix = "runn"


(* Create a file to compile and later link, using the given closed code *)
let create_comp_unit : 'a closed_code -> string = fun cde ->
  let (fname,oc) =
    Filename.open_temp_file ~mode:[Open_wronly;Open_creat;Open_text]
      code_file_prefix ".ml" in
  let ppf = formatter_of_out_channel oc in
  let ()  = fprintf ppf
      "Cached_runnative.result__ := Some (Obj.repr (%a))@."
      format_code cde in
  let () = close_out oc in
  fname                                 (* let the errors propagate *)


let create_plugin_file compilation_flags cde =
  let source_fname = create_comp_unit cde in
  compile_source compilation_flags source_fname

let determine_plugin_file compilation_flags use_caching cde =
  if use_caching then
    match Cache.find_compiled_file_opt (cde, compilation_flags) with
      | Some plugin_file ->
        debug_msg "found file in cache";
        plugin_file
      | None ->
        let plugin_file = create_plugin_file compilation_flags cde in
        debug_msg "adding file to cache";
        Cache.add_compiled_file (cde, compilation_flags) plugin_file;
        plugin_file
  else
    create_plugin_file compilation_flags cde


let run_native : string -> bool -> 'a closed_code -> 'a = fun compilation_flags use_caching cde ->
  if not Dynlink.is_native then
    failwith "run_native only works in the native code";
  let plugin_fname = determine_plugin_file compilation_flags use_caching cde in
  let () = Dynlink.loadfile_private plugin_fname in
  (*Sys.remove plugin_fname;*)
  (*Sys.remove source_fname;*)
  match !result__ with
  | None -> assert false                (* can't happen *)
  | Some x ->
      result__ := None;                 (* prevent the memory leak *)
      Obj.obj x
  (* If an exception is raised, leave the source and plug-in files,
     so to investigate the problem.
   *)

(* Abbreviations for backwards compatibility *)
let run compilation_flags use_caching cde = run_native compilation_flags use_caching (close_code cde)
let (!.) cde = run cde
