
open Jigsaw_ppx_shared.Ast_versioning.Ast
(*open Jigsaw_ppx_shared.Ast_versioning.Parsetree*)

(*
type stage =
  | Stage_one
  | Stage_two
  | Stage_either
  | Stage_inconclusive

let stage_two_executables = ["ocamlc" ; "ocamlc.opt" ; "ocamlopt" ; "ocamlopt.op"]

let evaluate_cmd_args _config _cookies =
  print_endline ("sys0: " ^ Sys.argv.(0));
  if Array.length Sys.argv >= 2 && Sys.argv.(1) = "--as-ppx" &&
    (List.mem Sys.argv.(0) stage_two_executables ) then
    Stage_two
  else
    Stage_one


let evaluate_load_path _config _cookies  =
  match !Clflags.include_dirs with
    | [] -> Stage_either
    | _ -> Stage_two

(*let evaluate_cookies _config cookies =
  match Driver.get_cookie cookies "library-name" with
    | None -> Stage_two
    | Some _ -> Stage_one*)


let determine_stage config cookies =
  let evaluators = [evaluate_cmd_args ; evaluate_load_path ] in
  List.fold_left (fun prev f ->
    let res = f config cookies in
    match prev, res with
      | Stage_inconclusive, _
      | Stage_one, Stage_two
      | Stage_two, Stage_one -> Stage_inconclusive
      | Stage_one, _
      |  _, Stage_one -> Stage_one
      | Stage_two, _
      |  _, Stage_two -> Stage_two
      | _ -> Stage_either
  ) Stage_either evaluators

*)




let toplevel_struct config cookies _ strct =
  let first_stage_executed = List.exists Jigsaw_ppx_shared.Ast_manipulation.is_first_stage_marker_extension strct in
  let mapper_to_delegate_to =
    if first_stage_executed then
      (Printf.printf "second stage chosen";
      Analysis_stage_two.mapper config cookies)
    else
      Analysis_stage_one.mapper config cookies in
  mapper_to_delegate_to.structure mapper_to_delegate_to strct



let disambiguation_mapper  config cookies =
  {Ast_mapper.default_mapper with
    structure = toplevel_struct config cookies}
  (*match determine_stage config cookies with
    | Stage_one -> Analysis_stage_one.mapper config cookies
    | Stage_two -> Analysis_stage_two.mapper config cookies
    | _ ->
      Jigsaw_ppx_shared.Errors.raise_error_noloc
        "Unable to determine whether we are called as a ppx rewriter (by the compiler) or as a standalone rewriter (called by, e.g., dune)"*)



let () = Migrate_parsetree.Driver.register
      ~name:"analysis_mapper"
      Jigsaw_ppx_shared.Ast_versioning.ocaml_version
      disambiguation_mapper
