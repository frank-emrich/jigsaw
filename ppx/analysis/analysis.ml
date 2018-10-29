
open Jigsaw_ppx_shared.Ast_versioning.Ast
(*open Jigsaw_ppx_shared.Ast_versioning.Parsetree*)




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


let () = Migrate_parsetree.Driver.register
      ~name:"analysis_mapper"
      Jigsaw_ppx_shared.Ast_versioning.version
      disambiguation_mapper
