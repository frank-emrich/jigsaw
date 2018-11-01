(*open Ast_versioning.Ast*)
open Ast_versioning.Parsetree


type marshalled_type_wrapper = {
  mtw_ast_magic_number: string;
  mtw_human_readable : string; (* This is just for debugging, to get a human readable representation of the marshalled data. It is not used for unmarshalling *)
  mtw_marshalled_core_type : string
} [@@deriving yojson]

(* Note that this is not the current compiler magic number, but the one associated with our fixed version for internal use *)
let our_ast_magic_number = Ast_versioning.Version.Ast.Config.ast_impl_magic_number

let marshall_type (ct : core_type) =
  (our_ast_magic_number, Marshal.to_string ct)


let core_type_to_yojson ct =
  let mtw =
    {
      mtw_ast_magic_number = our_ast_magic_number;
      mtw_human_readable = Errors.string_of_core_type ct;
      mtw_marshalled_core_type = Marshal.to_string ct []
    } in
  marshalled_type_wrapper_to_yojson mtw

let core_type_of_yojson json =
  match marshalled_type_wrapper_of_yojson json with
    | Error msg -> Error msg
    | Ok (wrapped : marshalled_type_wrapper) ->
      if wrapped.mtw_ast_magic_number = our_ast_magic_number then
        let ct = Marshal.from_string wrapped.mtw_marshalled_core_type 0 in
        Ok ct
      else
        (* We could translate between the versions here instead of failing *)
        Errors.raise_error_noloc "Trying to unmarshall data that was marshalled using a different version fo the OCaml AST."