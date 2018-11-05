let is_debug_mode =
  match Sys.getenv_opt Names.Env.debug with
    | Some _ -> true
    | None -> false

let is_print_stacktrace_enabled =
  match Sys.getenv_opt Names.Env.ocamlrunparam with
    | Some p -> String.contains p 'b'
    | None -> false


(* If we just raise an exception here, the stack trace that's printed
   only shows where the preprocessor was called during the execution of  ocamlc/ocamlopt,
   which is useless for debuggin *)
let raise_error location msg : 'a =
  let error = Location.error ~loc:location (msg ^ "\n")  in
  Location.report_error Format.err_formatter error;
  (*if is_print_stacktrace_enabled then
    prerr_endline  (Printexc.get_backtrace ());
  exit 1*)
  failwith "Dying after error"

let raise_error_noloc msg =
  failwith msg


let info ?(loc=None) msg : unit =
    let _bogus = loc in
    print_endline msg

let debug ?(loc=None) msg : unit =
  if is_debug_mode then
    let _bogus = loc in
    print_endline msg
  else
    ()

let check condition : unit =
  if not condition then raise_error_noloc "Internal Error" else ()


let string_of_expression (expr : Ast_versioning.Parsetree.expression) =
  let ppxlib_versioned_expr = Ast_versioning.expression_to_ppxlib expr in
  Ppxlib_ast.Pprintast.string_of_expression ppxlib_versioned_expr

let print_core_type fmt ct =
  let ppxlib_versioned_type = Ast_versioning.core_type_to_ppxlib ct in
  Ppxlib_ast.Pprintast.core_type fmt ppxlib_versioned_type

let string_of_core_type ct =
  Format.asprintf "%a" print_core_type ct


let print_structure fmt st =
  let ppxlib_versioned_structure = Ast_versioning.structure_to_ppxlib st in
  Ppxlib_ast.Pprintast.structure fmt ppxlib_versioned_structure

let string_of_structure st =
  Format.asprintf "%a" print_structure st

let print_module_expr fmt mexpr =
  let ppxlib_versioned_module_expr = Ast_versioning.module_expr_to_ppxlib mexpr in
  Ppxlib_ast.Pprintast.module_expr fmt ppxlib_versioned_module_expr

let string_of_module_expr mexpr =
  Format.asprintf "%a" print_module_expr mexpr