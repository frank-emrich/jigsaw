


let raise_error location msg : 'a =
  let error = Location.error ~loc:location (msg ^ "\n")  in
  Location.report_error Format.err_formatter error;
  failwith "Dying after PPX error"

let raise_error_noloc msg =
  failwith msg

let info ?(loc=None) msg : unit =
    let _bogus = loc in
    print_endline msg

let debug ?(loc=None) msg : unit =
    let _bogus = loc in
    print_endline msg

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
