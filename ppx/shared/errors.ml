let raise_error location msg : 'a =
  let error = Location.error ~loc:location (msg ^ "\n")  in
  Location.report_error Format.err_formatter error; 
  failwith "Dying after PPX error"


let raise_info msg : unit =
    print_endline msg