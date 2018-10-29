
module Extensions =
struct
  let synthesized_type = "synthesized_type"
  let first_stage_marker = "misconfiguration__you_must_enable_staged_preprocessing"

  let all = [synthesized_type ; first_stage_marker]
end


module Attributes =
struct
  let extension_point = "extension_point"
  let extension_of = "extension_of"
  let inject = "inject"
  let library_name = "currently-compiled-library-name"

  let all = [extension_point ; extension_of ; inject; library_name]
end

module Cookies =
struct
  let library_name = "library-name"
end

let lowercase_ident = "\\([a-z][0-9a-zA-Z_']*\\)"
let lift_regexp = Str.regexp ("^lift_" ^ lowercase_ident ^ "_to_" ^ lowercase_ident ^ "$")
let unlift_regexp = Str.regexp ("^unlift_" ^ lowercase_ident ^ "_to_" ^ lowercase_ident ^ "$")

let parse_lift_function_name function_name =
  if Str.string_match lift_regexp function_name 0 then
    let extended_type = Str.matched_group 2 function_name in
    let type_extension = Str.matched_group 1 function_name in
    Some (extended_type, type_extension)
  else
    None


let parse_unlift_function_name function_name =
  if Str.string_match unlift_regexp function_name 0 then
    let extended_type = Str.matched_group 1 function_name in
    let type_extension = Str.matched_group 2 function_name in
    Some (extended_type, type_extension)
  else
   None