
module Extensions = 
struct
  let synthesized_extension_type = "synthesized_extension_type"

  let all = [synthesized_extension_type] 
end


module Attributes =
struct
  let extension_point = "extension_point"
  let extension_of = "extension_of"

  let all = [extension_point ; extension_of] 
end
