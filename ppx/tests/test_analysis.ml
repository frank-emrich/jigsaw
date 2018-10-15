let x = 7 + 4

module CoreLibrary =
struct

type 'ext my_extensible_type =
    | Moo of int
    | Ext of 'ext  [@extension_point]
    
end


module SomeExtensionLibrary = 
struct
type my_first_extension =
   | OneConstructor
   | AnotherConstructor  [@@extension_of my_extensible_type] 
end



module type MYFEATURE =
sig
    val myid : int -> int
end [@@feature_decl  ] 


let core_impl (_x [@moo  ] : int)   _y _z = x+2 [@@core_impl MYFEATURE.myid   ] 