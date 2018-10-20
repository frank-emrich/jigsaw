let x = 7 + 4

type foo = unit

module CoreLibrary =
struct

type 'ext first_extensible_type =
    | Foo of int
    | ExtFET of 'ext  [@extension_point]


type ('ext, 'ext_one_extensible_type) second_extensible_type =
   | Bar of int * 'ext_one_extensible_type first_extensible_type
   | ExtSET of 'ext [@extension_point]

end


module SomeExtensionLibrary =
struct
type my_first_extension =
   | OneConstructor
   | AnotherConstructor  [@@extension_of first_extensible_type]

type 'ext_first_extensible_type my_second_extension =
   | FancyConstructor of 'ext_first_extensible_type CoreLibrary.first_extensible_type
   | HelloThere  [@@extension_of second_extensible_type]

end



module type MYFEATURE =
sig
    val myid : int -> int
end [@@feature_decl  ]


let core_impl (_x [@moo  ] : int)   _y _z = x+2 [@@core_impl MYFEATURE.myid   ]