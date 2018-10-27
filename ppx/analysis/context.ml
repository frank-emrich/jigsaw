(* TODO: move registration functions here, potentially also the persisting *)


type injection_arg_element =
  | InjectionType of string
  | InjectionLift of string * string * string (* function name, overall, extension *)
  | InjectionUnlift of string * string * string (* function name, overall, extension *)
  | InjectionFeatureFunction of string * string * string (* function name, feature name, feature function name *)
    [@@deriving show]


type module_path_element =
  | ModulePathPlain of string
  | ModulePathFunctor of string
  | ModulePathInjectionFunctor of string * injection_arg_element list 
    [@@deriving show]


let current_module : module_path_element list ref = ref []

let push_plain_module name =
  current_module := (ModulePathPlain name) :: !current_module

let push_functor name =
  current_module := (ModulePathFunctor name) :: !current_module

let push_injection_functor name elements = 
  current_module := ((ModulePathInjectionFunctor (name, elements)) :: !current_module)

let pop_module () = 
  current_module := List.tl !current_module 