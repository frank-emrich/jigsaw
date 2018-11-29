type var = string [@@deriving show]

type 'value venv = (var * 'value) list [@@deriving show]

type 'typ tenv = (var * 'typ) list [@@deriving show]

exception TypeError of string