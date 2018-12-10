let debug = false

type var = string

type 'value venv = (var * 'value) list

type 'typ tenv = (var * 'typ) list

exception TypeError of string
