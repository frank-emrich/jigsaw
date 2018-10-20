type typ [@@extensible_type]

type 'typ core_typ =
  | UnitT
  | BoolT
  | IntT
  | StringT
  | Arrow of 'typ * 'typ [@@extension_of typ]
