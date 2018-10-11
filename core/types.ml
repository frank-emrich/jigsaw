type 'ext_typ core_type =
  | UnitT
  | BoolT
  | IntT
  | StringT
  | Arrow of 'ext_typ core_type * 'ext_typ core_type
  | ExtTyp of 'ext_typ
