type arith_typ =
  | IntT



type arith_value =
  | IntV of int



type 'term arith_term =
  | IntE of int
  | PlusE of 'term * 'term
  | IntEq of 'term * 'term


