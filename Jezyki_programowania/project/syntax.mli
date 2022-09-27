type term =
  | TmVar of int (* De Bruijn index *)
  | TmAbs of string * term
  | TmApp of term * term
  | TmNum of int
  | TmAdd of term * term
  | TmMul of term * term
  | TmSub of term * term
  | TmEq of term * term
  | TmIf of term * term * term
  | TmTrue
  | TmFalse
  | TmFix of term
  | TmPair of term * term
  | TmFst of term
  | TmSnd of term
  | TmNil
  | TmCons of term * term
  | TmHead of term
  | TmTail of term
  | TmIsNil of term
