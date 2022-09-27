open Syntax

let string_of_term free_vars =
  let rec string_of_expr ctx t =
    match t with
    | TmAbs (x, t1) -> "\\" ^ x ^ ". " ^ (string_of_expr (x :: ctx) t1)
    | TmApp (t1, t2) -> (string_of_atom ctx t1) ^ " " ^ (string_of_atom ctx t2)
    | TmAdd (t1, t2) -> (string_of_atom ctx t1) ^ " + " ^ (string_of_atom ctx t2)
    | TmMul (t1, t2) -> (string_of_atom ctx t1) ^ " * " ^ (string_of_atom ctx t2)
    | TmSub (t1, t2) -> (string_of_atom ctx t1) ^ " - " ^ (string_of_atom ctx t2)
    | TmEq (t1, t2) -> (string_of_atom ctx t1) ^ " = " ^ (string_of_atom ctx t2)
    | TmIf (t1, t2, t3) -> "if " ^ (string_of_expr ctx t1) ^ " then " ^ (string_of_expr ctx t2) ^ " else " ^ (string_of_expr ctx t3)
    | TmFix t1 -> "fix " ^ (string_of_expr ctx t1)
    | TmPair (t1, t2) -> (string_of_atom ctx t1) ^ ", " ^ (string_of_expr ctx t2)
    | TmFst t1 -> "fst " ^ (string_of_expr ctx t1)
    | TmSnd t1 -> "snd " ^ (string_of_expr ctx t1)
    | TmCons (t1, t2) -> (string_of_atom ctx t1) ^ " :: " ^ (string_of_expr ctx t2)
    | TmHead t1 -> "head " ^ (string_of_expr ctx t1)
    | TmTail t1 -> "tail " ^ (string_of_expr ctx t1)
    | TmIsNil t1 -> "isnil " ^ (string_of_expr ctx t1)
    | _ -> string_of_atom ctx t
  and string_of_atom ctx t =
    match t with
    | TmVar i ->
      if i >= 0 then
        List.nth ctx i
      else
        List.nth free_vars (List.length free_vars + i - 1)
    | TmNum n -> string_of_int n
    | TmTrue -> "true"
    | TmFalse -> "false"
    | TmNil -> "[]"
    | _ -> "(" ^ (string_of_expr ctx t) ^ ")"
  in
  string_of_expr []
