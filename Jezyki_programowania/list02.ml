(* 4 *)

type term =
  | TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term

let rec eval (t : term) : term =
  match t with
  | TmIf (t1, t2, t3) -> begin
    match eval t1 with
    | TmTrue -> eval t2
    | TmFalse -> eval t3
    | _ -> invalid_arg "Evaluation stuck"
  end
  | TmSucc t' -> TmSucc (eval t')
  | TmPred t' -> begin
    match eval t' with
    | TmZero -> TmZero
    | TmSucc nv -> nv
    | _ -> invalid_arg "Evaluation stuck"
  end
  | TmIsZero t' -> begin
    match eval t' with
    | TmZero -> TmTrue
    | TmSucc _ -> TmFalse
    | _ -> invalid_arg "Evaluation stuck"
  end
  | _ -> t
