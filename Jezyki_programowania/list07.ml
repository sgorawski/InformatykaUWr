(* 5 *)

type var = string

type term =
  | Var of var
  | Lam of var * term
  | App of term * term

type lazy_form = Closure of var * term * env
and env = lazy_form list

let lookup_env x = List.find (fun (Closure (v, _, _)) -> v = x)

let rec eval_cbn t e =
  match t with
  | Var x -> lookup_env x e
  | Lam (x, t) -> Closure (x, t, e)
  | App (t1, t2) ->
    let Closure (v, b, e') = eval_cbn t1 e in
    eval_cbn b (Closure (v, t2, e) :: e')
