let tru t f = t

let fls t f = f

let and' b c = b c fls

let test b t e = b t e

let cbool_of_bool p t f = if p then t else f

let bool_of_cbool cb = cb true false

let zero s z = z

let scc n s z = s (n s z)

let plus m n s z = m s (n s z)

let iszro m = m (fun _ -> fls) tru

let cnum_of_int n =
  let rec aux n acc =
    if n = 0 then acc else aux (n - 1) (scc acc)
  in aux n zero

let int_of_cnum cn = cn (( + ) 1) 0

let pair f s b = b f s

let fst p = p tru

let snd p = p fls

(* 1 *)

(* TAPL 5.2.2 *)

let scc' n s z = n s (s z)

(* TAPL 5.2.3 *)

let times m n s = m (n s)

(* TAPL 5.2.4 *)

let pow m n = n m

(* TAPL 5.2.5 *)

(* Could be without b, this is for OCaml *)
let zz b = pair zero zero b

let ss p = pair (snd p) (scc (snd p))

let prd m = fst (m ss zz)

let sub m n = n prd m

(* TAPL 5.2.6 *)

(* O(n) *)
(* TAPL:
Evaluating prd cn takes O(n) steps,
since prd uses n to construct a sequence of n pairs of numbers
and then selects the first component of the last pair of the sequence.
*)

(* TAPL 5.2.7 *)

(*
let equal m n = and' (iszro (sub m n)) (iszro (sub n m))
*)

(* 2 *)

(* TAPL 5.2.8 *)

let nil c n = n

let cons h t c n = c h (t c n)

let isnil l = l (fun _ _ -> fls) tru

let head l = l (fun h _ -> h) nil

let tail l c n = l (fun h t g -> g h (t c)) (fun _ -> n) nil

let clist_of_list xs = List.fold_right cons xs nil

let list_of_clist cxs = cxs List.cons []

(* TAPL 5.2.11 *)

(*
let fix f = (fun x -> f (fun y -> x x y)) (fun x -> f (fun y -> x x y))

let ff f l = test (isnil l) (fun _ -> zero) (fun _ -> (plus (head l) (f (tail l)))) zero

let sumlist = fix ff
*)

(* 3 *)

(* TAPL 5.3.8 *)

type term =
  | TmVar of int
  | TmAbs of term
  | TmApp of term * term

let isval t =
  match t with
  | TmAbs _ -> true
  | _ -> false

let termShift d t =
  let rec walk c t = match t with
  | TmVar x -> if x >= c then TmVar (x + d) else TmVar x
  | TmAbs t1 -> TmAbs (walk (c + 1) t1)
  | TmApp (t1, t2) -> TmApp (walk c t1, walk c t2)
  in walk 0 t

let termSubst j s t =
  let rec walk c t = match t with
  | TmVar x -> if x = j + c then termShift c s else TmVar x
  | TmAbs t1 -> TmAbs (walk (c + 1) t1)
  | TmApp (t1, t2) -> TmApp (walk c t1, walk c t2)
  in walk 0 t

let termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)

let rec eval t =
  match t with
  | v when isval v -> v
  | TmApp (t1, t2) ->
    let t1' = eval t1 in
    let v = eval t2 in
    if isval t1' && isval v
    then eval (termSubstTop v t1')
    else t
  | _ -> failwith "whatever"
