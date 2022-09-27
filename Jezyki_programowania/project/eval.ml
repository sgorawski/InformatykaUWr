open Syntax

let foldi f n z =
  let rec aux n acc =
    if n = 0 then acc else aux (n - 1) (f acc)
  in
  aux n z

let cnt = ref 0

let gen_var () = incr cnt; "v" ^ (string_of_int !cnt)

let ctru = TmAbs ("t", TmAbs ("f", TmVar 1))

let cfls = TmAbs ("t", TmAbs ("f", TmVar 0))

let cand = TmAbs ("p", TmAbs ("q", TmApp (TmApp (TmVar 1, TmVar 0), TmVar 1)))

let czero = TmAbs ("s", TmAbs ("z", TmVar 0))

let cscc = TmAbs ("n", TmAbs ("s", TmAbs ("z", TmApp (TmVar 1, TmApp (TmApp (TmVar 2, TmVar 1), TmVar 0)))))

let cpair = let f, s, b = "f", "s", "b" in TmAbs (f, (TmAbs (s, (TmAbs (b, TmApp (TmApp (TmVar 0, TmVar 2), TmVar 1))))))

let cfst = let p, f, s = "p", "f", "s" in TmAbs (p, TmApp (TmVar 0, TmAbs (f, TmAbs (s, TmVar 1))))

let csnd =  let p, f, s = "p", "f", "s" in TmAbs (p, TmApp (TmVar 0, TmAbs (f, TmAbs (s, TmVar 0))))

let zz = TmApp (TmApp (cpair, czero), czero)

let ss =
  let s1 = TmApp (csnd, TmVar 0) in
  let s2 = TmApp (cscc, TmApp (csnd, TmVar 0)) in
  TmAbs ("p", TmApp (TmApp (cpair, s1), s2))

let cpred = TmAbs ("m", TmApp (cfst, TmApp (TmApp (TmVar 0, ss), zz)))

let csub = TmAbs ("x", TmAbs ("y", TmApp (TmApp (TmVar 0, cpred), TmVar 1)))

let ciszro = TmAbs ("n", (TmApp (TmApp (TmVar 0, TmAbs ("x", cfls)), ctru)))

let rec desugar t =
  match t with
  | TmVar _ -> t
  | TmAbs (x, b) -> TmAbs (x, desugar b)
  | TmApp (t1, t2) -> TmApp (desugar t1, desugar t2)
  (* Numbers *)
  | TmNum n ->
    let s, z = "s", "z" in
    let scc cn = TmApp (TmVar 1, cn) in
    TmAbs (s, TmAbs (z, foldi scc n (TmVar 0)))
  | TmAdd (t1, t2) ->
    let m, n, s, z = "m", "n", "s", "z" in
    let cplus = TmAbs (m, TmAbs (n, TmAbs (s, TmAbs (z,
      TmApp (TmApp (TmVar 3, TmVar 1), TmApp (TmApp (TmVar 2, TmVar 1), TmVar 0))
    ))))
    in
    TmApp (TmApp (cplus, desugar t1), desugar t2)
  | TmMul (t1, t2) ->
    let cmul =
      let m, n, s, z = "m", "n", "s", "z" in
      TmAbs (m, TmAbs (n, TmAbs (s, TmAbs (z,
        TmApp (TmApp (TmVar 3, TmApp (TmVar 2, TmVar 1)), TmVar 0
      )))))
    in
    TmApp (TmApp (cmul, desugar t1), desugar t2)
  | TmSub (t1, t2) -> TmApp (TmApp (csub, desugar t1), desugar t2)
  | TmEq (t1, t2) ->
    let cle = TmAbs ("m", TmAbs ("n", TmApp (ciszro, TmApp (TmApp (csub, TmVar 1), TmVar 0)))) in
    let le1 = TmApp (TmApp (cle, TmVar 1), TmVar 0) in
    let le2 = TmApp (TmApp (cle, TmVar 0), TmVar 1) in
    let ceq = TmAbs ("x", (TmAbs ("y", TmApp (TmApp (cand, le1), le2)))) in
    TmApp (TmApp (ceq, desugar t1), desugar t2)
  (* Booleans *)
  | TmTrue -> ctru
  | TmFalse -> cfls
  | TmIf (t1, t2, t3) ->
    let p, t, f = "p", "t", "f" in
    let cif = TmAbs (p, TmAbs (t, TmAbs (f, TmApp (TmApp (TmVar 2, TmVar 1), TmVar 0)))) in
    TmApp (TmApp (TmApp (cif, desugar t1), desugar t2), desugar t3)
  (* Recursion *)
  | TmFix t' ->
      let ycombinator =
        let f, x = "f", "x" in
        let inner = TmAbs (x, TmApp (TmVar 1, TmApp (TmVar 0, TmVar 0))) in
        TmAbs (f, TmApp (inner, inner))
      in
      TmApp (ycombinator, desugar t')
  (* Pairs *)
  | TmPair (t1, t2) -> TmApp (TmApp (cpair, desugar t1), desugar t2)
  | TmFst t' -> TmApp (cfst, desugar t')
  | TmSnd t' -> TmApp (csnd, desugar t')
  (* Lists *)
  | TmNil -> TmAbs ("c", TmAbs ("n", TmVar 0))
  | TmCons (t1, t2) ->
    let ccons =
      let h, t, c, n = "h", "t", "c", "n" in
      let inner = TmApp (TmApp (TmVar 1, TmVar 3), TmApp (TmApp (TmVar 2, TmVar 1), TmVar 0)) in
      TmAbs (h, TmAbs (t, TmAbs (c, TmAbs (n, inner))))
    in
    TmApp (TmApp (ccons, desugar t1), desugar t2)
  | TmHead t' ->
    let chead = TmAbs ("l", TmApp (TmApp (TmVar 0, TmAbs ("h", TmAbs ("t", TmVar 1))), cfls)) in
    TmApp (chead, desugar t')
  | TmTail t' ->
    let ctail =
      let s1 = TmAbs ("h", TmAbs ("t", TmAbs ("g", TmApp (TmApp (TmVar 0, TmVar 2), TmApp (TmVar 1, TmVar 4))))) in
      let s2 = TmAbs ("t", TmVar 1) in
      let s3 = TmAbs ("h", TmAbs ("t", TmVar 0)) in
      TmAbs ("l", TmAbs ("c", TmAbs ("n", TmApp (TmApp (TmApp (TmVar 2, s1), s2), s3))))
    in
    TmApp (ctail, desugar t')
  | TmIsNil t' ->
    let cisnil = TmAbs ("l", TmApp (TmApp (TmVar 0, TmAbs ("h", TmAbs ("t", cfls))), ctru)) in
    TmApp (cisnil, desugar t')

let term_shift d t =
  let rec walk c t =
    match t with
    | TmVar i -> if i >= c then TmVar (i + d) else t
    | TmAbs (x, t1) -> TmAbs (x, walk (c + 1) t1)
    | TmApp (t1, t2) -> TmApp (walk c t1, walk c t2)
    | _ -> failwith "term_shift"
  in
  walk 0 t

let term_subst j s t =
  let rec walk c t =
    match t with
    | TmVar i -> if i = j + c then term_shift c s else t
    | TmAbs (x, t1) -> TmAbs (x, walk (c + 1) t1)
    | TmApp (t1, t2) -> TmApp (walk c t1, walk c t2)
    | _ -> failwith "term_subst"
  in
  walk 0 t

let term_subst_top s t =
  term_shift (-1) (term_subst 0 (term_shift 1 s) t)

let rec eval_cbn t =
  match t with
  | TmVar _ -> t
  | TmAbs _ -> t
  | TmApp (t1, t2) -> begin
    match eval_cbn t1 with
    | TmAbs (x, b) -> eval_cbn (term_subst_top t2 b)
    | t' -> TmApp (t', t2)
  end
  | _ -> failwith "eval_cbn"

let rec eval_no t =
  match t with
  | TmVar _ -> t
  | TmAbs (x, b) -> TmAbs (x, eval_no b)
  | TmApp (t1, t2) -> begin
    match eval_cbn t1 with
    | TmAbs (x, b) -> eval_no (term_subst_top t2 b)
    | n -> TmApp (eval_no n, eval_no t2)
  end
  | _ -> failwith "eval_no"

let normalize t = eval_no (desugar t)

let compare (t1, free_vars1) (t2, free_vars2) =
  let lookup_free_var i free_vars = List.nth free_vars (List.length free_vars + i - 1) in
  let rec cmp t1 t2 =
    let whnf1, whnf2 = eval_cbn t1, eval_cbn t2 in
    match whnf1, whnf2 with
    | TmVar i, TmVar j ->
      if i >= 0 then
        i = j
      else
        (* Assume that free variables are equal if they have the same name in source. *)
        lookup_free_var i free_vars1 = lookup_free_var j free_vars2
    | TmAbs (x, b1), TmAbs (y, b2) -> cmp b1 b2
    | TmApp (t11, t12), TmApp (t21, t22) -> begin
      match t11, t21 with
      | TmAbs (_, b1), TmAbs (_, b2) -> cmp (term_subst_top t12 b1) (term_subst_top t22 b2)
      | _ -> cmp t11 t21 && cmp t12 t22
    end
    | _ -> false
  in
  cmp (desugar t1) (desugar t2)
