(* Slawomir Gorawski *)

(* 1 *)

let rec ins_everywhere x lst =
  match lst with (* its better this way trust me *)
  | [] -> [[x]]
  | h :: tl -> (x :: lst) :: (List.map (List.cons h) (ins_everywhere x tl))

let ins_everywhere_tl x lst =
  let rec aux acc nxt = function
    | [] -> acc
    | h :: tl -> aux ((x :: h :: nxt) :: (List.map (List.cons h) acc)) (h :: nxt) tl
  in aux [[x]] [] (List.rev lst)

(* 2 *)

let rec iperm = function
  | [] -> [[]]
  | h :: tl -> List.concat (List.map (ins_everywhere h) (iperm tl))

(* 3 *)

let rec ins_ord x = function
  | [] -> [x]
  | h :: tl -> if x <= h then x :: h :: tl else h :: (ins_ord x tl)

let rec isort = function
  | [] -> []
  | h :: tl -> ins_ord h (isort tl)

(* 4 *)

let ins_ord_tl x lst =
  let rec aux acc = function
    | [] -> acc
    | h :: tl -> begin
      match acc with
      | [] -> raise (Failure "empty acc")
      | ah :: atl -> aux (if h > ah then ah :: h :: atl else h :: ah :: atl) tl
    end
  in aux [x] (List.rev lst)

let isort_tl lst =
  let rec aux acc = function
    | [] -> acc
    | h :: tl -> aux (ins_ord_tl h acc) tl
  in aux [] (List.rev lst)

(* 5 *)

let rec sel_anything = function
  | [] -> []
  | h :: tl -> (h, tl) :: List.map (fun (x, lst) -> (x, h :: lst)) (sel_anything tl)

(* 6 *)

let rec sperm = function
  | [] -> [[]]
  | lst ->
      List.concat (List.map (fun (x, lst) -> List.map (List.cons x) (sperm lst)) (sel_anything lst))

(* 7 *)

let rec sel_min = function
  | [] -> raise (Failure "cannot select min from empty list")
  | [x] -> (x, [])
  | h :: tl -> let mh, mtl = (sel_min tl) in if h < mh then h, mh :: mtl else mh, h :: mtl

let rec ssort = function
  | [] -> []
  | lst -> let mh, mrst = sel_min lst in mh :: (ssort mrst)

(* 8 *)

let sel_min_tl lst =
  let rec aux acc = function
    | [] -> acc
    | h :: tl -> let am, arst = acc in aux (if h < am then h, am :: arst else am, h :: arst) tl
  in match lst with
  | [] -> raise (Failure "cannot select min from empty list")
  | h :: tl -> aux (h, []) tl

let ssort_tl lst =
  let rec aux acc = function
    | [] -> acc
    | lst -> let mh, mrst = sel_min_tl lst in aux (mh :: acc) mrst
  in List.rev (aux [] lst)

(* 9 *)

let rec monotone = function
  | [] -> true
  | [x] -> true
  | h1 :: h2 :: tl -> (h1 <= h2) && (monotone (h2 :: tl))

let perm_sort gen = function
  | [] -> []
  | lst -> let rec pick_sorted = function
    | [] -> raise (Failure "no sorted permutation")
    | h :: tl ->  if monotone h then h else pick_sorted tl
  in pick_sorted (gen lst)

(* 10 *)

let rec split = function
  | [] -> ([], [])
  | [x] -> ([], [x])
  | h1 :: h2 :: tl -> let tl1, tl2 = split tl in (h1 :: tl1, h2 :: tl2)

(* TODO real split, not even/odd *)

(* 11 *)

let rec ( <+> ) lst1 lst2 =
  match lst1, lst2 with
  | l, [] -> l
  | [], l -> l
  | (h1 :: tl1), (h2 :: tl2) ->
      if h1 < h2
      then h1 :: (tl1 <+> (h2 :: tl2))
      else h2 :: ((h1 :: tl1) <+> tl2)

let rec msort = function
  | [] -> []
  | [x] -> [x]
  | lst -> let l, r = split lst in (msort l) <+> (msort r)

(* 12 *)

let rev_merge cmp l r =
  let rec aux acc l r =
    match l, r with
    | [], [] -> acc
    | [], h :: tl -> aux (h :: acc) l tl
    | h :: tl, [] -> aux (h :: acc) tl r
    | h1 :: tl1, h2 :: tl2 ->
        if cmp h1 h2
        then aux (h1 :: acc) tl1 (h2 :: tl2)
        else aux (h2 :: acc) (h1 :: tl1) tl2
  in aux [] l r

(* 13 *)

let rec msort2 = function
  | [] -> []
  | [x] -> [x]
  | lst -> let l, r = split lst in List.rev (rev_merge ( <= )  (msort2 l) (msort r))

(* 14 *)

let rec msort_down = function
  | [] -> []
  | [x] -> [x]
  | lst -> let l, r = split lst in rev_merge ( > ) (msort_up l) (msort_up r)
and msort_up = function
  | [] -> []
  | [x] -> [x]
  | lst -> let l, r = split lst in rev_merge ( <= ) (msort_down l) (msort_down r)
