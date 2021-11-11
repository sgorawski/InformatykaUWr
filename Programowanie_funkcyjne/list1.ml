(* Slawomir Gorawski *)

(* 1 *)

let t1_0 x = x + 0

let t1_1 f g x = f (g x)

let rec t1_2 f x = f x

let t1_3 x _ = x

let t1_4 = max

let t1_x _ = raise (Failure "err")

(* 2 *)

let rec f1 n =
  if n == 0 then 0
  else 2 * (f1 (n - 1)) + 1

let f2 n =
  let rec aux n acc =
    if n == 0 then acc
    else aux (n - 1) (2 * acc + 1)
  in aux n 0

(* 3 *)

let ( <.> ) f g x = f (g x)

let ( <^> ) f n x =
  let rec aux n acc =
    if n == 0 then acc
    else aux (n - 1) (f <.> acc)
  in aux n (fun x -> x) x

let ( <*> ) x y =
  ((( + ) x) <^> y) 0

let ( <**> ) x y =
  ((( * ) x) <^> y) 1

(* 4 *)

let hd s = s 0

let tl s x = s (x + 1)

let add c s x = (s x) + c

let map f s x = f (s x)

let map2 f s1 s2 x = f (s1 x) (s2 x)

let replace n a s x = if x mod n == 0 then a else (s x)

let take n s x = s (n * x)

let scan f a s x =
  let rec aux i acc =
    if i >= x then acc
    else aux (x + 1) (f acc (s x))
   in aux 0 a

let tabulate s ?(b = 0) e =
  let rec aux e acc =
    if e < b then acc
    else aux (e - 1) ((s e) :: acc)
  in aux e []

(* 5 *)

let ctrue x _ = x

let cfalse _ y = y

let cand cbl cbr = cbl cbr cfalse

let cor cbl cbr = cbl ctrue cbr

let cbool_of_bool b x y = if b then x else y

let bool_of_cbool cb = cb true false

(* 6 *)

let zero _ x = x

let succ n f x = f (n f x)

let add m n f x = m f (n f x)

let mul m n f = m (n f)

let is_zero n = n (fun _ -> false) true

let cnum_of_int n =
  let rec aux n acc =
    if n == 0 then acc
    else aux (n - 1) (succ acc)
  in aux n zero

let int_of_cnum cn =
  cn (( + ) 1) 0
