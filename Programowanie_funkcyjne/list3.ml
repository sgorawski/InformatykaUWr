(* Slawomir Gorawski *)

(* 1 *)

let atoi_tr xs =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (acc * 10 + int_of_char x - 48) xs
  in aux 0 xs

let atoi =
  List.fold_left (fun dec x -> dec * 10 + int_of_char x - 48) 0

(* 2 *)

let polynomial_tr poly x0 =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (acc *. x0 +. x) xs
  in aux 0.0 poly

let polynomial poly x0 =
  List.fold_left (fun value x -> value *. x0 +. x) 0.0 poly

(* 3 *)

let fold_right_tr f xs c =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (f x acc) xs
  in aux c @@ List.rev xs

let fold_right' f xs c =
  List.fold_left (fun res x -> f x res) c @@ List.rev xs

(* 4 *)

let ins_everywhere_fr el xs =
  List.fold_right
  (fun x (prev, orig) -> ((el :: x :: orig) :: List.map (List.cons x) prev, x :: orig))
  xs
  ([[el]], [])
  |> fst

let ins_everywhere_fl el xs =
  List.fold_left
  (fun (prev, orig) x -> ((el :: x :: orig) :: List.map (List.cons x) prev, x :: orig))
  ([[el]], [])
  (List.rev xs)
  |> fst

(* 5 *)

let rec polynomial_rev poly x0 =
  match poly with
  | [] -> 0.0
  | x :: xs -> x +. x0 *. (polynomial_rev xs x0)

let polynomial_rev_fr poly x0 =
  List.fold_right (fun x value -> x +. x0 *. value) poly 0.0

let polynomial_rev_tr poly x0 =
  let rec aux acc pow = function
    | [] -> acc
    | x :: xs -> aux (acc +. x *. pow) (x0 *. pow) xs
  in aux 0.0 1.0 poly

let polynomial_rev_fl poly x0 =
  List.fold_left
  (fun (value, pow) x -> (value +. x *. pow, x0 *. pow))
  (0.0, 1.0)
  poly
  |> fst

(* 6 *)

let iperm_fr xs =
  List.fold_right
  (fun x perms -> perms |> List.map @@ ins_everywhere_fr x |> List.concat)
  xs
  [[]]

let iperm_fl xs =
  List.fold_left
  (fun perms x -> perms |> List.map @@ ins_everywhere_fl x |> List.concat)
  [[]]
  (List.rev xs)

(* 7 *)

let ins_ord_fr el xs =
  List.fold_right
  (fun x (h :: tl) -> if x > h then h :: x :: tl else x :: h :: tl)
  xs
  [el]

let ins_ord_fl el xs =
  List.fold_left
  (fun (h :: tl) x -> if x > h then h :: x :: tl else x :: h :: tl)
  [el]
  (List.rev xs)

let isort_fr xs = List.fold_right ins_ord_fr xs []

let isort_fl =
  List.fold_left
  (fun sorted x -> ins_ord_fl x sorted)
  []

(* 8 *)

(* TODO *)

(* 9 *)

let rec sublist = function
  | [] -> [[]]
  | x :: xs -> let subs = sublist xs in subs @ List.map (List.cons x) subs

let sublist_f xs =
  List.fold_right
  (fun x subs -> subs @ List.map (List.cons x) subs)
  xs
  [[]]

(* 10 *)

type 'a mtx = 'a list list

exception Mtx of string

type dim = { rows : int; columns : int }

let mtx_dim (m : 'a mtx) =
  match m with
  | [] -> raise @@ Mtx "empty matrix"
  | [] :: _ -> raise @@ Mtx "empty row"
  | x :: xs ->
    let r = List.length m in
    let c = List.hd m |> List.length in
    if List.exists (fun x -> c <> List.length x) xs
    then raise @@ Mtx "different row lengths"
    else { rows=r; columns=c }

(* 11 *)

let rec mtx_row ~row (m : 'a mtx) =
  try
    List.nth m @@ row - 1
  with exn -> raise @@ Mtx "wrong args"

let rec mtx_column ~column (m : 'a mtx) =
  try
    List.map (fun xs -> List.nth xs @@ column - 1) m
  with exn -> raise @@ Mtx "wrong args"

let rec mtx_elem ~column ~row (m : 'a mtx) =
  try
    List.nth (mtx_row row m) (column - 1)
  with exn -> raise @@ Mtx "wrong args"

(* 12 *)

let rec transpose (m : 'a mtx) : 'a mtx =
  try
    match m with
    | [] -> []
    | [] :: xss -> transpose xss
    | (x :: xs) :: xss ->
        (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)
  with exn -> raise @@ Mtx "wrong args"

(* 13 *)

let mtx_add (m1 : float mtx) (m2 : float mtx) : float mtx =
  try
    List.map2 (List.map2 ( +. )) m1 m2
  with exn -> raise @@ Mtx "wrong args"

(* 14 *)

let scalar_prod v1 v2 =
  List.map2 ( *. ) v1 v2 |> List.fold_left ( +. ) 0.0

let polynomial_alt poly x0 =
  let pows =
    List.fold_right (fun _ (x :: xs) -> x0 *. x :: x :: xs) poly [1.0]
  in scalar_prod pows poly

(* 15 *)

let mtx_apply (m : float mtx) v =
  List.map (scalar_prod v) m

let mtx_mul (m1 : float mtx) (m2 : float mtx) : float mtx =
  transpose m2 |> List.map @@ mtx_apply m1 |> transpose

(* 16 *)

(* TODO *)
