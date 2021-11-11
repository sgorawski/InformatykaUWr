(* Slawomir Gorawski *)

(* 2 *)

(* TODO *)

(* 3 *)

(* TODO *)

(* 4 *)

let cyclist = function
  | [] -> invalid_arg "empty list"
  | x :: xs ->
      let s = Streams.stream_of_list xs in
      let rec cl = lazy (Streams.Cons (x, Streams.(++) s cl)) in cl

(* 5 *)

type 'a two_way_stream = 'a twcell Lazy.t
  and 'a twcell = TWCons of 'a two_way_stream * 'a * 'a two_way_stream

let tws_next (t : 'a two_way_stream) =
  let TWCons (_, _, n) = Lazy.force t in n

let tws_prev (t : 'a two_way_stream) =
  let TWCons (p, _, _) = Lazy.force t in p

let tws_elem (t : 'a two_way_stream) =
  let TWCons (_, e, _) = Lazy.force t in e

(* 6 *)

(* hard *)

(* 7 *)

let tws_init f =
  let rec to_right prev i =
    let rec this =
      lazy (TWCons (prev, f i, to_right this (i + 1))) in this
  in let rec to_left next i =
    let rec this =
      lazy (TWCons (to_left this (i - 1), f i, next)) in this
  in let rec keystone =
      lazy (TWCons (to_left keystone (-1), f 0, to_right keystone 1)) in keystone

(* 8 *)

(* TODO *)
