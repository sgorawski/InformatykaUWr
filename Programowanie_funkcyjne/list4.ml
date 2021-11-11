(* Slawomir Gorawski *)

type 'var prop =
  | Var of 'var
  | Not of 'var prop
  | And of 'var prop * 'var prop
  | Or of 'var prop * 'var prop
  | Imp of 'var prop * 'var prop


exception Logic of string

(* 1 *)

let parse_rpn str =
  let len = String.length str in
  let rec prop_of_char stack = function
    | n when n >= len -> begin
      match stack with
      | p :: [] -> p
      | _ -> raise @@ Logic "not a single formula"
    end
    | n -> begin
      match str.[n] with
      | '~' -> begin
        match stack with
        | p :: ps -> prop_of_char ((Not p) :: ps) (n + 1)
        | _ -> raise @@ Logic "cannot apply ~, no vars left"
      end
      | '*' -> begin
        match stack with
        | p :: p' :: ps -> prop_of_char ((And (p', p)) :: ps) (n + 1)
        | _ -> raise @@ Logic "cannot apply *, no vars left"
      end
      | '+' -> begin
        match stack with
        | p :: p' :: ps -> prop_of_char ((Or (p', p)) :: ps) (n + 1)
        | _ -> raise @@ Logic "cannot apply +, no vars left"
      end
      | '>' -> begin
        match stack with
        | p :: p' :: ps -> prop_of_char ((Imp (p', p)) :: ps) (n + 1)
        | _ -> raise @@ Logic "cannot apply >, no vars left"
      end
      | x -> prop_of_char ((Var x) :: stack) (n + 1)
    end
  in prop_of_char [] 0

let rec unparse_rpn = function
  | Var p -> String.make 1 p
  | Not p -> (unparse_rpn p) ^ "~"
  | And (p, p') -> (unparse_rpn p) ^ (unparse_rpn p') ^ "*"
  | Or (p, p') -> (unparse_rpn p) ^ (unparse_rpn p') ^ "+"
  | Imp (p, p') -> (unparse_rpn p) ^ (unparse_rpn p') ^ ">"

(* 2 *)

(* TODO *)

(* 3 *)

type 'var lit =
  | Pos of 'var
  | Neg of 'var

type 'var nnf =
  | LitNNF of 'var lit
  | AndNNF of 'var nnf * 'var nnf
  | OrNNF of 'var nnf * 'var nnf

let nnf_of_prop p =
  let rec t = function
    | Var p -> LitNNF (Pos p)
    | Or (p, p') -> OrNNF (t p, t p')
    | And (p, p') -> AndNNF (t p, t p')
    | Imp (p, p') -> OrNNF (f p, t p')
    | Not p -> f p
  and f = function
    | Var p -> LitNNF (Neg p)
    | Or (p, p') -> AndNNF (f p, f p')
    | And (p, p') -> OrNNF (f p, f p')
    | Imp (p, p') -> AndNNF (t p, f p')
    | Not p -> t p
  in t p

(* 4 *)

type 'var clause = 'var lit list

type 'var cnf = 'var clause list

let cnf_of_prop (p : 'var prop) : 'var cnf =
  let rec cnf_of_nnf = function
    | LitNNF p -> [[p]]
    | AndNNF (p, p') -> (cnf_of_nnf p) @ (cnf_of_nnf p')
    | OrNNF (p, p') ->
        let cnf_p, cnf_p' = cnf_of_nnf p, cnf_of_nnf p' in
        cnf_p |> List.map (fun p -> List.map (( @ ) p) cnf_p') |> List.concat
  in nnf_of_prop p |> cnf_of_nnf

(* 5 *)

let var_of_prop p =
  let rec aux acc = function
    | Var p -> if List.exists (( == ) p) acc then acc else p :: acc
    | Or (p, p') -> let half = aux acc p in aux half p'
    | And (p, p') -> let half = aux acc p in aux half p'
    | Imp (p, p') -> let half = aux acc p in aux half p'
    | Not p -> aux acc p
  in aux [] p

(* 6 *)

let rec subst_prop f = function
  | Var x -> f x
  | Not x -> Not (subst_prop f x)
  | And (x, x') -> And (subst_prop f x, subst_prop f x')
  | Or (x, x') -> Or (subst_prop f x, subst_prop f x')
  | Imp (x, x') -> Imp (subst_prop f x, subst_prop f x')

(* 7 *)

let rec valuation f = function
  | Var x -> f x
  | Not x -> not @@ valuation f x
  | And (x, x') -> valuation f x && valuation f x'
  | Or (x, x') -> valuation f x || valuation f x'
  | Imp (x, x') -> not (valuation f x) || valuation f x'

(* 8 *)

type varval = (char * bool) list

exception Unvalued of char

let rec getval (vv : varval) x =
  match vv with
  | [] -> raise @@ Unvalued x
  | (v, vl) :: vs -> if x = v then vl else getval vs x

(* 9 *)

let nextval (vv : varval) : varval option =
  let rec aux acc = function
    | [] -> None
    | (v, vl) :: vs -> begin
      match vl with
      | false -> Some (List.rev_append acc @@ (v, true) :: vs)
      | true -> aux ((v, false) :: acc) vs
    end
  in aux [] vv

(* 10 *)

let sat_prop p =
  let vars = var_of_prop p in
  let vv = vars |> List.map @@ fun v -> (v, false) in
  let rec is_sat = function
    | None -> false
    | Some vv ->
        if p |> valuation @@ getval vv
        then true
        else is_sat @@ nextval vv
  in is_sat @@ Some vv

(* 11 *)

type 'a btree =
  | Leaf
  | Node of 'a btree * 'a * 'a btree

type 'a mtree = Mtree of 'a * 'a forest
and 'a forest = 'a mtree list

let bprefix t =
  let rec aux acc = function
    | Leaf -> acc
    | Node (l, v, r) -> let half = aux (v :: acc) l in aux half r
  in aux [] t

let mprefix t =
  let rec aux acc = function
    | Mtree (v, []) -> v :: acc
    | Mtree (v, frst) -> v :: List.fold_left aux acc frst
  in aux [] t

(* 12 *)

(* TODO *)

(* 13 *)

let rec const_tree v = function
  | 0 -> Leaf
  | n when n mod 2 = 1 -> let t = const_tree v (n / 2) in Node (t, v, t)
  | n -> let l = const_tree v (n / 2 - 1) in let r = const_tree v (n / 2) in Node (l, v, r)

(* 14 *)

(* TODO *)
