type token =
  | TokenLParen
  | TokenRParen
  | TokenLambda of char
  | TokenVar of char

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let is_whitespace c = c = ' ' || c = '\t' || c = '\n'

let lex prog =
  let rec aux acc chars =
    match chars with
    | '(' :: rest -> aux (TokenLParen :: acc) rest
    | ')' :: rest -> aux (TokenRParen :: acc) rest
    | '/' :: v :: '.' :: rest -> aux (TokenLambda v :: acc) rest
    | v :: rest when is_whitespace v -> aux acc rest
    | v :: rest -> aux (TokenVar v :: acc) rest
    | [] -> acc
  in List.rev (aux [] (explode prog))

type term =
  | TmVar of char
  | TmAbs of char * term
  | TmApp of term * term

let string_of_char c = String.make 1 c

let rec string_of_term t =
  match t with
  | TmVar v -> string_of_char v
  | TmAbs (v, t) -> "/" ^ string_of_char v ^ "." ^ string_of_term t
  | TmApp (TmVar v1, TmVar v2) -> string_of_char v1 ^ string_of_char v2
  | TmApp (TmVar v1, t2) -> string_of_char v1 ^ "(" ^ string_of_term t2 ^ ")"
  | TmApp (t1, TmVar v2) -> "(" ^ string_of_term t1 ^ ")" ^ string_of_char v2
  | TmApp (t1, t2) -> "(" ^ string_of_term t1 ^ ")(" ^ string_of_term t2 ^ ")"

let split_on_parens tokens =
  let rec aux acc nesting_level tokens =
    match tokens with
    | TokenLParen :: rest -> aux (TokenLParen :: acc) (nesting_level + 1) rest
    | TokenRParen :: rest ->
      if nesting_level = 1 then acc, tokens else aux (TokenRParen :: acc) (nesting_level - 1) rest
    | token :: rest -> aux (token :: acc) nesting_level rest
    | _ -> invalid_arg "Syntax error"
  in let fp, sp = aux [] 0 tokens in
  match (List.rev fp), sp with
  | _ :: fp', _ :: sp' -> fp', sp'
  | _ -> invalid_arg "Syntax error"

let rec parse tokens =
  match tokens with
  | TokenVar v :: [] -> TmVar v
  | TokenLambda v :: rest -> TmAbs (v, parse rest)
  | _ -> begin
    match parse_app_seq tokens with
    | t :: rest -> List.fold_left (fun t1 t2 -> TmApp (t1, t2)) t rest
    | [] -> invalid_arg "Syntax error"
  end
and parse_app_seq tokens =
  (* Note: this can be tail-recursive *)
  match tokens with
  | TokenVar v :: rest -> TmVar v :: parse_app_seq rest
  | TokenLParen :: _ ->
    let first_part, second_part = split_on_parens tokens in
    parse first_part :: parse_app_seq second_part
  | [] -> []
  | _ -> invalid_arg "Syntax error"

exception NoRuleApplies

let is_val t =
  match t with
  | TmAbs _ -> true
  | _ -> false

(** Substitutes [c] with [t'] in term [t] *)
let rec subst t c t' =
  match t with
  | TmVar v when v = c -> t'
  | TmAbs (v, b) -> TmAbs (v, subst b c t')
  | TmApp (t1, t2) -> TmApp (subst t1 c t', subst t2 c t')
  | _ -> t

let rec eval1 t =
  match t with
  | TmApp (TmAbs (c, t), v) when is_val v -> subst t c v
  | TmApp (v, t2) when is_val v -> TmApp (v, eval1 t2)
  | TmApp (t1, t2) -> TmApp (eval1 t1, t2)
  | _ -> raise NoRuleApplies

let rec eval t =
  try let t' = eval1 t in eval t'
  with NoRuleApplies -> t

(** Run-eval-print *)
let rep prog =
  prog |> lex |> parse |> eval |> string_of_term

(* Using De Bruijn indices *)

let find e l =
  let rec aux i l' =
    match l' with
    | h :: _ when h = e -> i
    | _ :: t -> aux (i + 1) t
    | _ -> failwith "Not found"
  in aux 0 l

type termdb =
  | TmDBVar of int
  | TmDBAbs of char * termdb
  | TmDBApp of termdb * termdb

let remove_names t =
  let rec aux ctx t' =
    match t' with
    | TmVar v -> TmDBVar (find v ctx)
    | TmAbs (v, b) -> TmDBAbs (v, aux (v :: ctx) b)
    | TmApp (t1, t2) -> TmDBApp (aux ctx t1, aux ctx t2)
  in aux [] t

let restore_names t =
  let rec aux ctx t' =
    match t' with
    | TmDBVar i -> TmVar (List.nth ctx i)
    | TmDBAbs (v, b) -> TmAbs (v, aux (v :: ctx) b)
    | TmDBApp (t1, t2) -> TmApp (aux ctx t1, aux ctx t2)
  in aux [] t

let is_valdb t =
  match t with
  | TmDBAbs _ -> true
  | _ -> false

let termShift d t =
  let rec walk c t = match t with
  | TmDBVar x -> if x >= c then TmDBVar (x + d) else TmDBVar x
  | TmDBAbs (v, t1) -> TmDBAbs (v, walk (c + 1) t1)
  | TmDBApp (t1, t2) -> TmDBApp (walk c t1, walk c t2)
  in walk 0 t

let termSubst j s t =
  let rec walk c t = match t with
  | TmDBVar x -> if x = j + c then termShift c s else TmDBVar x
  | TmDBAbs (v, t1) -> TmDBAbs (v, walk (c + 1) t1)
  | TmDBApp (t1, t2) -> TmDBApp (walk c t1, walk c t2)
  in walk 0 t

let termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)

let rec eval1db t =
  match t with
  | TmDBApp (TmDBAbs (_, t), v) when is_valdb v -> termSubstTop v t
  | TmDBApp (v, t2) when is_valdb v -> TmDBApp (v, eval1db t2)
  | TmDBApp (t1, t2) -> TmDBApp (eval1db t1, t2)
  | _ -> raise NoRuleApplies

let rec evaldb t =
  try let t' = eval1db t in evaldb t'
  with NoRuleApplies -> t

let repdb prog =
  prog |> lex |> parse |> remove_names |> evaldb |> restore_names |> string_of_term
