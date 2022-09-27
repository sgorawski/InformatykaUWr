type ty =
  | TyBool
  | TyArr of ty * ty
  | TyVar of string

let rec string_of_ty t =
  match t with
  | TyBool -> "Bool"
  | TyArr (TyBool, t2) -> "Bool -> " ^ string_of_ty t2
  | TyArr (TyVar v, t2) -> v ^ " -> " ^ string_of_ty t2
  | TyArr (t1, t2) -> "(" ^ string_of_ty t1 ^ ") -> " ^ string_of_ty t2
  | TyVar v -> v

type term =
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmTrue
  | TmFalse
  | TmIf of term * term * term

let rec string_of_term t =
  match t with
  | TmVar v -> v
  | TmAbs (v, typ, t) -> "/" ^ v ^ ":" ^ string_of_ty typ ^ "." ^ string_of_term t
  | TmApp (TmVar v1, TmVar v2) -> v1 ^ v2
  | TmApp (TmVar v1, t2) -> v1 ^ "(" ^ string_of_term t2 ^ ")"
  | TmApp (t1, TmVar v2) -> "(" ^ string_of_term t1 ^ ")" ^ v2
  | TmApp (t1, t2) -> "(" ^ string_of_term t1 ^ ")(" ^ string_of_term t2 ^ ")"
  | TmTrue -> "true"
  | TmFalse -> "false"
  | TmIf (t1, t2, t3) -> "if " ^ string_of_term t1 ^ " then " ^ string_of_term t2 ^ " else " ^ string_of_term t3

module Parser = struct
  type token =
    | TokenLParen
    | TokenRParen
    | TokenLambda
    | TokenVar of string
    | TokenTrue
    | TokenFalse
    | TokenIf
    | TokenThen
    | TokenElse
    | TokenColon
    | TokenDot
    | TokenTyBool
    | TokenTyArr

  let split_ann tokens =
    let rec aux acc tokens =
      match tokens with
      | TokenColon :: rest -> List.rev rest, acc
      | token :: rest -> aux (token :: acc) rest
      | [] -> invalid_arg "No colon found"
    in aux [] (List.rev tokens)

  let find_matching opening closing tokens =
    let rec aux acc nesting_level tokens =
      match tokens with
      | token :: rest when token = opening -> aux (token :: acc) (nesting_level + 1) rest
      | token :: rest when token = closing ->
        if nesting_level = 1 then acc, tokens else aux (token :: acc) (nesting_level - 1) rest
      | token :: rest -> aux (token :: acc) nesting_level rest
      | _ -> invalid_arg "Syntax error"
    in let fp, sp = aux [] 0 tokens in
    (List.rev fp), sp

  let break_up_if tokens =
    let if_part, rest = find_matching TokenIf TokenThen tokens in
    let then_part, else_part = find_matching TokenThen TokenElse rest in
    match if_part, then_part, else_part with
    | _ :: f, _ :: t, _ :: e -> f, t, e
    | _ -> invalid_arg "Syntax error"

  let split_on_parens tokens =
    let fp, sp = find_matching TokenLParen TokenRParen tokens in
    match fp, sp with
    | _ :: fp', _ :: sp' -> fp', sp'
    | _ -> invalid_arg "Syntax error"

  let split_lambda tokens =
    let fp, sp = find_matching TokenColon TokenDot tokens in
    match fp, sp with
    | _ :: fp', _ :: sp' -> fp', sp'
    | _ -> invalid_arg "Syntax error"

  let rec parse_type tokens =
    match tokens with
    | TokenTyBool :: [] -> TyBool
    | TokenVar tv :: [] -> TyVar tv
    | TokenTyBool :: TokenTyArr :: rest -> TyArr (TyBool, parse_type rest)
    | TokenVar tv :: TokenTyArr :: rest -> TyArr (TyVar tv, parse_type rest)
    | TokenLParen :: _ -> begin
      let first_part, second_part = split_on_parens tokens in
      match second_part with
      | TokenTyArr :: rest -> TyArr (parse_type first_part, parse_type rest)
      | [] -> parse_type first_part
      | _ -> invalid_arg "Unrecognized type"
    end
    | _ -> invalid_arg "Unrecognized type"


  let rec parse tokens =
    match tokens with
    | TokenTrue :: [] -> TmTrue
    | TokenFalse :: [] -> TmFalse
    | TokenIf :: _ ->
      let if_tokens, then_tokens, else_tokens = break_up_if tokens in
      TmIf (parse if_tokens, parse then_tokens, parse else_tokens)
    | TokenVar v :: [] -> TmVar v
    | TokenLambda :: TokenVar v :: rest ->
      let type_part, body_part = split_lambda rest in
      TmAbs (v, parse_type type_part, parse body_part)
    | _ -> begin
      match parse_app_seq tokens with
      | t :: rest -> List.fold_left (fun t1 t2 -> TmApp (t1, t2)) t rest
      | [] -> invalid_arg "Syntax error"
    end
  and parse_app_seq tokens =
    (* Note: this can be tail-recursive *)
    match tokens with
    | TokenVar v :: rest -> TmVar v :: parse_app_seq rest
    | TokenTrue :: rest -> TmTrue :: parse_app_seq rest
    | TokenFalse :: rest -> TmFalse :: parse_app_seq rest
    | TokenLParen :: _ ->
      let first_part, second_part = split_on_parens tokens in
      parse first_part :: parse_app_seq second_part
    | [] -> []
    | _ -> invalid_arg "Syntax error"
end

module Lexer = struct
  open Parser

  let token_by_word = [
    ("true", TokenTrue);
    ("false", TokenFalse);
    ("if", TokenIf);
    ("then", TokenThen);
    ("else", TokenElse);
    ("(", TokenLParen);
    (")", TokenRParen);
    ("/", TokenLambda);
    (":", TokenColon);
    (".", TokenDot);
    ("Bool", TokenTyBool);
    ("->", TokenTyArr);
  ]

  let is_whitespace c = c = ' ' || c = '\t' || c = '\n'

  let tokenize input =
    let rec tokenize' acc i =
      if i = String.length input then acc else
      if is_whitespace input.[i] then tokenize' acc (i + 1) else
      let this, di =
        let startswith word =
          i + String.length word <= String.length input
          && String.sub input i (String.length word) = word
        in let f (word, token) =
          if startswith word then Some (token, String.length word) else None
        in match List.find_map f token_by_word with
        | Some (token, di) -> token, di
        | None ->  TokenVar (String.sub input i 1), 1
    in tokenize' (this :: acc) (i + di)
    in List.rev (tokenize' [] 0)
end

let parse input = input |> Lexer.tokenize |> Parser.parse

(* 4 *)

let counter = ref 0

let pick_new_type_name () =
  incr counter;
  "T" ^ string_of_int !counter

let rec gen_constr ctx t =
  match t with
  | TmVar v -> List.assoc v ctx, []
  | TmAbs (v, typ1, b) ->
    let typ2, cs = gen_constr ((v, typ1) :: ctx) b in
    TyArr (typ1, typ2), cs
  | TmApp (t1, t2) ->
    let typ1, cs1 = gen_constr ctx t1 in
    let typ2, cs2 = gen_constr ctx t2 in
    let x = TyVar (pick_new_type_name ()) in
    x, ((typ1, TyArr (typ2, x)) :: (cs1 @ cs2))
  | TmTrue -> TyBool, []
  | TmFalse -> TyBool, []
  | TmIf (t1, t2, t3) ->
    let typ1, cs1 = gen_constr ctx t1 in
    let typ2, cs2 = gen_constr ctx t2 in
    let typ3, cs3 = gen_constr ctx t3 in
    typ2, ((typ1, TyBool) :: (typ2, typ3) :: cs1 @ cs2 @ cs3)

let rec subst v t typ =
  match typ with
  | TyVar x when x = v -> t
  | TyArr (typ1, typ2) -> TyArr (subst v t typ1, subst v t typ2)
  | _ -> typ

let subst_all substs typ =
  List.fold_left (fun typ' (v, t) -> subst v t typ') typ substs

let rec subst_term substs t =
  match t with
  | TmAbs (v, typ, b) -> TmAbs (v, subst_all substs typ, subst_term substs b)
  | TmApp (t1, t2) -> TmApp (subst_term substs t1, subst_term substs t2)
  | _ -> t

let subst_constraints v t constraints =
  List.map (fun (typ1, typ2) -> (subst v t typ1, subst v t typ2)) constraints

let unify constraints =
  let rec aux acc constraints =
    match constraints with
    | [] -> acc
    | c :: cs -> begin
      match c with
      | s, t when s = t -> aux acc cs
      | TyVar x, t -> aux ((x, t) :: acc) (subst_constraints x t cs)
      | s, TyVar x -> aux ((x, s) :: acc) (subst_constraints x s cs)
      | TyArr (s1, s2), TyArr (t1, t2) -> aux acc ((s1, t1) :: (s2, t2) :: cs)
      | _ -> failwith "Cannot unify"
    end
  in aux [] constraints

let test_unify input =
  let pt i = i |> Lexer.tokenize |> Parser.parse_type in
  let constraints = List.map (fun (sr, tr) -> pt sr, pt tr) input in
  unify constraints

let test input =
  let t = input |> Lexer.tokenize |> Parser.parse in
  let typ, cs = gen_constr [] t in
  let substs = unify cs in
  subst_all substs typ |> string_of_ty
