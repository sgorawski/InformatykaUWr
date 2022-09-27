type ty =
  | TyBool
  | TyArr of ty * ty

let rec string_of_ty t =
  match t with
  | TyBool -> "Bool"
  | TyArr (TyBool, t2) -> "Bool -> " ^ string_of_ty t2
  | TyArr (t1, t2) -> "(" ^ string_of_ty t1 ^ ") -> " ^ string_of_ty t2

type term =
  | TmVar of string
  | TmAbs of string * term
  | TmApp of term * term
  | TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmAnn of term * ty

let rec string_of_term t =
  match t with
  | TmVar v -> v
  | TmAbs (v, t) -> "/" ^ v ^ "." ^ string_of_term t
  | TmApp (TmVar v1, TmVar v2) -> v1 ^ v2
  | TmApp (TmVar v1, t2) -> v1 ^ "(" ^ string_of_term t2 ^ ")"
  | TmApp (t1, TmVar v2) -> "(" ^ string_of_term t1 ^ ")" ^ v2
  | TmApp (t1, t2) -> "(" ^ string_of_term t1 ^ ")(" ^ string_of_term t2 ^ ")"
  | TmTrue -> "true"
  | TmFalse -> "false"
  | TmIf (t1, t2, t3) -> "if " ^ string_of_term t1 ^ " then " ^ string_of_term t2 ^ " else " ^ string_of_term t3
  | TmAnn (t, typ) -> string_of_term t ^ " : " ^ string_of_ty typ

module Parser = struct
  type token =
    | TokenLParen
    | TokenRParen
    | TokenLambda of string
    | TokenVar of string
    | TokenTrue
    | TokenFalse
    | TokenIf
    | TokenThen
    | TokenElse
    | TokenColon
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

    let rec parse_type tokens =
      match tokens with
      | TokenTyBool :: [] -> TyBool
      | TokenTyBool :: TokenTyArr :: rest -> TyArr (TyBool, parse_type rest)
      | TokenLParen :: _ -> begin
        let first_part, second_part = split_on_parens tokens in
        match second_part with
        | TokenTyArr :: rest -> TyArr (parse_type first_part, parse_type rest)
        | [] -> parse_type first_part
        | _ -> invalid_arg "Unrecognized type"
      end
      | _ -> invalid_arg "Unrecognized type"


  let rec parse tokens =
    try parse_type_annotation tokens
    with Invalid_argument _ -> match tokens with
    | TokenTrue :: [] -> TmTrue
    | TokenFalse :: [] -> TmFalse
    | TokenIf :: _ ->
      let if_tokens, then_tokens, else_tokens = break_up_if tokens in
      TmIf (parse if_tokens, parse then_tokens, parse else_tokens)
    | TokenVar v :: [] -> TmVar v
    | TokenLambda v :: rest -> TmAbs (v, parse rest)
    | _ -> begin
      match parse_app_seq tokens with
      | t :: rest -> List.fold_left (fun t1 t2 -> TmApp (t1, t2)) t rest
      | [] -> invalid_arg "Syntax error"
    end
  and parse_type_annotation tokens =
    let term_tokens, type_tokens = split_ann tokens in
    let t = parse term_tokens in
    let typ = parse_type type_tokens in
    TmAnn (t, typ)
  and parse_app_seq tokens =
    (* Note: this can be tail-recursive *)
    match tokens with
    | TokenVar v :: rest -> TmVar v :: parse_app_seq rest
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
    (":", TokenColon);
    ("Bool", TokenTyBool);
    ("->", TokenTyArr);
  ]

  let is_whitespace c = c = ' ' || c = '\t' || c = '\n'

  let tokenize input =
    let rec tokenize' acc i =
      if i = String.length input then acc else
      let this, di = match input.[i] with
      | '(' -> Some TokenLParen, 1
      | ')' -> Some TokenRParen, 1
      | '/' -> Some (TokenLambda (String.sub input (i + 1) 1)), 3
      | v when is_whitespace v -> None, 1
      | _ -> begin
        let startswith word =
          i + String.length word <= String.length input
          && String.sub input i (String.length word) = word
        in let f (word, token) =
          if startswith word then Some (token, String.length word) else None
        in match List.find_map f token_by_word with
        | Some (token, di) -> Some token, di
        | None -> Some (TokenVar (String.sub input i 1)), 1
      end
      in let acc' = match this with
      | Some token -> token :: acc
      | None -> acc
    in tokenize' acc' (i + di)
    in List.rev (tokenize' [] 0)
end

let parse input = input |> Lexer.tokenize |> Parser.parse

(* 3 *)

exception TypeError

let rec infer_type ctx t =
  match t with
  | TmTrue -> TyBool
  | TmFalse -> TyBool
  | TmIf (t1, t2, t3) ->
    let t1, t2, t3 = infer_type ctx t1, infer_type ctx t2, infer_type ctx t3 in
    if t1 = TyBool && t2 = t3 then t2 else raise TypeError
  | TmVar v -> begin
    try List.assoc v ctx
    with Not_found -> raise TypeError
  end
  | TmAnn (t, typ) -> if check_type ctx t typ then typ else raise TypeError
  | TmApp (t1, t2) -> begin
    match infer_type ctx t1 with
    | TyArr (typ1, typ2) -> if check_type ctx t2 typ1 then typ2 else raise TypeError
    | _ -> raise TypeError
  end
  | _ -> failwith "What to do?"
and check_type ctx t typ =
  match t, typ with
  | TmIf (t1, t2, t3), _ ->
    check_type ctx t1 TyBool && check_type ctx t2 typ && check_type ctx t3 typ
  | TmAbs (v, b), TyArr (typ1, typ2) ->
    check_type ((v, typ1) :: ctx) b typ2
  | TmAbs _, _ -> false
  | _, _ -> infer_type ctx t = typ

let typecheck term_input type_input =
  let t = term_input |> Lexer.tokenize |> Parser.parse in
  let typ = type_input |> Lexer.tokenize |> Parser.parse_type in
  check_type [] t typ
