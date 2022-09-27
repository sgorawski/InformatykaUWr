(* 4 *)

type 'a b23tree =
  | Leaf
  | TwoNode of 'a b23tree * 'a * 'a b23tree
  | ThreeNode of 'a b23tree * 'a * 'a b23tree * 'a * 'a b23tree

(* 5 *)

type term =
  | TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term

(* Grammar: t ::= true | false | 0 | iszero t | succ t | pred t | if t then t else t *)

type token =
  | TokenTrue
  | TokenFalse
  | TokenZero
  | TokenSucc
  | TokenPred
  | TokenIsZero
  | TokenIf
  | TokenThen
  | TokenElse

let token_of_string word =
  match word with
  | "true" -> TokenTrue
  | "false" -> TokenFalse
  | "0" -> TokenZero
  | "succ" -> TokenSucc
  | "pred" -> TokenPred
  | "iszero" -> TokenIsZero
  | "if" -> TokenIf
  | "then" -> TokenThen
  | "else" -> TokenElse
  | _ -> invalid_arg word

let lex source_code =
  let rec aux acc words =
    match words with
    | "" :: rest -> aux acc rest (* whitespace in input *)
    | word :: rest -> aux (token_of_string word :: acc) rest
    | [] -> acc
  in source_code |> String.split_on_char ' ' |> aux [] |> List.rev

let find_matching opening closing tokens =
  let rec aux acc nesting_level tokens =
    match tokens with
    | token :: rest when token = opening -> aux (token :: acc) (nesting_level + 1) rest
    | token :: rest when token = closing ->
      if nesting_level = 1 then acc, tokens else aux (token :: acc) (nesting_level - 1) rest
    | token :: rest -> aux (token :: acc) nesting_level rest
    | _ -> invalid_arg "Syntax error"
  in let first_part, second_part = aux [] 0 tokens in
  List.rev first_part, second_part

let break_up_if tokens =
  let if_part, rest = find_matching TokenIf TokenThen tokens in
  let then_part, else_part = find_matching TokenThen TokenElse rest in
  match if_part, then_part, else_part with
  | _ :: f, _ :: t, _ :: e -> f, t, e
  | _ -> invalid_arg "Syntax error"

let rec parse tokens =
  match tokens with
  | TokenTrue :: [] -> TmTrue
  | TokenFalse :: [] -> TmFalse
  | TokenZero :: [] -> TmZero
  | TokenSucc :: rest -> TmSucc (parse rest)
  | TokenPred :: rest -> TmPred (parse rest)
  | TokenIsZero :: rest -> TmIsZero (parse rest)
  | TokenIf :: _ ->
    let if_tokens, then_tokens, else_tokens = break_up_if tokens in
    TmIf (parse if_tokens, parse then_tokens, parse else_tokens)
  | _ -> invalid_arg "Syntax error"

let term_of_string source_code =
  source_code |> lex |> parse
