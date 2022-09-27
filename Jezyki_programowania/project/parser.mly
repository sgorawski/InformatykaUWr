%{
  let indexof e l =
    let rec aux acc l =
      match l with
      | x :: xs -> if x = e then acc else aux (acc + 1) xs
      | [] -> failwith "Not found"
    in
    aux 0 l

  let empty_ctx = []

  let lookup_ctx = indexof

  let update_ctx x ctx = x :: ctx

  let free_vars = ref ["-"]

  let lookup_free_vars x = indexof x !free_vars - List.length !free_vars + 1

  let update_free_vars x = free_vars := (x :: !free_vars)

  let reset_free_vars () = free_vars := ["-"]

  let get_free_var x =
    try
      lookup_free_vars x
    with Failure _ ->
      update_free_vars x;
      lookup_free_vars x

  let get_var x ctx =
    try
      lookup_ctx x ctx
    with Failure _ ->
      get_free_var x
%}

%token <string> VAR
%token <int> NUM
%token LPAREN
%token RPAREN
%token LAMBDA
%token DOT
%token EOF
%token SEMI
%token PLUS
%token TIMES
%token MINUS
%token EQ
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token FIX
%token COMMA
%token FST
%token SND
%token NIL
%token CONS
%token HEAD
%token TAIL
%token ISNIL

%start main

%type <(Syntax.term * string list) list> main

%%

main :
  | expr SEMI main { reset_free_vars (); let parsed = $1 empty_ctx in (parsed, !free_vars) :: $3 }
  | EOF { [] }
;

expr :
  | LAMBDA VAR DOT expr { fun ctx -> Syntax.TmAbs ($2, $4 (update_ctx $2 ctx)) }
  | atom atom { fun ctx -> Syntax.TmApp ($1 ctx, $2 ctx) }
  | atom PLUS atom { fun ctx -> Syntax.TmAdd ($1 ctx, $3 ctx) }
  | atom TIMES atom { fun ctx -> Syntax.TmMul ($1 ctx, $3 ctx) }
  | atom MINUS atom { fun ctx -> Syntax.TmSub ($1 ctx, $3 ctx) }
  | atom EQ atom { fun ctx -> Syntax.TmEq ($1 ctx, $3 ctx) }
  | IF expr THEN expr ELSE expr { fun ctx -> Syntax.TmIf ($2 ctx, $4 ctx, $6 ctx) }
  | FIX expr { fun ctx -> Syntax.TmFix ($2 ctx) }
  | atom COMMA atom { fun ctx -> Syntax.TmPair ($1 ctx, $3 ctx) }
  | FST expr { fun ctx -> Syntax.TmFst ($2 ctx) }
  | SND expr { fun ctx -> Syntax.TmSnd ($2 ctx) }
  | atom CONS expr { fun ctx -> Syntax.TmCons ($1 ctx, $3 ctx) }
  | HEAD expr { fun ctx -> Syntax.TmHead ($2 ctx) }
  | TAIL expr { fun ctx -> Syntax.TmTail ($2 ctx) }
  | ISNIL expr { fun ctx -> Syntax.TmIsNil ($2 ctx) }
  | atom { fun ctx -> $1 ctx }
;

atom :
  | LPAREN expr RPAREN { fun ctx -> $2 ctx }
  | VAR { fun ctx -> Syntax.TmVar (get_var $1 ctx) }
  | NUM { fun _ -> Syntax.TmNum $1 }
  | TRUE { fun _ -> Syntax.TmTrue }
  | FALSE { fun _ -> Syntax.TmFalse }
  | NIL { fun _ -> Syntax.TmNil }
;
