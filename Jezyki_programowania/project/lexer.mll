{
  open Parser
}

rule main = parse
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '\\' { LAMBDA }
  | '.' { DOT }
  | '+' { PLUS }
  | '*' { TIMES }
  | '-' { MINUS }
  | '=' { EQ }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fix" { FIX }
  | ',' { COMMA }
  | "fst" { FST }
  | "snd" { SND }
  | "[]" { NIL }
  | "::" { CONS }
  | "head" { HEAD }
  | "tail" { TAIL }
  | "isnil" { ISNIL }
  | ['0'-'9']+ { NUM (int_of_string (Lexing.lexeme lexbuf))}
  | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9']* { VAR (Lexing.lexeme lexbuf) }
  | [' ' '\t' '\n'] { main lexbuf } (* Whitespace, ignore *)
  | '#' [^'\n']* { main lexbuf } (* Comments, ignore *)
  | ';' { SEMI }
  | eof { EOF }
