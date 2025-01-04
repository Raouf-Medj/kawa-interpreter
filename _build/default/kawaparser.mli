
(* The type of tokens. *)

type token = 
  | WHILE
  | VIRG
  | VAR
  | TVOID
  | TRUE
  | TINT
  | THIS
  | TBOOL
  | SUB
  | SEMI
  | RPAR
  | RETURN
  | PT
  | PRINT
  | OR
  | NOT
  | NEW
  | NEQ
  | MUL
  | METHOD
  | MAIN
  | LT
  | LPAR
  | LE
  | INT of (int)
  | IF
  | IDENT of (string)
  | GT
  | GE
  | FALSE
  | EQS
  | EQ
  | EOF
  | END
  | ELSE
  | DIV
  | CLASS
  | BEGIN
  | ATTR
  | AND
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Kawa.program)
