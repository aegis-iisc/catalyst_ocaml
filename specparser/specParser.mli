
(* The type of tokens. *)

type token = 
  | UNION
  | UINST
  | TRUE
  | SUBSETEQ
  | SUBSET
  | STAR
  | SEMICOLON
  | RPAREN
  | RELATION
  | RCURLY
  | RBRACE
  | PRIMITIVE
  | PLUS
  | PIPE
  | NOT
  | MINUS
  | LPAREN
  | LCURLY
  | LBRACE
  | LAMBDA
  | INT of (int)
  | IMPL
  | IFF
  | ID of (string)
  | FALSE
  | EQUALOP
  | EOL
  | EOF
  | DOT
  | DISJ
  | CROSSPRD
  | CONJ
  | COMMA
  | COLON
  | ASSUME
  | ARROW

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val start: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (SpecLang.RelSpec.t)
