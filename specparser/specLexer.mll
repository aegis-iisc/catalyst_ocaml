(*Header*)
{
open Lexing
open Parser
module Tokens = SpecParser

type pos = int 
let line = ref 0
let debugFlag = ref true
let eof = fun () -> Tokens.EOF
let debug = fun s -> if !debugFlag then Printf.printf "%s" s else ()


exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

(*Commonly used named regular expressions*)
let int = '-'? ['0'-'9'] ['0'-'9']*
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z']
let variable=(alpha|"_")+(digit)*("'")*
let number= (digit)+
let eol = ('\n'|"\013\n"|"\013"|"\010")
let ws=[' ' '\t']


let int = '-'? ['0'-'9'] ['0'-'9']*
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
| eol {line := (!line)+1; token lexbuf}
|(ws)+  {debug "whitespace"; token lexbuf}
|("primitive")  {debug "primitive" ; Tokens.PRIMITIVE}
|("relation")  {debug "relation"; Tokens.RELATION}
|("true") {debug "true"; Tokens.TRUE}
|("assume")  {debug "assume"; Tokens.ASSUME}
|("false")  {debug "false"; Tokens.FALSE}
|("not")  {debug "not"; Tokens.NOT}
|("\\")  {debug "lambda"; Tokens.LAMBDA}
|(".")  {debug "dot"; Tokens.DOT}
|("+")  {debug "plus"; Tokens.PLUS}
|("-")  {debug "minus"; Tokens.MINUS}
|("U")  {debug "union"; Tokens.UNION}
|("X")  {debug "crossprd"; Tokens.CROSSPRD}
|("C=")  {debug "subseteq"; Tokens.SUBSETEQ}
|("C")  {debug "subset"; Tokens.SUBSET}
|("=")  {debug "equalop";Tokens.EQUALOP}
|("=>")  {debug "implies";Tokens.IMPL}
|("<=>") {debug "iff";Tokens.IFF}
|("/\\")  {debug "conj";Tokens.CONJ}
|("\\/")  {debug "disj";Tokens.DISJ}
|(":")  {debug "colon\n";Tokens.COLON}
|(";")  {debug "semicolon\n";Tokens.SEMICOLON}
|(",")  {debug "comma\n";Tokens.COMMA}
|("*")  {debug "star\n";Tokens.STAR}
|("(")  {debug "lparen\n"; Tokens.LPAREN}
|(")")  {debug "rparen\n"; Tokens.RPAREN}
|("{")  {debug "lcurly\n"; Tokens.LCURLY}
|("}")  {debug "rcurly\n"; Tokens.RCURLY}
|("[")  {debug "lbrace\n"; Tokens.LBRACE}
|("]")  {debug "rbrace\n"; Tokens.RBRACE}
|("->")  {debug "arrow\n"; Tokens.ARROW}
|("(*")  {debug "comment begin\n"; comment lexbuf}
|("|")  {debug "pipe\n"; Tokens.PIPE}
|(variable)  {
					let v = lexeme lexbuf in 
					debug ("var: "^v^"\n"); Tokens.ID (v)}
|(number as nm)  {debug ("int: "^lexeme lexbuf^"\n") ; 
                      let n = int_of_string nm in 
                          Tokens.INT (n)
                          
                     }
|eof  {eof()}
and comment= 
  parse
 | '.'  {comment lexbuf}
|(ws)+  {comment lexbuf}
|(eol)  {line := (!line)+1; comment lexbuf}
|("*)")  {debug "comment end\n"; token lexbuf}

