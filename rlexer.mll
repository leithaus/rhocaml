(* -*- mode: Tuareg;-*-                                                     *)
(* Filename:    lexer.mll                                                   *)
(* Authors:     lgm                                                         *)
(* Creation:    Mon Mar  7 11:45:56 2005                                    *)
(* Copyright:   Biosimilarity LLC 2004 - 2006. All rights reserved.         *)
(*              See LICENSE.BIOSIM in the license directory.                *)
(* Description:                                                             *)
(* ------------------------------------------------------------------------ *)

{
open Rparser;;        (* The type token is defined in parser.mli *)
exception Eof;;
}

let white = ' ' | '\t' | '\r' | '\n'
(* let digit = ['0'-'9'] *)
(* let alpha = ['A'-'Z' 'a'-'z'] *)

rule token =
  parse
| [' ' '\t' '\r' '\n']  { token lexbuf }     (* skip blanks *)
| "//"          { comment lexbuf }

| '"' [' ' '!' '#'-'~']* '"'
    { FILENAME(Lexing.lexeme lexbuf) }

| "0"           { STOP }
| "<|"          { LLIFT }
| "|>"          { RLIFT }
| '('           { LPAREN }
| ')'           { RPAREN }
| '['           { LBRACK }
| ']'           { RBRACK }
| '{'           { LCURLY }
| '}'           { RCURLY }
| "{|"          { LMSET }
| "|}"          { RMSET }
| '^'           { CARROT }
| "'"           { QUOTE }
| "<"           { LANGLE }
| ">"           { RANGLE }
| '.'           { DOT }
| ';'           { SEMICOLON }
| '|'           { PAR }
| '?'           { WHIMPER }
| '~'           { TILDE }
| '&'           { AMPERSAND }
| "|="          { MODELS }
| "|-"          { VALID }
| "geometry"    { GEOMETRY }
| "quit"        { QUIT }
| "true"        { TRUE }
| eof           { EOF }

and comment = parse
| ['\n' '\r']   { token lexbuf }
| _             { comment lexbuf }
| eof           { EOF }
