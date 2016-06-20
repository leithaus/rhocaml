(* -*- mode: Tuareg;-*-                                                     *)
(* Filename:    loadall.ml                                                  *)
(* Authors:     lgm                                                         *)
(* Creation:    Mon Mar  7 11:52:24 2005                                    *)
(* Copyright:   Biosimilarity LLC 2004 - 2006. All rights reserved.         *)
(*              See LICENSE.BIOSIM in the license directory.                *)
(* Description:                                                             *)
(* ------------------------------------------------------------------------ *)

let run_command com f =
 match Sys.command (com ^ " " ^ f) with
 | 0 -> ()
 | _ -> failwith (Printf.sprintf "Cannot succeed to run ``%s %s''" com f);;

let compile f = run_command "ocamlc -c" f;;

compile "rho.ml";;

compile "vm.ml";;

compile "logic.ml";;

compile "mc.ml";;

compile "shell.ml";;

run_command "ocamlyacc" "parser.mly";;

compile "parser.mli";;

run_command "ocamllex" "lexer.mll";;
compile "lexer.ml";;

compile "parser.ml";;

compile "frontend.ml";;

#load "rho.cmo";;
#load "vm.cmo";;
#load "logic.cmo";;
#load "mc.cmo";;
#load "shell.cmo";;
#load "lexer.cmo";;
#load "parser.cmo";;
#load "frontend.cmo";;

compile "repl.ml";;

#load "repl.cmo";;

open Repl;;

print_string
 "\nTo run: type\n   read_eval_print_loop ();;\n\n \
  Try for instance:\n  \
   '0'('0'){ 0 }|'0'{| 0 |}\n\
  See the README file for more information.\n";
print_newline();;

