(* -*- mode: Tuareg;-*-                                                     *)
(* Filename:    frontend.ml                                                 *)
(* Authors:     lgm                                                         *)
(* Creation:    Mon Mar  7 11:50:46 2005                                    *)
(* Copyright:   Biosimilarity LLC 2004 - 2006. All rights reserved.         *)
(*              See LICENSE.BIOSIM in the license directory.                *)
(* Description:                                                             *)
(* ------------------------------------------------------------------------ *)

let lexbuf = Lexing.from_channel stdin;;
let parse () = Rparser.main Rlexer.token lexbuf;;
