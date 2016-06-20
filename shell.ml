(* -*- mode: Tuareg;-*-                                                     *)
(* Filename:    shell.ml                                                    *)
(* Authors:     lgm                                                         *)
(* Creation:    Mon Mar 21 15:01:10 2005                                    *)
(* Copyright:   Biosimilarity LLC 2005. All rights reserved.                *)
(*              See LICENSE.BIOSIM in the license directory.                *)
(* Description:                                                             *)
(* ------------------------------------------------------------------------ *)

open Rho
open Logic
open Geometry
open Geometrize

type command =
    Quit
    | Eval of process
    | Prove of formula
    | Check of judgment
    | Geometry of Geometrize.format

exception InvalidExtraction of command

let quit () = Quit
let evaluate proc = Eval( proc )
let prove form = Prove( form )
let check jdgmnt = Check( jdgmnt )
let geometry proc = Geometry( proc )

let commandProcess cmd =
  match cmd with
      Quit -> (raise (InvalidExtraction( cmd )))
    | Eval( process ) -> process
    | Prove( formula ) -> (raise (InvalidExtraction( cmd )))
    | Check( Models( process, _ ) ) -> process
    | Geometry( fmt ) -> 
        match fmt with
            Geometrize.Stdio( process ) -> process
          | Geometrize.Fileio( process, _ ) -> process

let commandFormula cmd =
  match cmd with
      Quit -> (raise (InvalidExtraction( cmd )))
    | Eval( _ ) -> (raise (InvalidExtraction( cmd )))
    | Prove( formula ) -> formula
    | Check( Models( _, formula ) ) -> formula
    | Geometry( _ ) -> (raise (InvalidExtraction( cmd )))

let runCommand cmd =
  match cmd with
	  Quit ->
            begin
              print_string " *** Rho Top Level exiting *** \n";
	      false
            end
	| Eval( process ) ->
	    let reduct = Vm.reduce process in
              begin
                print_string (Rho.processPrint reduct);
		print_newline ();
		flush stdout;
		true
              end
	| Prove( formula ) -> (* temporary interpretation *)
	    begin
              print_string (Logic.formulaPrint formula);
	      print_newline ();
	      flush stdout;
	      true
            end
	| Check( judgment ) ->
	    begin
              print_string (if (Mc.check judgment) then "tt" else "ff");
	      print_newline ();
	      flush stdout;
	      true
            end
        | Geometry( fmt ) ->
            let rslt =
              (Geometrize.formatProcess fmt) in
              begin
                print_string rslt;
	        print_newline ();
	        flush stdout;
	        true
              end
