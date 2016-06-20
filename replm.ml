(* -*- mode: Tuareg;-*-                                                     *)
(* Filename:    main.ml                                                     *)
(* Authors:     lgm                                                         *)
(* Creation:    Mon Mar  7 11:42:44 2005                                    *)
(* Copyright:   Biosimilarity LLC 2004 - 2006. All rights reserved.         *)
(*              See LICENSE.BIOSIM in the license directory.                *)
(* Description:                                                             *)
(* ------------------------------------------------------------------------ *)

let read_eval_print_loop () =
  let dbg = ref false in
  let rslt = ref true in
    print_string " *** Rho Top Level version 0.01 *** \n";
    try
      (while (!rslt)
        do	  
	  print_string "> ";
	  flush stdout;
	  let cmd = Rfrontend.parse () in
            begin
	      rslt := Shell.runCommand( cmd );
              (if (!dbg)
                then print_string ("rslt = " ^ (string_of_bool (!rslt))));
              print_newline ();
              flush stdout;
            end
        done)
    with e ->
      begin
        print_string "caught exception...\n"; 
        print_string (Printexc.to_string e);
        print_string "\n exiting";
        print_newline ();
        flush stdout;
        ()
      end;;


