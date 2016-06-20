(* -*- mode: Tuareg;-*-                                                     *)
(* Filename:    test-dbg.ml                                                 *)
(* Authors:     lgm                                                         *)
(* Creation:    Thu Jan 10 09:33:55 2008                                    *)
(* Copyright:   Not supplied                                                *)
(* Description:                                                             *)
(* ------------------------------------------------------------------------ *)

exception TestFailed of string

let lexbuf = Lexing.from_string "'0'('0'){ 0 }|'0'{| 0 |};;";;
let cmd = Rparser.main Rlexer.token lexbuf;;
let parRedex = Shell.commandProcess cmd;;
let inputRedex = 
  match parRedex with 
      Rho.Par( [] ) -> (raise (TestFailed( "ill formed parRedex" )))
    | Rho.Par( hdPar :: _ ) -> hdPar
    | _ -> (raise (TestFailed( "ill formed parRedex" )));;
let outputRedex =
  match parRedex with
      Rho.Par( [] ) -> (raise (TestFailed( "ill formed parRedex" )))
    | Rho.Par( _ :: rP :: _ ) -> rP
    | _ -> (raise (TestFailed( "ill formed parRedex" )));;
let inputSubject =
  match inputRedex with
      Rho.Input( Rho.Action ( nSubj, _ ), _ ) -> nSubj
    | _ -> (raise (TestFailed( "ill formed inputRedex" )));;
let outputSubject =
  match outputRedex with
      Rho.Lift( nSubj, _ ) -> nSubj
    | _ -> (raise (TestFailed( "ill formed inputRedex" )));;

if (not (Rho.nameEquivalent inputSubject outputSubject))
then (raise (TestFailed( "subjects don't match" )))
else if (not (Rho.structurallyEquivalent (Vm.reduce parRedex) Rho.Zero))
then (raise (TestFailed( "unexpected reduction" )))
else print_string ("success" ^ "\n");;
  
