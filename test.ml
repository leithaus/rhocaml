(* -*- mode: Tuareg;-*-                                                     *)
(* Filename:    test.ml                                                     *)
(* Authors:     lgm                                                         *)
(* Creation:    Mon Mar  7 18:27:51 2005                                    *)
(* Copyright:   Biosimilarity LLC 2004 - 2006. All rights reserved.         *)
(*              See LICENSE.BIOSIM in the license directory.                *)
(* Description:                                                             *)
(* ------------------------------------------------------------------------ *)

open Rho;;
open Logic;;
open Vm;;
open Mc;;

(****************************************************************************)
(*                                Processes                                 *)
(****************************************************************************)

let redex00 = Input( Action( Quote( Zero ), Quote( Zero ) ), Zero );;
let redex01 = Lift( Quote( zero ), zero );;
let redex0 = Par( [ redex00; redex01 ] );;
let dropNFwd =
  Input( Action( Quote( Zero ), Quote( Zero ) ),
		 Par( [ Drop( Quote( Zero ) );
				Lift( Quote( Zero ), Drop( Quote( Zero ) ) ) ] ) )

(****************************************************************************)
(*                                 Formulae                                 *)
(****************************************************************************)

let phi00 = negate (zzero);;
let phi01 = mix [ (negate (zzero)); (negate (zzero)) ];;
