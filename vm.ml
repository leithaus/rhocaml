(* -*- mode: Tuareg;-*-                                                     *)
(* Filename:    vm.ml                                                       *)
(* Authors:     lgm                                                         *)
(* Creation:    Mon Mar  7 12:17:57 2005                                    *)
(* Copyright:   Biosimilarity LLC 2004 - 2006. All rights reserved.         *)
(*              See LICENSE.BIOSIM in the license directory.                *)
(* Description:                                                             *)
(* ------------------------------------------------------------------------ *)

open Rho;;

exception InvalidReductionResult of process;;
exception InvalidMatchResult of process;;

(****************************************************************************)
(*                  Is this a channel/co-channel pair?                      *)
(****************************************************************************)

let channelCochannel insubj onsubj = nameEquivalent insubj onsubj

(****************************************************************************)
(*                 Find the io match of proc in procList                    *)
(****************************************************************************)
let matchIO proc procList =
  match procList with
	  [] -> ( None, procList )
	| hProc :: rProc ->
		match proc with
			Zero        ->  ( None, procList )         (* 0 has no match *)
		  | Drop( n )   ->  ( None, procList )         (* Drop has no match *)
		  | Par( prox ) ->  ( None, procList )         (* This could be extended pt-wise ... *)
			  
		  | Input( Action( insubj, inobj ), iproc ) -> (* Find a Lift( ... ) with matching subject *)
			  (let ans =
				 List.partition
				   (fun proc ->
					  match proc with
						  Lift( onsubj, oproc ) -> (channelCochannel insubj onsubj)
						| p -> false)
				   procList
			   in match ans with
				   ( [], rProc ) -> ( None, procList )
				 | ( mProc::mrProc, rProc ) -> ( Some( mProc ), (mrProc @ rProc) ))
				
		  | Lift( onsubj, oproc ) ->                   (* Find an Input( ... ) with matching subject *)
			  (let ans =
				 List.partition
				   (fun proc ->
					  match proc with
						  Input( Action( insubj, inobj ), iproc ) -> (channelCochannel insubj onsubj)
						| p -> false)
				   procList
			   in match ans with
				   ( [], rProc ) -> ( None, procList )
				 | ( mProc::mrProc, rProc ) -> ( Some( mProc ), (mrProc @ rProc) ))
						  


(****************************************************************************)				
(*                    Substitution accounting for drop                      *)
(****************************************************************************)				
let rec semanticSubstitution proc nsource ntarget =
	  match proc with
		  Zero -> Zero
		| Input( Action( nsubj, nobj ), cont ) ->
			let obj =
				(if (nameEquivalent nobj ntarget) (* ( nobj = ntarget ) replaced with name equality *)
				 then (calculateNextName (Input( Action( nsubj, nobj ), cont )))
				 else nobj)
			in Input( Action( (if (nameEquivalent nsubj ntarget) (* ( nsubj = ntarget ) replaced with name equality *)
				 then nsource
				 else nsubj),
				obj ),
				(semanticSubstitution
				   (if (nameEquivalent nobj ntarget) (* ( nobj = ntarget ) replaced with name equality *)
					then (semanticSubstitution cont obj nobj)
					else cont)
				   nsource
				   ntarget)
			  )
		| Lift( nsubj, cont ) ->
			Lift(
			  (if (nameEquivalent nsubj ntarget) (* ( nsubj = ntarget ) replaced with name equality *)
			   then nsource
			   else nsubj),
			  (semanticSubstitution cont nsource ntarget)
			)
		| Drop( n ) ->
			if (nameEquivalent n ntarget) (* ( n = ntarget ) replaced with name equality *)
			then match nsource with
				Quote( sProc ) -> sProc
			else Drop( n )
		| Par( proclist ) ->
			Par( List.map (fun proc -> (semanticSubstitution proc nsource ntarget)) proclist )


(****************************************************************************)
(*                                 Reduction                                *)
(****************************************************************************)
let rec reduceOnce proc =
  match proc with
	  Zero -> ( false, Zero )                                                    (* stuck *)
	| Input( act, proc ) -> ( false, Input( act, proc ) )                        (* stuck *)
	| Lift( nsubj, proc ) -> ( false, Lift( nsubj, proc ) )                      (* stuck *)
	| Drop( nsubj ) -> ( false, Drop( nsubj ) )                                  (* stuck *)

	| Par( [] ) -> ( false, Par( [] ) )                                          (* stuck *)
	| Par( [ Zero ] ) -> ( false, Par( [ Zero ] ) )                              (* stuck *)
    | Par( [ Input( act, proc ) ] ) -> ( false, Par( [ Input( act, proc ) ] ) )  (* stuck *)
	| Par( [ Lift( nsubj, proc ) ] ) -> (false, Par( [ Lift( nsubj, proc ) ] ) ) (* stuck *)
	| Par( [ Drop( nsubj ) ] ) -> ( false, Par( [ Drop( nsubj ) ] ) )            (* stuck *)
 
	| Par( Par( hprox ) :: rProc ) -> reduceOnce( Par( (hprox @ rProc) ) )       (* mix *)
    | Par( Zero :: rProc ) -> reduceOnce( Par( rProc ) )                         (* structural equivalence *)
    | Par( Drop( nsubj ) :: rProc ) ->                                           (* mix *)
		reduceOnce( Par( rProc @ [ Drop( nsubj ) ] ) )                         

	| Par( Input( Action( insubj, inobj ), iproc ) :: rProc ) ->
		(let mProc = (matchIO (Input( Action( insubj, inobj ), iproc )) rProc) in
		   match mProc with
			   ( None, nrProc ) ->                                                (* mix *)
				 (let mix = reduceOnce( Par( rProc ) ) in
					match mix with
						( b, Par( prox ) ) ->
						  ( b, Par( Input( Action( insubj, inobj ), iproc ) :: prox ) )
					  | ( b, redproc ) -> raise (InvalidReductionResult( redproc )) )
			 | ( Some( Lift( onsubj, oproc ) ), nrProc ) ->                       (* comm *)
				 let inputK = (semanticSubstitution iproc (Quote( oproc )) inobj) in
				   ( true, Par( (nrProc @ [ inputK ]) ) )
			 | ( Some( badMProc ), rmproc ) -> raise (InvalidMatchResult( badMProc )) )

	| Par( Lift( onsubj, oproc ) :: rProc ) ->
		(let mProc = (matchIO (Lift( onsubj, oproc )) rProc) in
		   match mProc with
			   ( None, nrProc ) ->                                                (* mix *)
				 (let mix = reduceOnce( Par( rProc ) ) in
					match mix with
						( b, Par( prox ) ) ->
						  ( b, Par( Lift( onsubj, oproc ) :: prox ) )
					  | ( b, redproc ) -> raise (InvalidReductionResult( redproc )) )
			 | ( Some( Input( Action( insubj, inobj ), iproc ) ), nrProc ) ->     (* comm *)
				 let inputK = (semanticSubstitution iproc (Quote( oproc )) inobj) in
				   ( true, Par( (nrProc @ [ inputK ]) ) )
			 | ( Some( badMProc ), rmproc ) -> raise (InvalidMatchResult( badMProc )) )

let rec reduce proc =
  match (reduceOnce proc) with
	  ( true, reduct ) -> reduce( reduct )
	| ( false, fixpt ) -> fixpt
