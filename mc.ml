(* -*- mode: Tuareg;-*-                                                     *)
(* Filename:    mc.ml                                                       *)
(* Authors:     lgm                                                         *)
(* Creation:    Fri Mar 18 12:00:31 2005                                    *)
(* Copyright:   Biosimilarity LLC 2005. All rights reserved.                *)
(*              See LICENSE.BIOSIM in the license directory.                *)
(* Description: A naive model-checker for rho.                              *)
(* ------------------------------------------------------------------------ *)

open Rho;;
open Logic;;

exception InvalidModelFormulaSetList;;
exception InvalidModelType of string;;
exception InvalidPropertyType of string;;
exception InvalidJudgmentType of string;;

let rec satisfies proc form =
  match form with
	  True -> true
	| ZeroF -> structurallyEquivalent proc (zero)
	| Negation( nForm ) -> (not (satisfies proc nForm))
	| Conjunction( [] ) -> false
	| Conjunction( hForm :: rForm ) -> 
		List.fold_left
		  (fun acc form -> ((satisfies proc form) && acc))
		  true
		  (hForm :: rForm)
	| Mixture( [] ) -> structurallyEquivalent proc (zero)		
	| Mixture( hForm :: rForm ) ->
		(match proc with
			 Par( [] ) ->
			   (List.fold_left
				  (fun acc form -> acc && (form = ZeroF))
				  true
				  (hForm :: rForm))
		   | Par( hProc :: rProc ) -> 
			   (parMix hProc rProc hForm rForm)
		   | pOther -> false)
	| Descent( m ) -> (* should we require exact match in this case and let the user specify the true? *)
		(match proc with
			 Drop( n ) -> (nameEquivalent m n)
		   | Par( hProc :: rProc ) ->
			   if (not (satisfies hProc (Descent( m ))) )
			   then (satisfies (Par( rProc )) (Descent( m )))
			   else true
		   | pOther -> false)
	| Elevation( i, eForm ) ->
		(match proc with
			 Lift( nsubj, lProc ) -> (nominallySatisfies nsubj i) && (satisfies lProc eForm)
		   | Par( hProc :: rProc ) ->
			   if (not (satisfies hProc (Elevation( i, eForm ))) )
			   then (satisfies (Par( rProc )) (Elevation( i, eForm )))
			   else true
		   | pOther -> false)
	| Activity( Condition( i, n ), aForm ) ->
		(match proc with
			 Input( Action( nsubj, nobj ), iProc ) ->
			   (nominallySatisfies nsubj i)
			   && (let ntarget = Quote( Par( [ Lift( nobj, Zero ); Lift( n, Zero ) ] ) )
				   in (satisfies
						 (syntacticSubstitution iProc nobj ntarget)
						 (logicalSubstitution aForm n ntarget)))
		   | pOther -> false)

and nominallySatisfies n i =
  match n with
	  Quote( p ) ->
		(match i with
			 Quotation( f ) ->
			   satisfies p f
		   | Naming(Quote( q )) -> nameEquivalent (Quote( p )) (Quote( q )))


(***********************************************************************************************************************)
(* the core algorithm for this case is to recognize when there are too many formulae competing for available processes *)
(* the converse, i.e. more processes than formula, is ok when there is a true in the mix of formulae                   *)
(* the complexity of this algorithm is bound to be unacceptable because there is no attempt to take advantage of lucky *)
(* breaks in the given configuration of formulae and processes; all processes in the mix that satisfy a given formula  *)
(* will be computed rather than trying to find some configuration that satisfies the constraints                       *)
(* however, we eliminate backtracking by doing this so the complexity analysis is a tad subtle                         *)
(***********************************************************************************************************************)
and parMix hProc rProc hForm rForm =
  let ntForm = (List.partition (fun phi -> (phi = True)) ( hForm :: rForm ))
  in match ntForm with
	  ( ltform, [] ) -> true (* form looks like true | ... | true *)
	| ( ltform, hntForm :: rntForm ) -> (* form looks like phi_0 | ... | phi_n| true | ... | true *)
		let rec aloop pfMatch =
		  match pfMatch with
			  [] -> true (* no failing ( { p_j }, [ phi_k ] ) pair *)
			| hpfMatch :: rpfMatch ->
				match hpfMatch with
					( pSet, fList ) ->
					  let fls = (List.length fList) in
					  let pss = (List.length (ProcessSet.elements pSet)) in
						if ( fls > pss ) 
						then false (* ( { p_j }, [ phi_k ] ) with k > j *)
						else if ( pss > fls )
						then match ltform with
							[] -> false (* ( { p_j }, [ phi_k ] ) with j > k and no true to absorb leftover p *)
						  | ne -> aloop rpfMatch (* ( { p_j }, [ phi_k ] ) with j > k and true to absorb leftover p *)
						else aloop rpfMatch (* ( { p_j }, [ phi_k ] ) with j = k *)
		in let rec floop lForm acc =
			match lForm with
				[] -> aloop acc (* we've come to the end of the list of phi_i *)							   
			  | hlForm :: rlForm -> (* some formulae to process *)
				  match (List.partition (fun p -> (satisfies p hlForm)) ( hProc :: rProc )) with
					  ( [], nProc ) -> false (* no process matching hlForm *)
					| ( hmProc :: rmProc, nProc ) -> (* some processes matching hlForm *)
						let pSet =
						  (List.fold_left
							 (fun acc proc -> (ProcessSet.union acc (ProcessSet.singleton proc)))
							 (ProcessSet.singleton hmProc)
							 rmProc)
						in let cacc =
							(List.fold_left
							   (fun facc hacc ->
								  (match hacc with
									   ( hapSet, hafSet ) ->
										 (match facc with 
											  ( [ ( gpSet, gfSet ) ], ansSet ) ->
												(let ipSet = ProcessSet.inter gpSet hapSet
												 in (if (ProcessSet.is_empty ipSet)
													   (* so far only pSet |= hlForm *)
													 then ( [ ( gpSet, gfSet ) ], ansSet @ [ ( hapSet, hafSet ) ] )
													   (* pSet, hapSet |= hlForm *)
													 else if ((ProcessSet.equal ipSet gpSet) || (ProcessSet.equal gpSet ipSet))
													 then ( [], ansSet @ [ ( hapSet, hlForm :: hafSet ) ] )
													 else ( [ ( gpSet, gfSet @ hafSet ) ],
															ansSet @ [ ( hapSet, hlForm :: hafSet ) ] )))
												| ( [], ansSet ) -> ( [], ansSet @ [ ( hapSet, hafSet ) ] )
												| other -> raise InvalidModelFormulaSetList)))
							   ( [ ( pSet, [ hlForm ] ) ], [] )
							   acc)
						in floop rlForm (match cacc with ( pfSet, nacc ) -> nacc @ pfSet)
		in floop (hForm :: rForm) []

let check j =
  match j with
	  Models( proc, form ) -> satisfies proc form
