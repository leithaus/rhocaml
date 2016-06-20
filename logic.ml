(* -*- mode: Tuareg;-*-                                                     *)
(* Filename:    logic.ml                                                    *)
(* Authors:     lgm                                                         *)
(* Creation:    Fri Mar 18 12:00:31 2005                                    *)
(* Copyright:   Biosimilarity LLC 2005. All rights reserved.                *)
(*              See LICENSE.BIOSIM in the license directory.                *)
(* Description:                                                             *)
(* ------------------------------------------------------------------------ *)

open Rho

type formula =
	True
  | ZeroF
  | Negation of formula
  | Conjunction of formula list
  | Mixture of formula list
  | Descent of name
  | Elevation of indicator * formula
  | Activity of condition * formula
and indicator =
	Quotation of formula
  | Naming of name
and condition = Condition of indicator * name

let zzero = ZeroF
let ttrue = True
let ffalse = Negation( True )
let negate form = Negation( form )
let conjoin formList =
  match formList with
	  [] -> (ffalse)
	| hForm :: rForm -> Conjunction( hForm :: rForm )
let disjoin formList =
  match formList with
	  [] -> (ttrue)
	| hForm :: rForm -> negate (conjoin (List.map (fun form -> negate form) (hForm :: rForm)))
let mix formList =
  match formList with
	  [] -> ZeroF
	| hForm :: rForm -> Mixture( hForm :: rForm )
let descend n = Descent( n )
let elevate i form = Elevation( i, form )
let arm a form = Activity( a, form )
let indicate form = Quotation( form )
let notice i n = Condition( i, n )
let rec logicalSubstitution form nsource ntarget =
  match form with
	  True -> True
	| ZeroF -> ZeroF
	| Negation( nForm ) -> Negation( (logicalSubstitution nForm nsource ntarget) )
	| Conjunction( [] ) -> Negation( True )
	| Conjunction( hForm :: rForm ) ->
		Conjunction( (List.map (fun phi -> (logicalSubstitution phi nsource ntarget)) (hForm :: rForm)) )
	| Mixture( [] ) -> ZeroF
	| Mixture( hForm :: rForm ) ->
		Mixture( (List.map (fun phi -> (logicalSubstitution phi nsource ntarget)) (hForm :: rForm)) )
	| Descent( n ) ->
		Descent( if (nameEquivalent n nsource) then ntarget else nsource)
	| Elevation( i, eForm ) ->		
		Elevation( (match i with
						Quotation( form ) -> i
					  | Naming( n ) -> Naming( (if (nameEquivalent n nsource) then ntarget else n) ) ),
				   (logicalSubstitution eForm nsource ntarget) )
	| Activity( Condition( i, n ), aForm ) -> (* bug: need a more robust substitution collision algorithm *)
		let nntarget = Quote( Par( [ Lift( n, Zero ); Lift( nsource, Zero ) ] ) ) in
		Activity( Condition( (match i with
								  Quotation( form ) -> i
								| Naming( n ) -> Naming( (if (nameEquivalent n nsource) then ntarget else n) ) ),
							 nntarget ),
				  (logicalSubstitution
					 (logicalSubstitution aForm n nntarget)
					 nsource
					 ntarget) )

let rec formulaPrint form =
  match form with
	  True -> "tt"
	| ZeroF -> "0"
	| Negation( True ) -> "ff"
	| Negation( nForm ) -> "~" ^ "(" ^ (formulaPrint nForm) ^ ")"
	| Conjunction( [] ) -> "ff"
	| Conjunction( hForm :: rForm ) ->
		List.fold_left
		  (fun acc phi -> acc ^ "&" ^ (formulaPrint phi))
		  (formulaPrint hForm)
		  rForm
	| Mixture( [] ) -> "0"
	| Mixture( hForm :: rForm ) ->
		List.fold_left
		  (fun acc phi -> acc ^ "|" ^ (formulaPrint phi))
		  (formulaPrint hForm)
		  rForm
	| Descent( n ) ->
		">" ^ (namePrint n) ^ "<"
	| Elevation( i, eForm ) ->
		(indicationPrint i) ^ "<<" ^ (formulaPrint eForm) ^ ">>"
	| Activity( Condition( i, n ), aForm ) -> 
		"<<" ^ (indicationPrint i) ^ "?" ^ (namePrint n) ^ ">>" ^ (formulaPrint aForm)
and indicationPrint i =
  match i with
	  Quotation( form ) -> "'" ^ (formulaPrint form) ^ "'"
	| Naming( n ) -> (namePrint n)

type judgment =	Models of process * formula

let judge proc form = Models( proc, form )
