(* -*- mode: Tuareg;-*-                                                     *)
(* Filename:    RHO.ml                                                      *)
(* Authors:     lgm                                                         *)
(* Creation:    Tue Dec 28 12:55:02 2004                                    *)
(* Copyright:   Biosimilarity LLC 2004 - 2006. All rights reserved.         *)
(*              See LICENSE.BIOSIM in the license directory.                *)
(* Description:                                                             *)
(* ------------------------------------------------------------------------ *)

(* module RHO =
   struct *)

	(* grammar *)
	type process =
		Zero
	  | Input of action * process
	  | Lift of name * process
	  | Drop of name
	  | Par of process list
	and action = Action of name * name
	and name =
		Quote of process

	(* abstract constructors *)
	let zero = Zero
	let guard nsubj nobj = Action( nsubj, nobj )
	let input nsubj nobj cont = Input( Action( nsubj, nobj ), cont )
    let prefix act cont =
	  match act with
		  Action( Quote( proc1 ), Quote( proc2 ) ) -> Input( Action( Quote( proc1 ), Quote( proc2 ) ), cont )
	let lift nsubj cont = Lift( nsubj, cont )
	let drop n = Drop( n )

	(* note that given newfound sensitivity to spatial logic considerations, 0's are not removed *)
	let par proc1 proc2 =
	  match ( proc1, proc2 ) with
		  ( Par( proclist1 ), Par( proclist2 ) ) -> Par( proclist1 @ proclist2 )
		| ( Par( proclist ), proc ) -> Par( proclist @ [ proc ] )
		| ( proc, Par( proclist ) ) -> Par( proc :: proclist )
		| ( p1, p2 ) -> Par( [ p1; p2 ] )
	let parstar proclist =
	  match proclist with
		  [] -> Zero
		| proclisthd :: proclisttl ->
			List.fold_left
			  (fun acc proc -> par acc proc)
			  proclisthd
			  proclisttl

	let quote proc = Quote( proc )

	(* protocols *)
	let rec processQuoteDepth v =
	  match v with
		  Zero -> 0
		| Input( Action( nsubj, nobj ), cont ) ->
			let qDSubj = nameQuoteDepth nsubj
			in let qDCont = processQuoteDepth cont
			in if ( qDSubj >= qDCont )
			  then qDSubj
			  else qDCont
		| Lift( nsubj, cont ) ->
			let qDSubj = nameQuoteDepth nsubj
			in let qDCont = processQuoteDepth cont
			in if ( qDSubj >= qDCont )
			  then qDSubj
			  else qDCont
		| Drop( n ) -> nameQuoteDepth n
		| Par( procs ) ->
			List.fold_left
			  (fun qD proc ->
				 let qDP = processQuoteDepth proc
				 in if ( qD >= qDP )
				   then qD
				   else qDP)
			  0
			  procs
	and nameQuoteDepth v =
	  match v with
		  Quote( proc ) -> 1 + (processQuoteDepth proc)

	(* quote depth ordered sets of names *)
	module QuoteDepthOrderedProcessName =
	  struct
		type t = name
		let compare n1 n2 =
		  let qDN1 = nameQuoteDepth n1
		  in let qDN2 = nameQuoteDepth n2
		  in if (qDN1 < qDN2)
			then -1
			else if (qDN1 = qDN2)
			then 0
			else 1
	  end

	module QuoteDepthNameSet = Set.Make(QuoteDepthOrderedProcessName)

  (* quote depth ordered sets of processes *)
	module QuoteDepthOrderedProcess =
	  struct
		type t = process
		let compare p1 p2 =
		  let qDP1 = processQuoteDepth p1
		  in let qDP2 = processQuoteDepth p2
		  in if ( qDP1 < qDP2 )
			then -1
			else if ( qDP1 = qDP2 )
			then 0
			else 1
	  end

	module QuoteDepthProcessSet = Set.Make(QuoteDepthOrderedProcess)
	  
	  (* sets of names *)
	module UnorderedProcessName =
	  struct
		type t = name
		let compare n1 n2 =
		  let qDN1 = nameQuoteDepth n1
		  in let qDN2 = nameQuoteDepth n2
		  in if (n1 = n2)
			then 0
			else if (qDN1 < qDN2)
			then -1
			else 1
	  end

	module NameSet = Set.Make(UnorderedProcessName)

  (* sets of processes *)
	module UnorderedProcess =
	  struct
		type t = process
		let compare p1 p2 =
		  let qDP1 = processQuoteDepth p1
		  in let qDP2 = processQuoteDepth p2
		  in if ( p1 = p2 )
			then 0
			else if ( qDP1 < qDP2 )
			then -1
			else 1
	  end

	module ProcessSet = Set.Make(UnorderedProcess)
  
  (* free names *)
	let rec freeNames proc =
	  match proc with
		  Zero -> NameSet.empty
		| Input( Action( nsubj, nobj ), cont ) ->
			NameSet.add nsubj (NameSet.diff (freeNames cont) (NameSet.add nobj NameSet.empty))
		| Lift( nsubj, cont ) ->
			NameSet.add nsubj (freeNames cont)
		| Drop( n ) -> NameSet.add n NameSet.empty
		| Par( proclist ) ->
			List.fold_left
			  (fun acc proc -> NameSet.union acc (freeNames proc))
			  NameSet.empty
			  proclist

  (* syntactic substitution *)
	let calculateNextName proc =
	  match proc with
		  Zero -> Quote( Zero )
		| Input( Action( Quote( psubj ), Quote( pobj ) ), cont ) ->
			Quote( parstar [ psubj; pobj; cont ] )
		| Lift( Quote( psubj ), cont ) ->
			Quote( par psubj cont )
		| Drop( Quote( p ) ) -> Quote( par p p )
		| Par( [] ) -> Quote( Zero )
		| Par( proclisthd :: proclisttl ) ->
			Quote( List.fold_left (fun acc proc -> par acc proc) proclisthd proclisttl )
			  
	let rec syntacticSubstitution proc nsource ntarget =
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
				(syntacticSubstitution
				   (if (nameEquivalent nobj ntarget) (* ( nobj = ntarget ) replaced with name equality *)
					then (syntacticSubstitution cont obj nobj)
					else cont)
				   nsource
				   ntarget)
			  )
		| Lift( nsubj, cont ) ->
			Lift(
			  (if (nameEquivalent nsubj ntarget) (* ( nsubj = ntarget ) replaced with name equality *)
			   then nsource
			   else nsubj),
			  (syntacticSubstitution cont nsource ntarget)
			)
		| Drop( n ) ->
			Drop(
			  if (nameEquivalent n ntarget) (* ( n = ntarget ) replaced with name equality *)
			  then nsource
			  else n
			) 
		| Par( proclist ) ->
			Par( List.map (fun proc -> (syntacticSubstitution proc nsource ntarget)) proclist )

  (* alpha equivalence *)

	and alphaEquivalent proc1 proc2 =
	  match ( proc1, proc2 ) with
		  ( Input( Action( nsubj1, nobj1 ), cont1 ),
			Input( Action( nsubj2, nobj2 ), cont2 ) ) ->
			((nameEquivalent nsubj1 nsubj2) (* ( nsubj1 = nsubj2 ) replaced with name equality *)
			   && ( cont1 = (syntacticSubstitution cont2 nobj1 nobj2) ))
		| ( p1, p2 ) -> (p1 = p2)

  (* structural equivalence *)

	and structurallyEquivalent proc1 proc2 =
	  match ( proc1, proc2 ) with

		  (* the empty par is 0 *)
		  ( Zero, Par( [] ) ) -> true
		| ( Par( [] ), Zero ) -> true
			
		(* 0 is structurally equivalent to 0|0|...|0 *)
		| ( Zero, Par( proclisthd :: proclisttl ) ) ->
			((structurallyEquivalent proc1 proclisthd)
			   && (structurallyEquivalent proc1 (Par( proclisttl ))))
		| ( Par( proclisthd :: proclisttl ), Zero ) ->
			(structurallyEquivalent Zero (Par( proclisthd :: proclisttl )))

		(* structural equivalence includes alpha equivalence *)  
		| ( Input( Action( nsubj1, nobj1 ), cont1 ), Input( Action( nsubj2, nobj2 ), cont2 ) ) ->
			((nameEquivalent nsubj1 nsubj2) (* (nsubj1 = nsubj2) replaced with name equality *)
			   && (structurallyEquivalent
					 cont1
					 (syntacticSubstitution cont2 nobj1 nobj2)))

		(* par is commutative and associative *)
		| ( Par( proclisthd1 :: proclisttl1 ), Par( proclist2 ) ) ->
			(match
			   (List.partition
				  (fun proc -> structurallyEquivalent proclisthd1 proc)
				  proclist2)
			 with ([],tl) -> false
			   | (eqhd,eqtl) ->
				   (match
					  (List.fold_left
						 (fun rejects proc ->
							match rejects with
								(false,r,l) ->
								  (if (structurallyEquivalent
										 (parstar (r @ l @ eqtl))
										 (Par( proclisttl1 )))
								   then (true,r,l)
								   else (false,r @ [proc],(List.tl l)))
							  | (true,r,l) -> (true,r,l))
						 (false,[],(List.tl eqhd))
						 eqhd)
					with (ans,r,l) -> ans))
		| ( Par( proclist1 ), Par( proclisthd2 :: proclisttl2 ) ) ->
			(structurallyEquivalent
			   (Par( proclisthd2 :: proclisttl2 ))
			   (Par( proclist1 )))

		(* 0 is the identity for par *)
		| ( proc1, Par( proclist ) ) ->
			(match
			   (List.partition
				  (fun proc -> structurallyEquivalent proc1 proc)
				  proclist)
			 with ( [], procs ) -> false
			   | ( [eqproc], procs ) -> (structurallyEquivalent Zero (Par( procs )))
			   | ( eqproclisthd :: eqproclisttl, procs ) -> false)
		| ( Par( proclist ), proc2 ) ->
			(structurallyEquivalent proc2 (Par( proclist )))

		(* structural equivalence includes syntactic equality *)
		| ( p1, p2 ) -> ( p1 = p2 )

	(* name equality *)
	and nameEquivalent n1 n2 =
	  match ( n1, n2 ) with
		  ( Quote( Drop( n11 ) ), n2 ) -> (nameEquivalent n11 n2)
		| ( n1, Quote( Drop( n21 ) ) ) -> (nameEquivalent n1 n21)
		| ( Quote( p1 ), Quote( p2 ) ) -> (structurallyEquivalent p1 p2)

	(* io control *)
	let rec processPrint proc =
	  match proc with
		  Zero -> "0"
		| Input( Action( nsubj, nobj ), cont ) ->
			(namePrint nsubj) ^ "(" ^ (namePrint nobj) ^ ")" ^ "{" ^  (processPrint cont) ^ "}"
		| Lift( nsubj, cont ) ->
			(namePrint nsubj) ^ "{|" ^ (processPrint cont ) ^ "|}"
		| Drop( n ) -> ">" ^ (namePrint n) ^ "<"
		| Par( [] ) -> "0"
		| Par( proclisthd :: proclisttl ) ->
			(List.fold_left
			   (fun acc proc -> acc ^ "|" ^ (processPrint proc))
			   (processPrint proclisthd)
			   proclisttl)
	and namePrint n =
	  match n with
		  Quote( p ) -> "'" ^ (processPrint p) ^ "'"
			  
(*  end;; *)
