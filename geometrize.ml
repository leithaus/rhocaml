(* -*- mode: Tuareg;-*-                                                 *)
(* Filename:    geometrize.ml                                           *)
(* Authors:     lgm                                                     *)
(* Creation:    Tue Jan 16 15:26:09 2007                                *)
(* Copyright:   Biosimilarity LLC 2004 - 2006. All rights reserved.     *)
(*              See LICENSE.BIOSIM in the license directory.            *)
(* Description:                                                         *)
(* -------------------------------------------------------------------- *)

open Geometry
open Rho

module Geometrize =
  struct
    type format =
        Stdio of process
        | Fileio of process * string
    type ctornames =
        ZeroCtor
        | InputCtor
        | LiftCtor
        | DropCtor
        | ParCtor
        | QuoteCtor

    type ctorConfigParams = float * float

    module CtorHash 
      =
    struct
      type t = ctornames
      let equal = (==)
      let hash = Hashtbl.hash
    end
    module CtorTable = Hashtbl.Make( CtorHash )

    exception UnexpectedProcessComplexShape

    let _ctorToDimTableInitialized = ref false
    let _cdt = ref (CtorTable.create 6)

    let _ctorToConfigTableInitialized = ref false
    let _cct = ref (CtorTable.create 6)

    let noScalingNoOffset = (1.0,(Geometry.origin ()))

    let aBitOfScalingAndOffset =
      (0.1,
       (Geometry.smult
	   (Geometry.vplus
	       (Geometry.unitx ())
               (Geometry.vplus
		   (Geometry.unity ())
		   (Geometry.vplus
		       (Geometry.unitz ())
		       (Geometry.vplus
			   (Geometry.unityaw ())
			   (Geometry.vplus
			       (Geometry.unitpitch ())
			       (Geometry.unitroll ()))))))
	   0.1))

    let ctorToDimTable =      
      if (!_ctorToDimTableInitialized)
      then !_cdt
      else
        begin
          (CtorTable.add !_cdt ZeroCtor Geometry.Roll);
          (CtorTable.add !_cdt InputCtor Geometry.Y);
          (CtorTable.add !_cdt LiftCtor Geometry.Z);
          (CtorTable.add !_cdt DropCtor Geometry.Yaw);
          (CtorTable.add !_cdt ParCtor Geometry.Pitch);
          (CtorTable.add !_cdt QuoteCtor Geometry.X);
	  _ctorToDimTableInitialized := true;
          !_cdt
        end

    let ctorToConfigTable =      
      if (!_ctorToConfigTableInitialized)
      then !_cct
      else
        begin
          (CtorTable.add !_cct ZeroCtor noScalingNoOffset);
          (CtorTable.add !_cct InputCtor noScalingNoOffset);
          (CtorTable.add !_cct LiftCtor noScalingNoOffset);
          (CtorTable.add !_cct DropCtor noScalingNoOffset);
          (CtorTable.add !_cct ParCtor noScalingNoOffset);
          (CtorTable.add !_cct QuoteCtor noScalingNoOffset);
	  _ctorToConfigTableInitialized := true;
          !_cct
        end

    let useNameScalingAndParOffset () =
      begin
        (CtorTable.add !_cct ZeroCtor noScalingNoOffset);
        (CtorTable.add !_cct InputCtor noScalingNoOffset);
        (CtorTable.add !_cct LiftCtor noScalingNoOffset);
        (CtorTable.add !_cct DropCtor noScalingNoOffset);
        (CtorTable.add !_cct ParCtor
	    (1.0,
	     (Geometry.smult
		 (Geometry.DimTable.find
		     Geometry.dimensionUnitTable
		     (CtorTable.find ctorToDimTable ParCtor))
		 0.1)));
        (CtorTable.add !_cct QuoteCtor (0.1,(Geometry.origin ())));
	_ctorToConfigTableInitialized := true;
        !_cct
      end

    let rec realizeProcess p =
      match p with
          Zero ->
            (* Geometry.Extremum( [(Geometry.origin ())] ) *)
            (Geometry.Solid (Geometry.Sphere (Geometry.origin (),0.1)))
        | Input( Action( nsubj, nobj ), kp ) ->
            let gNSubj = (realizeName nsubj) in	      	    
            let gNObj = (realizeName nobj) in	   
            let gKP = (realizeProcess kp) in
	    let gNSubjBc = (Geometry.barycenter (Geometry.top gNSubj)) in
	    let gNObjBc = (Geometry.barycenter (Geometry.top gNObj)) in
	    let gKPBc = (Geometry.barycenter (Geometry.top gKP)) in
            let v =
              (Geometry.completeComplex
                  (Geometry.Extremum( [gNSubjBc;gNObjBc;gKPBc] ))
                  (CtorTable.find ctorToDimTable InputCtor)) in
	    let ccplx =
              (Geometry.Complex(
		  [(Geometry.Extremum( [gNSubjBc;gNObjBc;gKPBc;v] ));
		   gNSubj;gNObj;gKP]
		)) in
	    let (sf,delta) = (CtorTable.find ctorToConfigTable InputCtor) in
	      (Geometry.translate (Geometry.scale ccplx sf) delta)
        | Lift( nsubj, kp ) ->
            let gNSubj = (realizeName nsubj) in	    
            let gKP = (realizeProcess kp) in
	    let gNSubjBc = (Geometry.barycenter (Geometry.top gNSubj)) in
	    let gKPBc = (Geometry.barycenter (Geometry.top gKP)) in
            let v =
              (Geometry.completeComplex
                  (Geometry.Extremum( [gNSubjBc;gKPBc] ))
                  (CtorTable.find ctorToDimTable LiftCtor)) in
	    let ccplx =
	      (Geometry.Complex(
		[(Geometry.Extremum( [gNSubjBc;gKPBc;v] ));
		 gNSubj;gKP]
	      )) in
	    let (sf,delta) = (CtorTable.find ctorToConfigTable LiftCtor) in
	      (Geometry.translate (Geometry.scale ccplx sf) delta)
        | Drop( n ) ->
            let gN = (realizeName n) in
	    let gNBc = (Geometry.barycenter (Geometry.top gN)) in
            let v =
              (Geometry.completeComplex 
                  (Geometry.Extremum( [gNBc] ))
                  (CtorTable.find ctorToDimTable DropCtor)) in
	    let ccplx =
              (Geometry.Complex(
		[(Geometry.Extremum( [gNBc;v] ));gN]
	      )) in
	    let (sf,delta) = (CtorTable.find ctorToConfigTable DropCtor) in
	      (Geometry.translate (Geometry.scale ccplx sf) delta)
        | Par( procs ) ->
            let gProcs =
	      (List.map realizeProcess procs) in
	    let gProcsBc =
	      (List.map
		  (fun gProc -> (Geometry.barycenter (Geometry.top gProc)))
		  gProcs) in
            let v =
              (Geometry.completeComplex
                  (Geometry.Extremum( gProcsBc ))
                  (CtorTable.find ctorToDimTable ParCtor)) in
	    let ccplx =
              (Geometry.Complex(
		([(Geometry.Extremum( (gProcsBc @ [v]) ))] @ gProcs)
	      )) in
	    let (sf,delta) = (CtorTable.find ctorToConfigTable ParCtor) in
	      (Geometry.translate (Geometry.scale ccplx sf) delta)
    and realizeName n =
      (match n with
          Quote( p ) ->
            let gP = (realizeProcess p) in
	    let gPBc = (Geometry.barycenter (Geometry.top gP)) in
            let v =
              (Geometry.completeComplex
                  (Geometry.Extremum( [gPBc] )) 
                  (CtorTable.find ctorToDimTable QuoteCtor)) in
            let gNShape = (Geometry.Solid (Geometry.Sphere (v,0.1))) in
            let gPNScaledNTxd =
              (Geometry.translate (Geometry.scale gP 0.001) (Geometry.vminus gPBc v)) in 
            let ccplx =
              (Geometry.Complex( [gNShape;gPNScaledNTxd] )) in 
	    let (sf,delta) = (CtorTable.find ctorToConfigTable QuoteCtor) in
	      (Geometry.translate (Geometry.scale ccplx sf) delta))

    let formatProcess f =
      match f with
          Stdio( proc ) ->
            let cplx = (realizeProcess proc) in
              (Geometry.stringOfComplex cplx "")
        | Fileio( proc, fname ) ->
            let rfname = (String.sub fname 1 ((String.length fname)-2)) in
            let cplx = (realizeProcess proc) in
              begin
                (Geometry.jsonOfComplex cplx rfname);
                fname
              end
  end
