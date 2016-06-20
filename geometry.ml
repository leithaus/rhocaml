(* -*- mode: Tuareg;-*-                                                 *)
(* Filename:    barycenter.ml                                           *)
(* Authors:     lgm                                                     *)
(* Creation:    Tue Jan 16 12:40:15 2007                                *)
(* Copyright:   Biosimilarity LLC 2004 - 2006. All rights reserved.     *)
(*              See LICENSE.BIOSIM in the license directory.            *)
(* Description:                                                         *)
(* -------------------------------------------------------------------- *)

open Json_type

module Geometry =
  struct
    type dimnames =
        X
        | Y
        | Z
        | Yaw
        | Pitch
        | Roll
            
    module DimHash 
      =
    struct
      type t = dimnames
      let equal = (==)
      let hash = Hashtbl.hash
    end
    module DimTable = Hashtbl.Make( DimHash )      

    type point =
        {x:float; y:float; z:float; yaw:float; pitch:float; roll:float}

    type platonic =
        Tetrahedron
        | Cube
        | Octahedron
        | Dodecahedron
        | Icosohedron

    type solid =
        ConvexPolyhedra of platonic * point * float
        | Sphere of point * float

    type complex =
        Extremum of point list
        | Solid of solid
        | Complex of complex list

    exception CaseNotCoveredException

    let unitx () =
      {x=1.0;y=0.0;z=0.0;yaw=0.0;pitch=0.0;roll=0.0}
    let unity () =
      {x=0.0;y=1.0;z=0.0;yaw=0.0;pitch=0.0;roll=0.0}
    let unitz () =
      {x=0.0;y=0.0;z=1.0;yaw=0.0;pitch=0.0;roll=0.0}
    let unityaw () =
      {x=0.0;y=0.0;z=0.0;yaw=1.0;pitch=0.0;roll=0.0}
    let unitpitch () =
      {x=0.0;y=0.0;z=0.0;yaw=0.0;pitch=1.0;roll=0.0}
    let unitroll () =
      {x=0.0;y=0.0;z=0.0;yaw=0.0;pitch=0.0;roll=1.0}
    let origin () =
      {x=0.0;y=0.0;z=0.0;yaw=0.0;pitch=0.0;roll=0.0}


    let _dimensionUnitTableInitialized = ref false

    let _dtut = ref (DimTable.create 6)

    let dimensionUnitTable =
      if (!_dimensionUnitTableInitialized)
      then !_dtut
      else
        begin
          (DimTable.add !_dtut X (unitx ()));
          (DimTable.add !_dtut Y (unity ()));
          (DimTable.add !_dtut Z (unitz ()));
          (DimTable.add !_dtut Yaw (unityaw ()));
          (DimTable.add !_dtut Pitch (unitpitch ()));
          (DimTable.add !_dtut Roll (unitroll ()));
          !_dtut
        end

    let smult p s =
      {x=p.x*.s;
       y=p.y*.s;
       z=p.z*.s;
       yaw=p.yaw*.s;
       pitch=p.pitch*.s;
       roll=p.roll*.s}

    let dot p1 p2 =
      ((p1.x*.p2.x)
	+.(p1.y*.p2.y)
	+.(p1.z*.p2.z)
	+.(p1.yaw*.p2.yaw)
	+.(p1.pitch*.p2.pitch)
	+.(p1.roll*.p2.roll))

    let norm p =
      (sqrt (dot p p))
          
    let vplus p1 p2 =
      {x=p1.x+.p2.x;
       y=p1.y+.p2.y;
       z=p1.z+.p2.y;
       yaw=p1.yaw+.p2.yaw;
       pitch=p1.pitch+.p2.pitch;
       roll=p1.roll+.p2.roll}

    let vminus p1 p2 =
      (vplus p1 (smult p2 (-1.0)))

    let rec scale cplx sf =
      (match cplx with
	  Extremum( pts ) ->
	    Extremum( (List.map (fun pt -> (smult pt sf)) pts) )
        | Solid s ->
            (match s with
                ConvexPolyhedra (kind,center,edgeLength) ->
                  (Solid (ConvexPolyhedra (kind,center,(edgeLength *. sf))))
              | Sphere (center,radius) ->
                  (Solid (Sphere (center,(radius *. sf)))))
	| Complex( cplxs ) ->
	    match cplxs with
		[] -> cplx
	      | top :: restCplxs ->
		  Complex( (List.map (fun plx -> (scale plx sf)) cplxs) ))

    let rec translate cplx delta =
      (match cplx with
	  Extremum( pts ) ->
	    Extremum( (List.map (fun pt -> (vplus pt delta)) pts) )
        | Solid s ->
            (match s with
                ConvexPolyhedra (kind,center,edgeLength) ->
                  (Solid (ConvexPolyhedra (kind,(vplus center delta),edgeLength)))
              | Sphere (center,radius) ->
                  (Solid (Sphere ((vplus center delta),radius))))
	| Complex( cplxs ) ->
	    match cplxs with
		[] -> cplx
	      | top :: restCplxs ->
		  Complex( (List.map (fun plx -> (translate plx delta)) cplxs) ))

    let top cplx =
      (match cplx with
	  Extremum( _ ) -> cplx
        | Solid( _ ) -> cplx
	| Complex( cplxs ) ->
	    match cplxs with
		[] -> cplx
	      | top :: _ -> top)

    let rec barycenter ensemble =
      match ensemble with
          Extremum( pts ) ->
            let n = float_of_int (List.length pts) in
            let (x,y,z,yaw,pitch,roll) =
              (List.fold_left
                  (fun (u,v,w,way,hctip,llor) {x=x;y=y;z=z;yaw=yaw;pitch=pitch;roll=roll} ->
                    (u+.x, v+.y, w+.z, way+.yaw, hctip+.pitch, llor+.roll))
                  (0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
                  pts) in
              {x=x/.n; y=y/.n; z=z/.n; yaw=yaw/.n; pitch=pitch/.n; roll=roll/.n}
        | Solid s ->
            (match s with
                ConvexPolyhedra (kind,center,edgeLength) -> center
              | Sphere (center,radius) -> center)
        | Complex( cplxs ) ->
            let centers = 
              (List.map 
                  (fun center -> (barycenter center))
                  cplxs) in
              (barycenter (Extremum( centers )))
    
    let rec completeComplex cplx ctor =
      match cplx with
          Extremum( pts ) ->
            (match pts with		
		[] -> raise CaseNotCoveredException
              | p1 :: [] ->
                  (* ------------------------------------------------------------ *)
                  (* This case merely calculates the pt unit length from p1 in    *)
                  (* the ctor direction.                                          *)
                  (* ------------------------------------------------------------ *)
                  (vplus (DimTable.find dimensionUnitTable ctor) p1)
              | p1 :: p2 :: [] ->
                  (* ------------------------------------------------------------ *)
                  (* The calculation for this case is as follows. Let             *)
                  (* l = ||p2-p1||. We seek the point pt such that ||pt-p2|| = l; *)
                  (* ||pt-p1|| = l; pt lines on the line perpendicular to p2-p1   *)
                  (* in the ctor direction.                                       *)
                  (* ------------------------------------------------------------ *)
		  let c = (DimTable.find dimensionUnitTable ctor) in
		  let d = (vminus p2 p1) in (* vector from p1 to p2               *)
		  let l = (norm d) in       (* length of d                        *)  
		  let t = ((dot (vminus c d) d) /. (dot d d)) in
		                           (* scale factor along d where the      *)
		                           (* perpendicular to d in ctor          *)
		                           (* direction intersects d              *)
		  let s = (l /. (norm (vminus c (smult d t)))) in
		                           (* normalizing scale factor            *)
		    (vplus
			(smult (vminus c (smult d t)) s)
			(smult d t))
              | p1 :: p2 :: p3 :: [] ->
		  (* ------------------------------------------------------------ *)
                  (* The calculation for this case is as follows. Let             *)
                  (* l = average(||p2-p1||,||p3-p2||,||p1-p3||). We seek the      *)
                  (* point pt such that ||pt-pi|| = l, i=1,2,3;                   *)
                  (* pt lines on the line normal to the plane described by p1,p2, *)
                  (* p3 in the ctor direction.                                    *)
                  (* ------------------------------------------------------------ *)
		  let c = (DimTable.find dimensionUnitTable ctor) in
                  let d = (vminus p3 p2) in
		  let e = (vminus p3 p1) in
		  let a1 = (dot c d) in
		  let b1 = (dot d d) in
		  let c1 = (dot e d) in
		  let a2 = (dot c e) in
		  let b2 = c1 in
		  let c2 = (dot e e) in
		  let u = (((b1*.a2) +. (b2*.a1))/.((b1*.c2)+.(b2*.c1))) in
		  let t = ((a1 -. (u*.c1))/.b1) in
		  let l = (((norm d) +. (norm e)) /. (2.0)) in
		  let r = (vplus (smult d t) (smult e u)) in
		  let v = (vminus c r) in
		  let w = (l /. (norm v)) in
		    (vplus (smult v w) r)
              | ptlist ->
		  (* ------------------------------------------------------------ *)
		  (* i have realized that par's will hit this case. The way to    *)
		  (* handle this is to calculate the barycenter of the points,    *)
		  (* then drop a line from that point to the ctor                 *)
		  (* ------------------------------------------------------------ *)
		  let pc = (barycenter (Extremum( ptlist ))) in
		    (vplus (DimTable.find dimensionUnitTable ctor) pc))
	| Solid s ->
            (match s with
                ConvexPolyhedra (kind,center,edgeLength) ->
                  (completeComplex (Extremum( [center] )) ctor)
              | Sphere (center,radius) ->
                  (completeComplex (Extremum( [center] )) ctor))
        | Complex( cplxs ) ->
            let centers =
              (List.map 
                  (fun center -> (barycenter center))
                  cplxs) in
              (completeComplex (Extremum( centers )) ctor)

    let rec stringOfComplex cplx indentation =
      match cplx with
          Extremum( pts ) ->
            (indentation
              ^ "Simplex"
              ^ "("
              ^ (List.fold_left
                    (fun acc pt ->
                      acc
                      ^ "\n"
                      ^ (indentation ^ "   ")
                      ^ (stringOfPoint pt))
                    ""
                    pts)
              ^ "\n"
              ^ indentation
              ^ ")")
        | Solid s ->
            (indentation
              ^ "Solid"
              ^ "("
              ^ (match s with
                  ConvexPolyhedra (kind,center,edgeLength) ->
                    (match kind with
                        Tetrahedron -> "Tetrahedron"
                      | Cube -> "Cube"
                      | Octahedron -> "Octahedron"
                      | Dodecahedron -> "Dodecahedron"
                      | Icosohedron -> "Icosohedron")                      
                      ^ "("
                      ^ (stringOfPoint center)
                      ^ ","
                      ^ (string_of_float edgeLength)
                      ^ ")"
                | Sphere (center,radius) ->
                    "Sphere"
                      ^ "("
                      ^ (stringOfPoint center)
                      ^ ","
                      ^ (string_of_float radius)
                      ^ ")")
              ^ ")"
              ^ "\n")
        | Complex( cplxs ) ->
            (indentation
              ^ "Complex"
              ^ "("
              ^ (List.fold_left
                    (fun acc cplx ->
                      acc
                        ^ "\n"
                        ^ (stringOfComplex cplx (indentation ^ "   ")))
                    ""
                    cplxs)
              ^ "\n"
              ^ indentation
              ^ ")")

    and stringOfPoint pt =
      ("("
        ^ (string_of_float pt.x)
        ^ ","
        ^ (string_of_float pt.y)
        ^ ","
        ^ (string_of_float pt.z)
        ^ ","
        ^ (string_of_float pt.yaw)
        ^ ","
        ^ (string_of_float pt.pitch)
        ^ ","
        ^ (string_of_float pt.roll)
        ^ ")")
        
    let jsonOfPt pt = 
      Object [ "x", Float pt.x;
               "y", Float pt.y;
               "z", Float pt.z;
               "yaw", Float pt.yaw;
               "pitch", Float pt.pitch;
               "roll", Float pt.roll ]

    let jsonOfSolid s =
      (Object
        (match s with
            ConvexPolyhedra (kind,center,edgeLength) ->            
              (match kind with
                  Tetrahedron ->
                    ["Kind", String "Tetrahedron";
                     "Center", (jsonOfPt center);
                     "Edge", Float edgeLength]
                | Cube -> 
                    ["Kind", String "Cube";
                     "Center", (jsonOfPt center);
                     "Edge", Float edgeLength]
                | Octahedron -> 
                    ["Kind", String "Octahedron";
                     "Center", (jsonOfPt center);
                     "Edge", Float edgeLength]
                | Dodecahedron -> 
                    ["Kind", String "Dodecahedron";
                     "Center", (jsonOfPt center);
                     "Edge", Float edgeLength]
                | Icosohedron ->
                    ["Kind", String "Icosohedron";
                     "Center", (jsonOfPt center);
                     "Edge", Float edgeLength])
        | Sphere (center,radius) ->
            ["Kind", String "Sphere";
             "Center", (jsonOfPt center);
             "Radius", Float radius]))

    let jsonOfComplex cplx fn = 
      let rec jsonOfCplx cplx =
        (match cplx with
            Extremum( pts ) ->
              Object
                [ "Simplex", Array (List.map jsonOfPt pts) ]
          | Solid s -> Object ["Solid", (jsonOfSolid s)]
          | Complex( cplxs ) ->
              let jplxs = 
                (List.map
                    (fun ccplx -> (jsonOfCplx ccplx))
                    cplxs) in
                Object [ "Complex", Array (jplxs :> Json_type.json_type list) ]) in
      let rslt = Json_io.save_json fn (jsonOfCplx cplx) in
        rslt
  end
