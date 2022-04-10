(* Incremental.ml
 * Implements the incremental construction convex hull algorithm.
 *)

open Core

module Incremental : CONVEXHULL =
  struct

    exception LessThanThreePoints

    type input = (float * float) list
    type output = (float * float) list

    (* leftside (p,q) r
     * Returns true if r lies to the left of the directed line defined by (p,q).
     *)
    let leftside (p,q) r =
      let (px,py) = p in
      let (qx,qy) = q in
      let (rx,ry) = r in
      Float.((qx - px) * (ry - py) - (qy - py) * (rx - px) > 0.)

    (* getltl l
     * Outputs (i,p) where i is the index of the ltl point p in l.
     *)
    let getltl l =
      Option.value_exn (List.foldi l ~init:None ~f:(fun i accum (px,py) ->
        match accum with
          None -> Some (i,(px,py))
        | Some (besti,(bestpx,bestpy)) ->
          if Float.(bestpy < py) then accum else
          if Float.(bestpy > py) then Some (i,(px,py)) else
          if Float.(bestpx < px) then accum else Some (i,(px,py))
        ))

    (* ch p
     * Implements the incremental construction convex hull algorithm. It is an
     * O(n^2) algorithm using the decrease-and-conquer method..
     *)
    let ch p =
      (* Take the first three points and create the initial triangular hull. *)
      let currhull =
        match p with
          p0::p1::p2::_ -> if leftside (p0,p1) p2 then [p0;p1;p2] else [p0;p2;p1]
        | _ -> raise LessThanThreePoints in
      let finalhull = List.foldi p ~init:currhull ~f:(fun i currhull newp ->
        if i <= 2 then currhull else (* Ignore the first three points. *)
        (* Now this is where the real work starts. We compute the two-bit
         * pattern for every vertex first. *)
        let hullsize = List.length currhull - 1 in
        let pred = List.drop currhull hullsize @ List.take currhull hullsize in
        let succ = List.drop currhull 1 @ List.take currhull 1 in
        let pattern = List.(zip_exn currhull (zip_exn pred succ) |> map ~f:(fun (v,(vp,vs)) -> (v,(leftside (newp,v) vp, leftside (newp,v) vs)))) in
        (* Using the pattern, we can determine the type of each vertex. If all
         * are RL, then the new point is interior and we do not add it. *)
        if List.for_all pattern ~f:(fun (_,pat) -> match pat with (false,true) -> true | _ -> false) then currhull else
        (* We remove the vertices with the pattern LR (true,false), and replace
         * that segment with the new point (before the (true,true) vertex). *)
        let removedwpat = List.filter pattern ~f:(fun (v,pat) -> match pat with (true,false) -> false | _ -> true) in
        let addbeforeidx =
          match List.findi removedwpat ~f:(fun _ (_,pat) -> match pat with (true,true) -> true | _ -> false) with
            None -> raise (Failure "ch: No new point successor vertex found.")
          | Some (idx,_) -> idx
        in
        let removed = List.map removedwpat ~f:(fun (v,_) -> v) in
        List.take removed addbeforeidx @ [newp] @ List.drop removed addbeforeidx
        ) in
      (* Find the ltl point and rotate the output list. *)
      let (ltlidx,_) = getltl finalhull in
      List.drop finalhull ltlidx @ List.take finalhull ltlidx

  end
