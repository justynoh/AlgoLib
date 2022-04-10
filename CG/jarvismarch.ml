(* jarvismarch.ml
 * Implements the Jarvis march convex hull algorithm.
 *)

open Core

module JarvisMarch : CONVEXHULL =
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
     * Implements the Jarvis march convex hull algorithm. It is an O(n^2)
     * algorithm that sweeps through the input points to find the appropriate
     * next vertex to add to the hull at each step. It is output-sensitive, and
     * is more precisely O(hn) where h is the size of the hull.
     *)
    let ch p =
      if List.length p < 3 then raise LessThanThreePoints;
      (* First, find the ltl point to start the hull construction. *)
      let ltlbundle = getltl p in
      let (ltlidx,_) = ltlbundle in
      (* On one iteration of the loop, we go from our last vertex in the hull
       * and find the best next vertex using left-side tests. *)
      let rec loop (kidx,k) partialhull =
        let bestbundle = List.foldi p ~init:None ~f:(fun sidx best s ->
          if sidx = kidx then best else (* Ignore testing the current vertex itself, obviously. *)
          match best with
            None -> Some (sidx,s)
          | Some (bestsidx,bests) -> if not (leftside (k,bests) s) then Some (sidx,s) else best
          ) in
        (* Now update our input variables. *)
        let partialhull = k::partialhull in (* store partial hull in reverse order, for efficiency. *)
        let (kidx,k) =
          match bestbundle with
            None -> raise (Failure "ch: No best next vertex found.")
          | Some bundle -> bundle in
        (* Terminate once we get back to the ltl point. *)
        if kidx = ltlidx then partialhull else loop (kidx,k) partialhull
      in
      loop ltlbundle [] |> List.rev

  end
