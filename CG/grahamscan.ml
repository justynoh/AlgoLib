(* grahamscan.ml
 * Implements the graham scan convex hull algorithm.
 *)

open Core

module GrahamScan : CONVEXHULL =
  struct

    exception LessThanThreePoints

    type input = (float * float) list
    type output = (float * float) list

    (* listdelete l i
     * For a given list l = [l0, l1, l2, ..., ln-1], and 0 <= i < n, returns
     * [l0, l1, ..., li-1, li+1, ..., ln-1].
     *)
    let rec listdelete l i =
      let oob = Failure "listextract: index out of bounds." in
      if i < 0 then raise oob else
      match l with
        [] -> raise oob
      | x::xs -> if i = 0 then xs else x::(listdelete xs (i-1))

    (* leftside (p,q) r
     * Returns true if r lies to the left of the directed line defined by (p,q).
     *)
    let leftside (p,q) r =
      let (px,py) = p in
      let (qx,qy) = q in
      let (rx,ry) = r in
      Float.((qx - px) * (ry - py) - (qy - py) * (rx - px) > 0.)

    (* ccwcompare o p q
     * Is a comparison function for the pair (p,q) with respect to an origin o.
     *)
    let ccwcompare o p q =
      (* p < q if q lies to the left of (o,p) *)
      if leftside (o,p) q then -1 else if leftside (o,q) p then 1 else 0

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
     * Implements the Graham scan convex hull algorithm. It is an O(n log n)
     * algorithm. The basis of the algorithm assumes each point in turn is part
     * of the hull, until a right turn is met, at which point we backtrack and
     * delete the vertex from consideration.
     *)
    let ch p =
      if List.length p < 3 then raise LessThanThreePoints;
      (* First, find the ltl point and sort the remainder of the points by
       * polar angle with respect to it. *)
      let (ltlidx,ltl) = getltl p in
      let rempts = List.sort (listdelete p ltlidx) ~compare:(ccwcompare ltl) in
      let (s,t) =
        match rempts with
          [] -> raise (Failure "ch: no points found after sorting.")
        | p2::ps -> ([p2;ltl],ps) in
      (* On one iteration of the loop, we go from our last vertex in the hull
       * and find the best next vertex using left-side tests. *)
      let rec loop s t =
        (* Terminate once T is empty *)
        match t with
          [] -> s
        | t0::t1 -> (
          match s with
            s0::s1::s2 -> if leftside (s1,s0) t0 then loop (t0::s) t1 else loop (s1::s2) t
          | _ -> raise (Failure "ch: stack invariant violated.")
          )
      in
      loop s t |> List.rev

  end
