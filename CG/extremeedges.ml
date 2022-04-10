(* extremeedges.ml
 * Implements the extreme edges convex hull algorithm.
 *)

open Core

module ExtremeEdges : CONVEXHULL =
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
     * Implements the extreme edges convex hull algorithm. It is an O(n^3)
     * algorithm, thus it is not very efficient.
     *)
    let ch p =
      let n = List.length p in
      if n < 3 then raise LessThanThreePoints;
      (* Go through every edge, keeping track of extreme edges as we go along. *)
      let extremeedges =
      List.foldi p ~init:[] ~f:(fun i ees p1 ->
        List.foldi p ~init:ees ~f:(fun j ees p2 ->
          if j <= i then ees else
          (* Conduct left-side tests for every point other than p1 and p2 itself. *)
          let (lempty,rempty) = List.foldi p ~init:(true,true) ~f:(fun k (lempty,rempty) q ->
            if i = k || j = k then (lempty,rempty) else
            if leftside (p1,p2) q then (false,rempty) else (lempty,false)) in
          if rempty then (p1,p2)::ees else if lempty then (p2,p1)::ees else ees
          )
        ) in
      (* Since the extreme edges form a cycle, we can just take the first element
       * of each edge. *)
      let (extreme,_) = List.unzip extremeedges in
      (* Find the ltl point and sort by polar angle with respect to it. *)
      let (ltlidx,ltl) = getltl extreme in
      ltl::(List.sort (listdelete extreme ltlidx) ~compare:(ccwcompare ltl))

  end
