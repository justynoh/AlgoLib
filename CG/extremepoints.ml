(* extremepoints.ml
 * Implements the extreme points convex hull algorithm.
 *)

open Core

module ExtremePoints : CONVEXHULL =
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

    (* intriangle (p,q,r) s
     * Returns true if s lies inside the triangle defined by (p,q,r).
     *)
    let intriangle (p,q,r) s =
      let b1 = leftside (p,q) s in
      let b2 = leftside (q,r) s in
      let b3 = leftside (r,p) s in
      Bool.(b1 = b2 && b2 = b3)

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
     * Implements the extreme points convex hull algorithm. It is an O(n^4)
     * algorithm, thus it is not very efficient.
     *)
    let ch p =
      let n = List.length p in
      if n < 3 then raise LessThanThreePoints;
      (* Presume every vertex is extreme unless proven otherwise. *)
      let temp = Array.init n ~f:(fun _ -> true) in
      (* Go through every triangle. *)
      List.iteri p ~f:(fun i p1 ->
        List.iteri p ~f:(fun j p2 ->
          if i < j then
          List.iteri p ~f:(fun k p3 ->
            if j < k then
            (* Test every other point to see if it is in the triangle. *)
            List.iteri p ~f:(fun l q ->
              if i <> l && j <> l && k <> l && intriangle (p1,p2,p3) q then temp.(l) <- false;
              )
            )
          )
        );
      (* Only those that survive all tests are extreme. *)
      let extreme = List.filteri p ~f:(fun i _ -> temp.(i)) in
      (* Find the ltl point and sort by polar angle with respect to it. *)
      let (ltlidx,ltl) = getltl extreme in
      ltl::(List.sort (listdelete extreme ltlidx) ~compare:(ccwcompare ltl))

  end
