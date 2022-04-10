(* ugraph.ml
 * Implements an adjacency matrix representation of weighted graphs.
 *)

open Core

module Graph_AdjMatrix : GRAPH =
  struct

    type vtx = int
    type wt = float
    type t = bool * int * wt option list list

    exception VertexOutOfBounds

    let empty d = (d, 0, [])

    let isdirected (d,_,_) = d

    let insertvertex (d,sz,g) =
      let sz' = sz + 1 in
      let rec augment ll =
        match ll with
          [] -> [List.init sz' ~f:(fun _ -> None)]
        | l::ll' -> (l @ [None]) :: (augment ll') in
      (d, sz', augment g)

    let isvertex (_,sz,_) u = 0 <= u && u < sz

    let insertedge (d,sz,g) u v w =
      if not (isvertex (d,sz,g) u) then raise VertexOutOfBounds else
      if not (isvertex (d,sz,g) v) then raise VertexOutOfBounds else
      let update l i v =
        if i < 0 then raise (Failure "update: index negative") else
        let rec updatemain l i =
          match l with
            [] -> raise (Failure "update: index greater than length of list")
          | x::l' -> if i = 0 then v::l' else x::(updatemain l' (i-1))
        in
        updatemain l i
      in
      let newu = update (List.nth_exn g u) v (Some w) in
      let newv = update (List.nth_exn g v) u (Some w) in
      let dg = update g u newu in
      (d, sz, if d then dg else update dg v newv)

    let size (_,sz,_) = sz

    let weight (d,sz,g) u v =
      if not (isvertex (d,sz,g) u) then raise VertexOutOfBounds else
      if not (isvertex (d,sz,g) v) then raise VertexOutOfBounds else
      List.nth_exn (List.nth_exn g u) v

    let neighborhood (d,sz,g) u =
      if not (isvertex (d,sz,g) u) then raise VertexOutOfBounds else
      List.foldi (List.nth_exn g u) ~init:[] ~f:(fun i l x -> match x with None -> l | Some w -> (i,w)::l)

    let of_list d n es =
      (* initialize a new graph with n vertices and no edges *)
      let g = List.init n ~f:(fun i -> List.init n ~f:(fun _ -> None)) in
      List.fold ~f:(fun g (u,v,w) -> insertedge g u v w) ~init:(d,n,g) es

  end
